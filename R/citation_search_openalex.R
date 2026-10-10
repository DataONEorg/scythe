#' Search for citations in OpenAlex
#'
#' This function searches [OpenAlex](https://openalex.org) for works that cite
#' or mention each dataset DOI, in two ways:
#'
#' - `"text"`: an exact full-text search for the DOI, which finds DOIs
#'   mentioned anywhere in a work's text (methods, data availability statements,
#'   acknowledgements, reference lists), whether or not OpenAlex indexes the
#'   dataset. DOIs are searched in batches, and batches with matches are split
#'   until each match is attributed to a single DOI.
#' - `"graph"`: OpenAlex's citation graph, which finds works whose reference
#'   lists resolve to the dataset, for datasets that OpenAlex indexes.
#'
#' The result only includes citing works that have a DOI and are not datasets
#' themselves. Everything else is written as JSON Lines files to `log_dir` for
#' review: `openalex-matches.jsonl` (every article match and which method found
#' it), `openalex-data-relations.jsonl` (datasets citing or mentioning other
#' datasets, which [classify_data_relations()] can label as version,
#' derivation, composition or citation links afterwards; that isn't needed for
#' the results), `openalex-no-doi-citers.jsonl` (citing works
#' without a DOI), `openalex-unresolved.jsonl` (identifiers that are not DOIs,
#' or that OpenAlex does not index) and `openalex-failed-batches.jsonl`.
#' The logs are appended to, and a run that resumes from `checkpoint_dir` only
#' logs what it found itself, so that a search run in installments logs each
#' match once.
#'
#' OpenAlex charges credits for each request against a daily budget: 1 for a
#' list query and 10 for a full-text search. Set an API key with
#' `scythe_set_key("openalex", ...)` for a larger budget than the 1,000 daily
#' credits available without one, and optionally a contact email as
#' `openalex_mailto`. Before sending any request, the function estimates the
#' credits the search will use and stops if that exceeds `max_credits`; set
#' `max_credits = Inf` to approve a large search. While running, it never
#' spends more than `max_credits`, and stops sending requests as soon as
#' OpenAlex reports a rate limit, returning the results found so far (see the
#' `openalex_status` attribute of the result).
#'
#' @param identifiers a vector of identifiers to be searched for
#' @param methods one or both of "text" and "graph"
#' @param since only find works published on or after this date
#'   (a Date or "YYYY-MM-DD"), or NULL to search all works
#' @param log_dir directory for the review logs; defaults to the
#'   `scythe.log_dir` option, or a temporary directory
#' @param checkpoint_dir directory where completed batches are recorded, so
#'   that a rerun with the same directory continues where an interrupted run
#'   stopped; NULL to disable checkpointing
#' @param state_file file recording when OpenAlex rate limits end, shared by
#'   every process using the same API key so none of them send requests before
#'   then; defaults to `openalex-state.json` in `checkpoint_dir`
#' @param max_credits the most OpenAlex credits to spend; defaults to the
#'   `scythe.openalex.max_credits` option, or 500
#' @param preflight if TRUE, stop before sending any request when the
#'   estimated cost exceeds `max_credits`; if FALSE, run until `max_credits`
#'   have been spent
#' @param batch_size the number of DOIs in each OpenAlex query (at most 100)
#'
#' @return data.frame of matching dataset and publication identifiers, with an
#'   `openalex_status` attribute recording whether the search completed, why
#'   it stopped if not, and the credits used
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.5063/F1T151VR")
#' result <- citation_search_openalex(identifiers)
#' attr(result, "openalex_status")
#' }
citation_search_openalex <- function(identifiers,
                                     methods = c("text", "graph"),
                                     since = NULL,
                                     log_dir = NULL,
                                     checkpoint_dir = NULL,
                                     state_file = NULL,
                                     max_credits = getOption("scythe.openalex.max_credits", 500),
                                     preflight = TRUE,
                                     batch_size = 100) {
  methods <- match.arg(methods, several.ok = TRUE)
  batch_size <- min(batch_size, 100)
  if (!is.null(since)) since <- format(as.Date(since))
  if (is.null(state_file) && !is.null(checkpoint_dir)) {
    state_file <- file.path(checkpoint_dir, "openalex-state.json")
  }
  log_dir <- openalex_log_dir(log_dir)
  retrieved_at <- format_utc(openalex_now())

  openalex_results <- data.frame(
    article_id = character(),
    article_title = character(),
    dataset_id = character(),
    source = character()
  )
  status <- list(complete = FALSE, stopped = NULL, paused_until = NULL,
                 credits_used = 0, requests = 0, estimate = 0, log_dir = log_dir)

  identifiers <- check_identifiers(identifiers)
  dois_in <- normalize_doi(identifiers)
  write_jsonl(log_rows(identifier = identifiers[is.na(dois_in)], reason = "not a DOI",
                         retrieved_at = retrieved_at), file.path(log_dir, "openalex-unresolved.jsonl"))
  dois <- unique(dois_in[!is.na(dois_in)])
  if (length(dois) == 0) {
    status$complete <- TRUE
    attr(openalex_results, "openalex_status") <- status
    return(openalex_results)
  }

  status$estimate <- estimate_openalex_cost(length(dois), methods, batch_size)
  message("OpenAlex: searching ", length(dois), " DOIs (", paste(methods, collapse = " and "),
          "); estimated cost about ", status$estimate, " credits")
  if (preflight && status$estimate > max_credits) {
    stop(call. = FALSE,
         "The estimated cost of ", status$estimate, " OpenAlex credits exceeds max_credits = ",
         max_credits, ". To approve this search, rerun with a larger max_credits ",
         "(or max_credits = Inf).")
  }

  client <- openalex_client(max_credits = max_credits, state_file = state_file)
  if (is.na(client$key)) {
    message(
            "No OpenAlex API key is set, so the daily budget is 1,000 credits. ",
            "Set one with scythe_set_key(\"openalex\", ...).")
  }

  checkpoint <- read_checkpoint(checkpoint_dir)
  # Pairs from batches completed by an earlier run are returned again, but
  # were already logged and classified by that run.
  found <- list()
  found_now <- list()
  incomplete <- FALSE
  for (method in methods) {
    if (method == "text") {
      unsearchable <- openalex_unsearchable(dois)
      write_jsonl(log_rows(identifier = dois[unsearchable],
                             reason = "characters not supported by OpenAlex search",
                             retrieved_at = retrieved_at),
                  file.path(log_dir, "openalex-unresolved.jsonl"))
      batches <- chunk_for_search(dois[!unsearchable], batch_size)
    } else {
      batches <- chunk(dois, batch_size)
    }
    keys <- vapply(batches, function(b) {
      paste(method, since %||% "all", paste(b, collapse = "|"))
    }, "")
    cached <- keys %in% names(checkpoint)
    found <- c(found, unname(checkpoint[keys[cached]]))
    if (all(cached)) next

    run <- if (method == "text") {
      openalex_text_search(client, batches[!cached], since)
    } else {
      openalex_graph_search(client, batches[!cached], since)
    }
    found <- c(found, list(run$pairs))
    found_now <- c(found_now, list(run$pairs))

    for (b in which(run$completed)) {
      batch_pairs <- run$pairs[run$pairs$dataset_doi %in% batches[!cached][[b]], , drop = FALSE]
      write_checkpoint(checkpoint_dir, keys[!cached][b], batch_pairs)
    }
    if (any(run$failed)) {
      write_jsonl(log_rows(dois = vapply(batches[!cached][run$failed], paste, "", collapse = "|"),
                          method = method, since = since %||% NA_character_,
                          retrieved_at = retrieved_at),
                  file.path(log_dir, "openalex-failed-batches.jsonl"))
    }
    if (length(run$unresolved)) {
      write_jsonl(log_rows(identifier = run$unresolved, reason = "not indexed in OpenAlex",
                             retrieved_at = retrieved_at),
                  file.path(log_dir, "openalex-unresolved.jsonl"))
    }
    if (!all(run$completed)) incomplete <- TRUE
  }

  pairs <- do.call(rbind, c(list(empty_pairs()), found))
  pairs <- pairs[is.na(pairs$work_doi) | pairs$work_doi != pairs$dataset_doi, , drop = FALSE]

  # One row per dataset and citing work, recording which methods found it.
  key <- paste(pairs$dataset_doi, pairs$work_id)
  methods_found <- tapply(pairs$method, key, function(m) {
    if (length(unique(m)) > 1) "both" else m[1]
  })
  pairs <- pairs[!duplicated(key), , drop = FALSE]
  pairs$match_method <- as.character(methods_found[paste(pairs$dataset_doi, pairs$work_id)])
  pairs$method <- NULL
  pairs$retrieved_at <- rep(retrieved_at, nrow(pairs))
  bin <- citer_bin(pairs)

  # Log only the pairs this run found. A pair found by one method in an
  # earlier run and by the other now is logged again, as "both".
  pairs_now <- do.call(rbind, c(list(empty_pairs()), found_now))
  new <- paste(pairs$dataset_doi, pairs$work_id) %in%
    paste(pairs_now$dataset_doi, pairs_now$work_id)
  articles <- pairs[bin == "article", , drop = FALSE]
  write_jsonl(pairs[new & bin == "article", , drop = FALSE],
              file.path(log_dir, "openalex-matches.jsonl"))
  write_jsonl(pairs[new & bin == "data_relation", , drop = FALSE],
              file.path(log_dir, "openalex-data-relations.jsonl"))
  write_jsonl(pairs[new & bin == "no_doi", , drop = FALSE],
              file.path(log_dir, "openalex-no-doi-citers.jsonl"))

  # Report each match under every form of the dataset identifier the caller
  # passed in that normalizes to the same DOI.
  rows <- lapply(seq_len(nrow(articles)), function(i) {
    ids <- unique(identifiers[!is.na(dois_in) & dois_in == articles$dataset_doi[i]])
    data.frame(article_id = articles$work_doi[i], article_title = articles$title[i],
               dataset_id = ids, source = "openalex", stringsAsFactors = FALSE)
  })
  openalex_results <- do.call(rbind, c(list(openalex_results), rows))

  status$complete <- !incomplete && is.null(client$stopped)
  status$stopped <- client$stopped
  status$paused_until <- if (!is.null(client$paused_until)) format_utc(client$paused_until)
  status$credits_used <- client$credits_used
  status$requests <- client$requests
  message("OpenAlex: found ", nrow(articles), " article citations, ",
          sum(bin == "data_relation"), " data relations and ", sum(bin == "no_doi"),
          " citing works without a DOI, using ", client$credits_used, " credits in ",
          client$requests, " requests. Review logs are in ", log_dir)
  attr(openalex_results, "openalex_status") <- status
  openalex_results
}
