# The two ways the OpenAlex adapter finds works that cite a dataset DOI:
#
# - text: an exact full-text search for the DOI string, which finds DOIs
#   mentioned anywhere in a work (methods, data availability statements,
#   acknowledgements) whether or not OpenAlex indexes the dataset itself.
# - graph: OpenAlex's citation graph, which finds works whose parsed reference
#   lists resolve to the dataset, for datasets that OpenAlex indexes.
#
# Both return a list with `pairs` (a data.frame with one row per dataset DOI
# and citing work) and, per input batch, whether it `completed` or `failed`.

openalex_work_fields <- "id,doi,title,type,publication_year,primary_location"

# Characters that would break OpenAlex's filter syntax; DOIs containing them
# are left out of text searches.
openalex_unsearchable <- function(dois) grepl("[\",|[:space:]]", dois)

# Split DOIs into batches of at most `size`, also keeping each batch's search
# filter short enough for a URL.
chunk_for_search <- function(dois, size, max_chars = 3000) {
  batches <- list()
  current <- character()
  chars <- 0
  for (d in dois) {
    len <- nchar(utils::URLencode(d, reserved = TRUE)) + 9
    if (length(current) >= size || (length(current) > 0 && chars + len > max_chars)) {
      batches[[length(batches) + 1]] <- current
      current <- character()
      chars <- 0
    }
    current <- c(current, d)
    chars <- chars + len
  }
  if (length(current)) batches[[length(batches) + 1]] <- current
  batches
}

openalex_since_filter <- function(since) {
  if (is.null(since)) "" else paste0(",from_publication_date:", since)
}

openalex_id <- function(x) sub("^https://openalex.org/", "", x %||% NA_character_)
openalex_doi <- function(x) tolower(sub("^https://doi.org/", "", x %||% NA_character_))

# Flatten OpenAlex work records into a data.frame of the fields the adapter and
# its review logs use, one row per work.
works_to_df <- function(works) {
  if (length(works) == 0) {
    return(data.frame(work_id = character(), work_doi = character(), title = character(),
                      type = character(), year = integer(), landing_page = character(),
                      venue = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    work_id = vapply(works, function(w) openalex_id(w$id), ""),
    work_doi = vapply(works, function(w) openalex_doi(w$doi), ""),
    title = vapply(works, function(w) w$title %||% NA_character_, ""),
    type = vapply(works, function(w) w$type %||% NA_character_, ""),
    year = vapply(works, function(w) as.integer(w$publication_year %||% NA), 1L),
    landing_page = vapply(works, function(w) w$primary_location$landing_page_url %||% NA_character_, ""),
    venue = vapply(works, function(w) w$primary_location$source$display_name %||% NA_character_, ""),
    stringsAsFactors = FALSE
  )
}

empty_pairs <- function() cbind(dataset_doi = character(), works_to_df(list()), method = character())

#' Find works that mention DOIs in their full text
#'
#' Each batch of DOIs is first screened with one search that ORs all of them
#' together; most batches have no matches. A batch with matches is split in
#' half and each half searched again, down to single DOIs, so the works found
#' can be attributed to the DOI they mention (adaptive group testing). Only the
#' single-DOI searches fetch the works themselves; the others just count them.
#' @noRd
openalex_text_search <- function(client, batches, since = NULL) {
  # "running" until every search for the batch has finished, then "done";
  # "incomplete" if the client stopped first, "failed" if a search failed.
  state <- rep("running", length(batches))
  pairs <- list()
  groups <- lapply(seq_along(batches), function(b) list(batch = b, dois = batches[[b]]))

  while (length(groups) > 0) {
    leaf <- lengths(lapply(groups, `[[`, "dois")) == 1
    queries <- lapply(seq_along(groups), function(g) {
      dois <- groups[[g]]$dois
      list(
        filter = paste0("fulltext.search.exact:", paste0("\"", dois, "\"", collapse = "|"),
                        openalex_since_filter(since)),
        select = if (leaf[g]) openalex_work_fields else "id",
        `per-page` = if (leaf[g]) "200" else "1"
      )
    })
    run <- openalex_run(client, queries, paginate = leaf)

    next_groups <- list()
    for (g in seq_along(groups)) {
      batch <- groups[[g]]$batch
      dois <- groups[[g]]$dois
      if (run$status[g] == "failed") {
        state[batch] <- "failed"
      } else if (run$status[g] == "pending") {
        if (state[batch] == "running") state[batch] <- "incomplete"
      } else if (run$count[g] > 0 && leaf[g]) {
        works <- works_to_df(run$results[[g]])
        if (nrow(works)) {
          pairs[[length(pairs) + 1]] <- cbind(dataset_doi = dois, works, method = "text",
                                              stringsAsFactors = FALSE)
        }
      } else if (run$count[g] > 0) {
        half <- ceiling(length(dois) / 2)
        next_groups <- c(next_groups, list(
          list(batch = batch, dois = dois[seq_len(half)]),
          list(batch = batch, dois = dois[-seq_len(half)])
        ))
      }
    }
    groups <- Filter(function(g) state[g$batch] == "running", next_groups)
    if (!is.null(client$stopped)) {
      state[unique(vapply(groups, `[[`, 1, "batch"))] <- "incomplete"
      groups <- list()
    }
  }
  state[state == "running"] <- "done"

  list(pairs = if (length(pairs)) do.call(rbind, pairs) else empty_pairs(),
       completed = state == "done", failed = state == "failed")
}

#' Find works that cite DOIs through OpenAlex's citation graph
#'
#' Resolves each batch of DOIs to OpenAlex work IDs (DOIs that OpenAlex does
#' not index are returned in `unresolved`), then fetches the works citing any
#' of them. Each citing work's `referenced_works` shows which of the batch's
#' datasets it cites.
#' @noRd
openalex_graph_search <- function(client, batches, since = NULL) {
  completed <- rep(FALSE, length(batches))
  failed <- rep(FALSE, length(batches))
  pairs <- list()
  unresolved <- character()

  resolve <- openalex_run(client, lapply(batches, function(dois) {
    list(filter = paste0("doi:", paste(dois, collapse = "|")),
         select = "id,doi", `per-page` = "200")
  }), paginate = FALSE)
  failed[resolve$status == "failed"] <- TRUE

  ids <- vector("list", length(batches))
  for (b in which(resolve$status == "done")) {
    found <- works_to_df(resolve$results[[b]])
    ids[[b]] <- stats::setNames(found$work_doi, found$work_id)
    unresolved <- c(unresolved, setdiff(batches[[b]], found$work_doi))
    if (nrow(found) == 0) completed[b] <- TRUE
  }

  todo <- which(resolve$status == "done" & lengths(ids) > 0)
  if (length(todo) && is.null(client$stopped)) {
    citing <- openalex_run(client, lapply(todo, function(b) {
      list(filter = paste0("cites:", paste(names(ids[[b]]), collapse = "|"),
                           openalex_since_filter(since)),
           select = paste0(openalex_work_fields, ",referenced_works"), `per-page` = "200")
    }), paginate = TRUE)
    for (k in seq_along(todo)) {
      b <- todo[k]
      if (citing$status[k] == "failed") failed[b] <- TRUE
      if (citing$status[k] != "done") next
      completed[b] <- TRUE
      for (w in citing$results[[k]]) {
        cited <- intersect(openalex_id(unlist(w$referenced_works)), names(ids[[b]]))
        if (length(cited)) {
          pairs[[length(pairs) + 1]] <- cbind(dataset_doi = unname(ids[[b]][cited]),
                                              works_to_df(list(w)), method = "graph",
                                              stringsAsFactors = FALSE)
        }
      }
    }
  }

  list(pairs = if (length(pairs)) do.call(rbind, pairs) else empty_pairs(),
       completed = completed & !failed, failed = failed, unresolved = unresolved)
}
