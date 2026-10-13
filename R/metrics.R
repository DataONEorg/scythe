# Comparing citations with those already recorded in DataONE's metrics
# service, so that only new ones are submitted to it.

# The metrics service's production address. metrics.dataone.org is the same
# server, but its TLS certificate doesn't include that name yet
# (DataONEorg/metrics-service#103); set options(scythe.metrics_url = ...)
# to switch once it does.
metrics_url_default <- "https://logproc-stage-ucsb-1.test.dataone.org/metrics"

default_metrics_url <- function() getOption("scythe.metrics_url", metrics_url_default)

#' Get the citations recorded in DataONE's metrics service
#'
#' Fetches the citations that DataONE's metrics service has recorded for the
#' datasets of each repository (member node), so that they can be compared
#' with newly found ones (see [citations_not_in_metrics()]).
#'
#' The service records each citation's target by its DataONE identifier (a
#' PID or SID), which may not be a DOI (for example `urn:uuid:...`); DOI
#' targets are mostly recorded as a bare DOI. Use
#' [citations_not_in_metrics()] to compare.
#'
#' @param nodes member node identifiers, such as "urn:node:ARCTIC"
#' @param from,to the range of months to fetch (Dates)
#' @param metrics_url the metrics service's address; defaults to the
#'   `scythe.metrics_url` option
#' @return data.frame with one row per citing work and dataset: `node`,
#'   `source_id` (the citing work's DOI), `target_id` (the dataset's
#'   identifier), and the citing work's `source_url`, `title`, `journal`,
#'   `publisher`, `year_of_publishing` and `link_publication_date`
#' @export
#' @examples
#' \dontrun{
#' recorded <- get_metrics_citations(c("urn:node:ARCTIC", "urn:node:KNB"))
#' }
get_metrics_citations <- function(nodes = "urn:node:ARCTIC", from = as.Date("2000-01-01"),
                                  to = Sys.Date(), metrics_url = NULL) {
  metrics_url <- metrics_url %||% default_metrics_url()
  rows <- lapply(nodes, function(node) {
    query <- list(
      metricsPage = list(total = 0, start = 0, count = 0),
      metrics = list("citations"),
      filterBy = list(
        list(filterType = "repository", values = list(node), interpretAs = "list"),
        list(filterType = "month", values = list(format(as.Date(from), "%m/%d/%Y"),
                                                 format(as.Date(to), "%m/%d/%Y")),
             interpretAs = "range")
      ),
      groupBy = list("month")
    )
    body <- metrics_request(metrics_url, jsonlite::toJSON(query, auto_unbox = TRUE))
    metrics_citations_df(body$resultDetails$citations, node)
  })
  do.call(rbind, c(list(metrics_citations_df(list(), character())), rows))
}

metrics_fields <- c("source_url", "title", "journal", "publisher", "year_of_publishing",
                    "link_publication_date")

# One row per citing work and dataset from the service's `citations` object
# (keyed by citing work, each listing its datasets in `target_id`).
metrics_citations_df <- function(citations, node) {
  rows <- lapply(citations, function(c) {
    targets <- as.character(unlist(c$target_id))
    if (length(targets) == 0) return(NULL)
    fields <- lapply(metrics_fields, function(f) {
      v <- as.character(unlist(c[[f]]))[1]
      rep(if (length(v) && !is.na(v)) v else NA_character_, length(targets))
    })
    data.frame(node = node, source_id = as.character(c$source_id)[1], target_id = targets,
               stats::setNames(fields, metrics_fields), stringsAsFactors = FALSE)
  })
  empty <- data.frame(node = character(), source_id = character(), target_id = character(),
                      stats::setNames(rep(list(character()), length(metrics_fields)), metrics_fields),
                      stringsAsFactors = FALSE)
  do.call(rbind, c(list(empty), rows))
}

# Send a query to the metrics service, retrying server and network errors.
metrics_request <- function(metrics_url, query) {
  url <- paste0(metrics_url, "?q=", utils::URLencode(as.character(query), reserved = TRUE))
  for (attempt in 1:4) {
    res <- http_get(url)[[1]]
    if (isTRUE(res$status == 200)) return(jsonlite::fromJSON(res$body, simplifyVector = FALSE))
    if (!(is.na(res$status) || res$status >= 500) || attempt == 4) break
    http_sleep(2^attempt)
  }
  stop("The metrics service query failed (", describe_response(res), ")", call. = FALSE)
}

#' Keep only citations not yet in the metrics service
#'
#' Compares citations found by a search with those from
#' [get_metrics_citations()], by citing work and target identifier.
#' Identifiers that are DOIs are compared as DOIs, so different
#' serializations (`doi:10...`, `https://doi.org/10...`, a bare `10...`) and
#' letter case don't count as different; other identifiers must match
#' exactly. The target is `found$target_id` (the identifier in DataONE) if
#' present, or else `found$dataset_id`.
#'
#' @param found citations found by a search, with `article_id` and
#'   `target_id` or `dataset_id` columns
#' @param recorded citations from [get_metrics_citations()]
#' @return the rows of `found` whose pair of citing work and target isn't in
#'   `recorded`
#' @export
citations_not_in_metrics <- function(found, recorded) {
  target <- found$target_id %||% found$dataset_id
  known <- paste(identifier_key(recorded$source_id), identifier_key(recorded$target_id))
  found[!paste(identifier_key(found$article_id), identifier_key(target)) %in% known, , drop = FALSE]
}

# An identifier for comparison: a DOI in any serialization becomes the
# normalized (lowercase, bare) DOI, since DOIs are case-insensitive; any
# other identifier is a case-sensitive literal, kept as it is.
identifier_key <- function(x) {
  x <- as.character(x)
  doi <- normalize_doi(x)
  ifelse(is.na(doi), trimws(x), doi)
}

#' Mark which citations the metrics service already has
#'
#' A citation is a (citing work, target) tuple, where the target is the
#' identifier the work cited, a PID or a SID; the service records it the
#' same way. A citation is "yes" when the service has the same tuple
#' (compared with identifier_key()), and "no" otherwise, including when the
#' service has the same work citing other targets: every tuple is submitted.
#' "unmapped" means the citation has no DataONE identifier as its target (see
#' annotate_citations()); it is never submitted.
#' @noRd
metrics_status <- function(total, recorded) {
  pair <- paste(identifier_key(total$article_id), identifier_key(total$target_id))
  recorded_pair <- paste(identifier_key(recorded$source_id), identifier_key(recorded$target_id))
  ifelse(is.na(total$target_id), "unmapped", ifelse(pair %in% recorded_pair, "yes", "no"))
}

# Compare the running total with the metrics service, at most every
# `every_hours`: mark each citation's `in_metrics` and write
# results/to-submit.csv with those the service doesn't have. On failure, the
# run carries on and the next run tries again.
check_metrics <- function(paths, now, every_hours) {
  state <- read_state(paths$metrics) %||% list()
  if (is.null(every_hours) || every_hours <= 0) return(state)
  if (!is.null(state$checked) &&
      difftime(now, parse_utc(state$checked), units = "hours") < every_hours) {
    return(state)
  }
  total <- read_csv_if_exists(paths$total)
  if (nrow(total) == 0) return(state)
  nodes <- sort(unique(stats::na.omit(total$node)))
  recorded <- tryCatch(get_metrics_citations(nodes), error = function(e) e)
  state$last_attempt <- format_utc(now)
  if (inherits(recorded, "error")) {
    state$error <- conditionMessage(recorded)
    state$failing_since <- state$failing_since %||% format_utc(now)
    message("Couldn't compare with the metrics service (", state$error, "); trying again on the next run.")
    write_state(state, paths$metrics)
    return(state)
  }
  total$in_metrics <- metrics_status(total, recorded)
  write_csv_atomic(total, paths$total)
  # The citations to submit, as the service records them: the citing work's
  # DOI in one canonical form, and the target exactly as in DataONE.
  submit <- total[total$in_metrics == "no", , drop = FALSE]
  submit <- data.frame(source_id = identifier_key(submit$article_id), submit, stringsAsFactors = FALSE)
  first <- c("source_id", "target_id", "target_type", "node")
  write_csv_atomic(submit[c(first, setdiff(names(submit), first))],
                   file.path(paths$results, "to-submit.csv"))
  counts <- table(factor(total$in_metrics, levels = c("yes", "no", "unmapped")))
  state <- list(checked = format_utc(now), last_attempt = format_utc(now),
                recorded = nrow(recorded), yes = counts[["yes"]], no = counts[["no"]],
                unmapped = counts[["unmapped"]])
  message("Metrics service: ", counts[["yes"]], " citations already recorded, ", counts[["no"]],
          " to submit (results/to-submit.csv)",
          if (counts[["unmapped"]] > 0) paste0(", ", counts[["unmapped"]], " without a DataONE identifier"),
          ".")
  write_state(state, paths$metrics)
  state
}
