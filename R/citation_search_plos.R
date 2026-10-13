#' Search for citations in PLOS
#'
#' This function searches the full text of PLOS articles for each identifier,
#' with one request to the PLOS search API per identifier.
#'
#' PLOS limits its API to 10 requests a minute, 300 an hour and 7,200 a day.
#' The search keeps to these limits, waiting when a minute's or an hour's limit
#' is reached; once a longer wait is needed (the day's limit), it stops and
#' returns what it has found, with a warning of class `scythe_rate_limited`
#' saying when it can continue. The limits can be changed with
#' `options(scythe.plos.windows = list(c(60, 10), c(3600, 300), c(86400, 7200)))`,
#' each element giving a window in seconds and the requests allowed in it.
#' With a `checkpoint_dir`, a rerun with the same directory continues where an
#' interrupted search stopped, and the requests already sent still count
#' against the limits.
#'
#' @inheritParams citation_search_scopus
#'
#' @return data.frame of matching dataset and publication identifiers, with a
#'   `search_status` attribute
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_plos(identifiers)
#' }
citation_search_plos <- function(identifiers, checkpoint_dir = NULL, state_file = NULL,
                                 deadline = NULL) {
  identifiers <- check_identifiers(identifiers)
  limiter <- source_limiter("plos", "PLOS", state_file, checkpoint_dir, min_interval = 6,
                            windows = plos_windows)
  report_est_wait(length(identifiers), limiter$min_interval, limiter$windows)

  search_by_identifier(
    "plos", identifiers, limiter = limiter, checkpoint_dir = checkpoint_dir,
    deadline = deadline,
    request = function(id) list(url = plos_search_url(id)),
    parse = parse_plos
  )
}

# PLOS's published API limits: requests allowed per minute, hour and day.
plos_windows <- list(c(60, 10), c(3600, 300), c(86400, 7200))

# Search for the identifier as a phrase, so that Solr doesn't treat the "/"
# in a DOI as the start of a regular expression, returning only full articles.
plos_search_url <- function(identifier) {
  query <- list(q = paste0("\"", bare_doi(identifier), "\""), fl = "id,title", fq = "doc_type:full",
                rows = "1000", wt = "json")
  values <- vapply(query, utils::URLencode, "", reserved = TRUE)
  paste0("https://api.plos.org/search?", paste(names(query), values, sep = "=", collapse = "&"))
}

parse_plos <- function(body) {
  docs <- Filter(function(d) is.character(d$id), body$response$docs %||% list())
  data.frame(
    article_id = vapply(docs, function(d) d$id, ""),
    article_title = vapply(docs, function(d) as.character(unlist(d$title))[1] %||% NA_character_, ""),
    stringsAsFactors = FALSE
  )
}
