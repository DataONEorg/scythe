#' Search for citations in xDD
#'
#' This function searches for citations in xDD. Uses 'snippets/term' function in xDD API and
#' searches through all of xDD corpus (not limited to full-text documents).
#'
#' Requests are spaced at least 0.2 seconds apart, and the spacing adapts if
#' xDD rate-limits the search. When xDD reports a rate limit, the search
#' pauses, or stops and returns what it has found with a warning of class
#' `scythe_rate_limited`. Identifiers that fail for other reasons are skipped
#' with a warning of class `scythe_request_failed`. With a `checkpoint_dir`, a
#' rerun with the same directory continues where an interrupted search
#' stopped.
#'
#' @param identifiers a vector of identifiers to be searched for, without hypertext
#' transfer protocol: "https://" or "http://"
#' @inheritParams citation_search_scopus
#'
#' @return data.frame of matching dataset and publication identifiers, with a
#'   `search_status` attribute
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR", "10.18739/A29K97")
#' result <- citation_search_xdd(identifiers)
#' }
#'
citation_search_xdd <- function(identifiers, checkpoint_dir = NULL, state_file = NULL,
                                deadline = NULL) {
  identifiers <- check_identifiers(identifiers)
  limiter <- source_limiter("xdd", "xDD", state_file, checkpoint_dir, min_interval = 0.2)
  report_est_wait(length(identifiers), limiter$min_interval)

  search_by_identifier(
    "xdd", identifiers, limiter = limiter, checkpoint_dir = checkpoint_dir,
    deadline = deadline,
    request = function(id) list(
      url = paste0("https://xdd.wisc.edu/api/snippets?term=",
                   utils::URLencode(bare_doi(id), reserved = TRUE), "&corpus=all")
    ),
    parse = parse_xdd
  )
}

# xDD returns matching works as `success$data`; works without a DOI are
# dropped. An `error` in the body is a failed request.
parse_xdd <- function(body) {
  if (!is.null(body$error)) {
    stop(body$error$message %||% paste(unlist(body$error), collapse = " "), call. = FALSE)
  }
  data <- Filter(function(d) is.character(d$doi) && nzchar(d$doi), body$success$data %||% list())
  data.frame(
    article_id = vapply(data, function(d) d$doi, ""),
    article_title = vapply(data, function(d) d$title %||% NA_character_, ""),
    stringsAsFactors = FALSE
  )
}
