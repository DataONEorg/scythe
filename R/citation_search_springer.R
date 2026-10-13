#' Search for citations from Springer
#'
#' This function searches the Springer Nature Metadata API for works that
#' mention each identifier. It requires an API key from
#' [Springer Nature](https://dev.springernature.com/), set with
#' `scythe_set_key("springer", ...)`.
#'
#' Requests are spaced at least a second apart, and the spacing adapts if
#' Springer rate-limits the search. When Springer reports a rate limit, the
#' search pauses, or stops and returns what it has found with a warning of
#' class `scythe_rate_limited`. A run of consecutive failures is treated the
#' same way, in case Springer reports a rate limit as some other error. (A
#' search with no results is answered with HTTP 404 "No data was found", which
#' is just an empty result.) A rejected API key stops the search with a
#' warning of class `scythe_auth_error`, and identifiers that fail for other
#' reasons are skipped with a warning of class `scythe_request_failed`.
#'
#' With a `checkpoint_dir`, a rerun with the same directory continues where an
#' interrupted search stopped.
#'
#' @inheritParams citation_search_scopus
#'
#' @return data.frame of matching dataset and publication identifiers, with a
#'   `search_status` attribute
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_springer(identifiers)
#' }
citation_search_springer <- function(identifiers, checkpoint_dir = NULL, state_file = NULL,
                                     deadline = NULL) {
  identifiers <- check_identifiers(identifiers)
  key <- scythe_get_key("springer")
  if (is.na(key)) {
    warning(
      "Skipping Springer search due to missing API key. Set an API key using scythe_set_key() to include Springer results."
    )
    return(no_key_result("springer", identifiers))
  }
  limiter <- source_limiter("springer", "Springer", state_file, checkpoint_dir, min_interval = 1)
  report_est_wait(length(identifiers), limiter$min_interval)

  search_by_identifier(
    "springer", identifiers, limiter = limiter, checkpoint_dir = checkpoint_dir,
    deadline = deadline,
    request = function(id) list(
      url = paste0("https://api.springernature.com/meta/v2/json?q=",
                   utils::URLencode(bare_doi(id), reserved = TRUE), "&api_key=", key)
    ),
    parse = parse_springer,
    no_results = springer_no_results
  )
}

# Springer answers a search with no results with a 404 and the message "No
# data was found for the given query."
springer_no_results <- function(res) {
  isTRUE(res$status == 404) &&
    grepl("no (matching )?data", response_message(res) %||% "", ignore.case = TRUE)
}

# Springer returns matching works as `records`, each with an identifier of
# the form "doi:10...."; records without a DOI are dropped.
parse_springer <- function(body) {
  records <- Filter(function(r) is.character(r$identifier) && nzchar(r$identifier),
                    body$records %||% list())
  data.frame(
    article_id = sub("^doi:", "", vapply(records, function(r) r$identifier, "")),
    article_title = vapply(records, function(r) r$title %||% NA_character_, ""),
    stringsAsFactors = FALSE
  )
}
