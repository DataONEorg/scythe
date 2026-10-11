#' Search for citations in Scopus
#'
#' This function searches Scopus for works that mention each identifier. It
#' requires an API key from the [Elsevier Developer Portal](https://dev.elsevier.com/),
#' set with `scythe_set_key("scopus", ...)`.
#'
#' Requests are spaced at least 0.12 seconds apart, and the spacing adapts if
#' Scopus rate-limits the search. Scopus reports its remaining quota (often
#' weekly) in each response; once it is used up, the search stops and returns
#' what it has found, with a warning of class `scythe_rate_limited` saying
#' when the quota resets. A rejected API key stops the search with a warning
#' of class `scythe_auth_error`. Identifiers that fail for other reasons are
#' skipped, with a warning of class `scythe_request_failed`.
#'
#' With a `checkpoint_dir`, each identifier searched is recorded there, and a
#' rerun with the same directory continues where an interrupted search
#' stopped; quota pauses are recorded in `state_file`, so a rerun before the
#' quota resets sends no requests.
#'
#' @param identifiers a vector of identifiers to be searched for
#' @param checkpoint_dir directory recording the identifiers already searched,
#'   or NULL to disable checkpointing
#' @param state_file file recording rate-limit pauses and recent request
#'   times between runs; defaults to `scopus-state.json` in `checkpoint_dir`
#' @param deadline a time (POSIXct) at which to stop searching and return the
#'   results found so far, or NULL to search until done
#'
#' @return data.frame of matching dataset and publication identifiers, with a
#'   `search_status` attribute recording whether the search completed, why it
#'   stopped if not, and which identifiers failed
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_scopus(identifiers)
#' attr(result, "search_status")
#' }
citation_search_scopus <- function(identifiers, checkpoint_dir = NULL, state_file = NULL,
                                   deadline = NULL) {
  identifiers <- check_identifiers(identifiers)
  key <- scythe_get_key("scopus")
  if (is.na(key)) {
    warning(
      "Skipping Scopus search due to missing API key. Set an API key using scythe_set_key() to include Scopus results."
    )
    return(empty_citations())
  }
  limiter <- source_limiter("scopus", "Scopus", state_file, checkpoint_dir, min_interval = 0.12)
  report_est_wait(length(identifiers), limiter$min_interval)

  search_by_identifier(
    "scopus", identifiers, limiter = limiter, checkpoint_dir = checkpoint_dir,
    deadline = deadline,
    # The key goes in a header, so it never appears in URLs or error messages.
    request = function(id) list(
      url = paste0("https://api.elsevier.com/content/search/scopus?query=ALL:",
                   utils::URLencode(id, reserved = TRUE)),
      headers = list(`X-ELS-APIKey` = key, Accept = "application/json")
    ),
    parse = parse_scopus,
    quota = scopus_quota
  )
}

# Scopus returns its search results as `search-results$entry`; an empty
# result is a single entry holding an error.
parse_scopus <- function(body) {
  entries <- body$`search-results`$entry %||% list()
  entries <- Filter(function(e) !is.null(e$`prism:doi`), entries)
  data.frame(
    article_id = vapply(entries, function(e) e$`prism:doi`, ""),
    article_title = vapply(entries, function(e) e$`dc:title` %||% NA_character_, ""),
    stringsAsFactors = FALSE
  )
}

# Elsevier reports the remaining quota, and when it resets as a Unix time in
# seconds.
scopus_quota <- function(headers) {
  reset_at <- header_number(headers, "x-ratelimit-reset")
  list(remaining = header_number(headers, "x-ratelimit-remaining"),
       reset = if (is.na(reset_at)) NA else max(0, reset_at - as.numeric(http_now())))
}
