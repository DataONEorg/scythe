#' Report estimated wait for rate limited queries
#'
#' @param n_queries number of queries
#' @param wait_seconds wait time in seconds between queries
#'
#' @return `NULL`, invisibly. Called for its side effect of printing the
#'   rate limit and estimated run time with [message()].
#' @noRd
report_est_wait <- function(n_queries, wait_seconds) {
  if (n_queries <= 1) {
    return(invisible())
  }
  rate_info <- if (wait_seconds == 0) {
    "None"
  } else if (wait_seconds <= 1) {
    sprintf("%d call(s) per second", round(1 / wait_seconds))
  } else {
    sprintf("One call every %.1f seconds", round(wait_seconds, 1))
  }
  completion_time_sec <- n_queries * wait_seconds
  wait_info <- if (completion_time_sec < 1) {
    "<1 second"
  } else if (completion_time_sec <= 60) {
    sprintf("%d second(s)", round(completion_time_sec))
  } else {
    sprintf("%.1f minute(s)", round(completion_time_sec / 60, 1))
  }
  message("Rate limit: ", rate_info)
  message("Estimated time to run ", n_queries, " searches: ", wait_info)
}

# Prefixes that can precede a DOI name: resolver URLs (http or https, with or
# without dx./www.), handle URLs, and the doi:, info:doi/ and urn:doi: schemes.
doi_prefix_pattern <- paste0(
  "^(?:(?:https?://)?(?:dx\\.|www\\.)?doi\\.org/",
  "|(?:https?://)?hdl\\.handle\\.net/",
  "|info:doi/|urn:doi:|doi:\\s*|doi\\s+)"
)

#' Normalize DOIs to bare, lowercased DOI names
#'
#' Strips any mix of resolver URLs and DOI schemes (for example
#' `https://dx.doi.org/`, `doi:`, `DOI: `, `info:doi/`, or stacked forms such
#' as `doi:https://doi.org/`), URL-decodes the identifier, and lowercases it.
#' DOI names are case-insensitive, so the lowercased form can be compared
#' directly with the DOIs that OpenAlex returns.
#'
#' @param x a character vector of identifiers
#' @return a character vector the same length as `x`, with `NA` for any
#' identifier that is not a DOI after normalization
#' @noRd
normalize_doi <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- vapply(x, function(s) {
    tryCatch(utils::URLdecode(s), error = function(e) s)
  }, "", USE.NAMES = FALSE)
  x <- trimws(x)
  x <- gsub("^[<\"'(\\[]+|[>\"'.,;\\]]+$", "", x, perl = TRUE)
  repeat {
    y <- trimws(sub(doi_prefix_pattern, "", x, perl = TRUE, ignore.case = TRUE))
    if (identical(y, x)) break
    x <- y
  }
  # Drop a trailing ")" only when it is unbalanced, since DOIs such as
  # 10.1002/(SICI)1097-... legitimately contain parentheses.
  unbalanced <- grepl("\\)$", x) &
    lengths(regmatches(x, gregexpr("\\)", x))) > lengths(regmatches(x, gregexpr("\\(", x)))
  x[unbalanced] <- sub("\\)$", "", x[unbalanced])
  x <- tolower(x)
  x[!grepl("^10\\.\\d{4,9}/\\S+$", x, perl = TRUE)] <- NA_character_
  x
}

# Split a vector into consecutive chunks of at most `size` elements.
chunk <- function(x, size) {
  if (length(x) == 0) return(list())
  unname(split(x, ceiling(seq_along(x) / size)))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
