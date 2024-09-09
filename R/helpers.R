#' Report estimated wait for rate limited queries
#'
#' @param n_queries number of queries
#' @param wait_seconds wait time in seconds between queries
#'
#' @return
#'
report_est_wait <- function(n_queries, wait_seconds) {
  if (n_queries <= 1) {
    return()
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
