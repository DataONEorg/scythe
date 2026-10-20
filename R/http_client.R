# Shared HTTP client for every search source, and for submitting citations to
# the metrics service: sending requests, rate limiting,
# and the state file that records rate-limit pauses between runs.
#
# Each source sends its requests through a rate limiter, an environment made
# by rate_limiter(). The limiter
#
# - spaces requests by an interval, which (if `adaptive`) doubles whenever the
#   service rate-limits it and slowly shrinks back while requests succeed;
# - keeps to fixed request windows, such as PLOS's 10 requests a minute;
# - opens a circuit breaker when the service rate-limits it, reports an
#   exhausted quota, or fails repeatedly (some services report rate limits as
#   other errors).
#
# A pause up to `max_wait` seconds is waited out. A longer one stops the
# limiter for the rest of the run, with a warning of class
# `scythe_rate_limited`, and is written to the state file so that later runs
# and other processes sharing the file hold off until it ends. A rejected API
# key stops the limiter with a warning of class `scythe_auth_error`, since
# waiting won't fix it.

# Wrappers around the clock, sleeping and the network so tests can replace them.
http_now <- function() Sys.time()
http_sleep <- function(seconds) Sys.sleep(seconds)

# Send GET requests for `urls` concurrently, with the request `headers` (a
# named list), and return for each a list with the HTTP `status` (NA on a
# network error), the response `headers` as a named list with lowercase names,
# and the `body` as text.
http_get <- function(urls, headers = list()) {
  pool <- curl::new_pool(total_con = length(urls), host_con = length(urls))
  out <- vector("list", length(urls))
  for (i in seq_along(urls)) {
    local({
      j <- i
      handle <- curl::new_handle(
        url = urls[j],
        useragent = "scythe (https://github.com/DataONEorg/scythe)",
        timeout = 120
      )
      if (length(headers)) curl::handle_setheaders(handle, .list = headers)
      curl::multi_add(
        handle,
        pool = pool,
        done = function(res) {
          out[[j]] <<- list(
            status = res$status_code,
            headers = curl::parse_headers_list(res$headers),
            body = rawToChar(res$content)
          )
        },
        fail = function(msg) {
          out[[j]] <<- list(status = NA_integer_, headers = list(), body = NULL, error = msg)
        }
      )
    })
  }
  curl::multi_run(pool = pool)
  out
}

# Send a POST request to `url` with `body` (JSON text) and the request
# `headers` (a named list; Content-Type defaults to application/json), and
# return a list like each of http_get()'s: the HTTP `status` (NA on a network
# error, with the message in `error`), the response `headers` and the `body`.
http_post <- function(url, body, headers = list()) {
  if (is.null(headers[["Content-Type"]])) headers[["Content-Type"]] <- "application/json"
  handle <- curl::new_handle(
    useragent = "scythe (https://github.com/DataONEorg/scythe)",
    timeout = 120,
    post = TRUE,
    postfields = enc2utf8(as.character(body))
  )
  curl::handle_setheaders(handle, .list = headers)
  tryCatch({
    res <- curl::curl_fetch_memory(url, handle = handle)
    list(status = res$status_code, headers = curl::parse_headers_list(res$headers),
         body = rawToChar(res$content))
  }, error = function(e) {
    list(status = NA_integer_, headers = list(), body = NULL, error = conditionMessage(e))
  })
}

format_utc <- function(time) format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
parse_utc <- function(text) as.POSIXct(text, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# A response header as a number, or NA.
header_number <- function(headers, name) {
  suppressWarnings(as.numeric(headers[[name]] %||% NA))
}

# The state file holds one entry per service, so several services can share
# one file: {"openalex": {"paused_until": ...}, "plos": {...}}. Writes go
# through a temporary file and a rename so that a reader never sees a partly
# written file. Updates aren't locked, so a writer never shortens a pause
# another process has recorded.
read_service_state <- function(path, service = NULL) {
  state <- if (is.null(path) || !file.exists(path)) {
    list()
  } else {
    tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) list())
  }
  if (is.null(service)) state else state[[service]] %||% list()
}

update_service_state <- function(path, service, ...) {
  if (is.null(path)) return(invisible())
  state <- read_service_state(path)
  entry <- state[[service]] %||% list()
  fields <- list(...)
  if (!is.null(fields$paused_until) && !is.null(entry$paused_until) &&
      isTRUE(parse_utc(entry$paused_until) > parse_utc(fields$paused_until))) {
    fields$paused_until <- entry$paused_until
  }
  entry[names(fields)] <- fields
  state[[service]] <- Filter(Negate(is.null), entry)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp", Sys.getpid())
  jsonlite::write_json(state, tmp, auto_unbox = TRUE, pretty = TRUE, digits = NA)
  file.rename(tmp, path)
  invisible(state)
}

# The end of a pause recorded for `service` in the state file, or NULL if
# none is still in force.
shared_pause <- function(path, service) {
  paused_until <- read_service_state(path, service)$paused_until
  if (is.null(paused_until)) return(NULL)
  paused_until <- parse_utc(paused_until)
  if (is.na(paused_until) || paused_until <= http_now()) return(NULL)
  paused_until
}

# Signal a classed warning (`scythe_rate_limited`, `scythe_auth_error` or
# `scythe_request_failed`) that callers can catch with withCallingHandlers().
scythe_warning <- function(class, message, ...) {
  cond <- structure(
    class = c(class, "scythe_warning", "warning", "condition"),
    list(message = message, call = NULL, ...)
  )
  warning(cond)
}

#' Create a rate limiter for one service
#'
#' @param service the service's name in the state file
#' @param label the service's name in messages
#' @param state_file path of the state file recording pauses, the learned
#'   request interval and recent request times, or NULL to keep them in this
#'   process only
#' @param min_interval the least time in seconds between requests
#' @param windows fixed request limits, as a list of `c(seconds, max)`, for
#'   example `list(c(60, 10), c(86400, 7200))`
#' @param adaptive whether the interval between requests doubles when the
#'   service rate-limits it and shrinks back while requests succeed
#' @param max_wait the longest pause, in seconds, waited out rather than
#'   stopping
#' @param backoff the first pause after a rate limit or repeated failures
#'   with no reset time; it doubles on each repeat
#' @param max_backoff the longest such pause
#' @param failure_breaker the number of consecutive failed requests that is
#'   treated as an unreported rate limit
#' @param concurrency the most requests sent at once
#' @return an environment holding the limiter's settings and state
#' @noRd
rate_limiter <- function(service, label = service, state_file = NULL,
                         min_interval = 0, windows = NULL, adaptive = TRUE,
                         max_wait = 300, backoff = 900, max_backoff = 86400,
                         failure_breaker = 10, concurrency = 1) {
  l <- new.env(parent = emptyenv())
  l$service <- service
  l$label <- label
  l$state_file <- state_file
  l$min_interval <- min_interval
  l$windows <- windows
  l$adaptive <- adaptive
  l$max_wait <- max_wait
  l$backoff <- backoff
  l$max_backoff <- max_backoff
  l$failure_breaker <- failure_breaker
  l$concurrency <- concurrency
  l$active_concurrency <- concurrency
  l$interval <- min_interval
  l$last_request <- -Inf
  l$request_log <- numeric()
  l$paused_until <- NULL
  l$pause_kind <- NULL
  l$consecutive_limits <- 0
  l$last_pause <- 0
  l$consecutive_failures <- 0
  l$breaker_trips <- 0
  l$successes <- 0
  l$requests <- 0
  # Why the limiter stopped sending requests ("rate_limited", "failures" or
  # "auth"), or NULL while it is still active.
  l$stopped <- NULL

  # Carry over what earlier runs learned: the request interval, and the
  # request times that still count against a window.
  saved <- read_service_state(state_file, service)
  if (adaptive && is.numeric(saved$interval)) l$interval <- max(min_interval, saved$interval)
  if (length(saved$requests)) {
    l$request_log <- as.numeric(saved$requests)
    trim_request_log(l)
  }

  # Honor a pause recorded by an earlier run or by another process.
  paused_until <- shared_pause(state_file, service)
  if (!is.null(paused_until)) {
    l$paused_until <- paused_until
    l$pause_kind <- saved$pause_kind %||% "rate_limited"
    l$stopped <- l$pause_kind
    message(label, " is rate limited until ", format_utc(paused_until),
            " (recorded in ", state_file, "); not sending any requests.")
  }
  l
}

now_seconds <- function() as.numeric(http_now())

# Keep only the request times inside the longest window.
trim_request_log <- function(l) {
  if (length(l$windows) == 0) {
    l$request_log <- numeric()
    return(invisible())
  }
  longest <- max(vapply(l$windows, `[[`, 1, 1))
  l$request_log <- l$request_log[l$request_log > now_seconds() - longest]
}

# Seconds until another request fits in every request window.
window_wait <- function(l) {
  if (length(l$windows) == 0) return(0)
  now <- now_seconds()
  waits <- vapply(l$windows, function(w) {
    recent <- sort(l$request_log[l$request_log > now - w[1]])
    if (length(recent) < w[2]) return(0)
    # Wait until the oldest request that keeps the window full drops out.
    recent[length(recent) - w[2] + 1] + w[1] - now
  }, 1)
  max(0, waits)
}

#' Open the circuit breaker for `seconds`
#'
#' With `seconds` NA, the pause is `backoff` doubled for each repeated rate
#' limit, up to `max_backoff`. A pause longer than `max_wait` is written to
#' the state file straight away so other processes see it.
#' @noRd
limiter_pause <- function(l, seconds, reason, kind = "rate_limited") {
  computed <- is.na(seconds)
  if (computed) seconds <- l$backoff * 2^max(0, l$consecutive_limits - 1)
  if (l$consecutive_limits >= 3) seconds <- max(seconds, 2 * l$last_pause)
  if (computed) seconds <- min(seconds, l$max_backoff)
  l$last_pause <- seconds
  until <- http_now() + seconds
  if (is.null(l$paused_until) || until > l$paused_until) {
    l$paused_until <- until
    l$pause_kind <- kind
  }
  message(l$label, ": ", reason, "; pausing requests until ", format_utc(l$paused_until))
  if (seconds > l$max_wait) {
    update_service_state(l$state_file, l$service, paused_until = format_utc(l$paused_until),
                         pause_kind = l$pause_kind)
  }
}

# Record a response that rate-limited the client, pausing for `seconds` (or a
# growing backoff when the service didn't say) and, if adaptive, doubling the
# interval between requests.
limiter_rate_limited <- function(l, seconds, reason) {
  l$consecutive_limits <- l$consecutive_limits + 1
  if (l$adaptive) l$interval <- min(60, max(2 * l$interval, l$min_interval, 1))
  limiter_pause(l, seconds, reason)
}

# Record a remaining quota reported by the service, pausing until `reset`
# seconds from now once it is used up.
limiter_note_quota <- function(l, remaining, reset, what = "request quota used up") {
  if (!is.na(remaining) && remaining <= 0) limiter_pause(l, reset, what)
}

# Record a successful request. While requests succeed, an adaptive interval
# shrinks back towards `min_interval`, and after a pause full concurrency
# returns once 10 requests have succeeded.
limiter_success <- function(l) {
  l$consecutive_limits <- 0
  l$consecutive_failures <- 0
  l$breaker_trips <- 0
  l$successes <- l$successes + 1
  if (l$adaptive && l$successes %% 20 == 0 && l$interval > l$min_interval) {
    l$interval <- max(l$min_interval, 0.8 * l$interval)
  }
  if (l$active_concurrency < l$concurrency && l$successes >= 10) {
    l$active_concurrency <- l$concurrency
  }
}

# Record a failed request. After `failure_breaker` failures in a row, assume
# an unreported rate limit and pause, for longer each time it happens again.
limiter_failure <- function(l) {
  l$consecutive_failures <- l$consecutive_failures + 1
  if (l$consecutive_failures >= l$failure_breaker) {
    l$breaker_trips <- l$breaker_trips + 1
    seconds <- min(l$max_backoff, l$backoff * 2^(l$breaker_trips - 1))
    failures <- l$consecutive_failures
    l$consecutive_failures <- 0
    limiter_pause(l, seconds, paste(failures, "requests in a row failed (possibly an unreported rate limit)"),
                  kind = "failures")
  }
}

# Stop the limiter because the service rejected the API key.
limiter_auth_failed <- function(l, detail) {
  l$stopped <- "auth"
  scythe_warning("scythe_auth_error", service = l$service,
                 paste0(l$label, " rejected the request (", detail, "). Check the API key set ",
                        "with scythe_set_key(\"", l$service, "\", ...); no further ", l$label,
                        " requests will be sent in this run."))
}

# Note that `n` requests are about to be sent.
limiter_record_requests <- function(l, n = 1) {
  now <- now_seconds()
  l$last_request <- now
  l$requests <- l$requests + n
  if (length(l$windows)) l$request_log <- c(l$request_log, rep(now, n))
}

#' Wait until the limiter may send a request
#'
#' Returns TRUE when a request may be sent, after waiting out any short pause
#' and the interval since the last request; FALSE once the limiter has
#' stopped. A pause longer than `max_wait` stops the limiter for the rest of
#' this run, with a `scythe_rate_limited` warning. After a pause for a rate
#' limit, requests resume one at a time. Pauses recorded in the state file by
#' other processes while this one runs are honored too.
#' @noRd
limiter_ready <- function(l) {
  if (!is.null(l$stopped)) return(FALSE)
  shared <- shared_pause(l$state_file, l$service)
  if (!is.null(shared) && (is.null(l$paused_until) || shared > l$paused_until)) {
    l$paused_until <- shared
    l$pause_kind <- "rate_limited"
  }
  window <- window_wait(l)
  if (window > 0) {
    until <- http_now() + window
    if (is.null(l$paused_until) || until > l$paused_until) {
      l$paused_until <- until
      l$pause_kind <- "window"
    }
  }
  if (!is.null(l$paused_until)) {
    wait <- as.numeric(difftime(l$paused_until, http_now(), units = "secs"))
    if (wait > l$max_wait) {
      l$stopped <- if (identical(l$pause_kind, "failures")) "failures" else "rate_limited"
      update_service_state(l$state_file, l$service, paused_until = format_utc(l$paused_until),
                           pause_kind = l$stopped)
      why <- switch(l$pause_kind %||% "rate_limited",
        window = paste0("has reached its request limit (", window_limits_text(l$windows), ")"),
        failures = "is failing repeatedly",
        "is rate limited"
      )
      scythe_warning("scythe_rate_limited", service = l$service, paused_until = l$paused_until,
                     reason = l$stopped,
                     paste0(l$label, " ", why, " until ", format_utc(l$paused_until),
                            "; stopping with partial results. Run again after that time to continue."))
      return(FALSE)
    }
    if (wait > 0) http_sleep(wait)
    if (!identical(l$pause_kind, "window")) {
      l$active_concurrency <- 1
      l$successes <- 0
    }
    l$paused_until <- NULL
    l$pause_kind <- NULL
  }
  gap <- l$interval - (now_seconds() - l$last_request)
  if (gap > 0) http_sleep(gap)
  TRUE
}

window_limits_text <- function(windows) {
  unit <- function(s) {
    if (s == 60) "minute" else if (s == 3600) "hour" else if (s == 86400) "day" else paste(s, "seconds")
  }
  limits <- vapply(windows, function(w) {
    u <- unit(w[1])
    paste(w[2], if (u == "hour") "an" else "a", u)
  }, "")
  paste(limits, collapse = ", ")
}

# Save what this run learned (the request interval and the request times that
# still count against a window) for the next run.
limiter_save <- function(l) {
  if (is.null(l$state_file)) return(invisible())
  trim_request_log(l)
  update_service_state(l$state_file, l$service,
                       interval = if (l$adaptive) l$interval,
                       requests = if (length(l$windows)) l$request_log)
}
