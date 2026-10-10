# A fake HTTP service and clock for testing the shared client and the
# per-identifier sources offline.
#
# `handler(url, headers, n)` returns the response to the n-th request, as made
# by respond(). The fake records every request's URL, headers and (fake) time.
fake_http <- function(handler = function(url, headers, n) respond()) {
  server <- new.env()
  server$handler <- handler
  server$requests <- list()
  server$get <- function(urls, headers = list()) {
    lapply(urls, function(url) {
      server$requests[[length(server$requests) + 1]] <-
        list(url = url, headers = headers, time = server$clock$now)
      server$handler(url, headers, length(server$requests))
    })
  }
  server$urls <- function() vapply(server$requests, `[[`, "", "url")
  server
}

# A response with `body` given as JSON text, or as a list to encode as JSON.
respond <- function(status = 200, body = "{}", headers = list()) {
  if (is.list(body)) body <- jsonlite::toJSON(body, auto_unbox = TRUE)
  list(status = status, headers = headers, body = as.character(body))
}

fixture <- function(name) {
  paste(readLines(test_path("fixtures", name), warn = FALSE), collapse = "\n")
}

# Run `code` against a fake HTTP service with a fake clock, the given API
# keys, and logs in a temporary directory.
with_fake_http <- function(server, code, keys = list(scopus = "test-key", springer = "test-key"),
                           env = parent.frame()) {
  clock <- new.env()
  clock$now <- as.POSIXct("2026-10-01 12:00:00", tz = "UTC")
  clock$sleeps <- numeric()
  server$clock <- clock
  testthat::local_mocked_bindings(
    http_get = server$get,
    http_now = function() clock$now,
    http_sleep = function(s) {
      clock$sleeps <- c(clock$sleeps, s)
      clock$now <- clock$now + s
    },
    scythe_get_key = function(source) keys[[source]] %||% NA,
    .package = "scythe",
    .env = env
  )
  suppressMessages(force(code))
}

# The identifier a fake request searched for, decoded from its URL.
requested_id <- function(url) {
  q <- utils::URLdecode(sub("^.*[?&](q|term|query)=([^&]*).*$", "\\2", url))
  gsub("^ALL:|\"", "", q)
}
