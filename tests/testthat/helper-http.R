# A fake HTTP service and clock for testing the shared client and the
# per-identifier sources offline.
#
# `handler(url, headers, n)` returns the response to the n-th request (a GET),
# as made by respond(). `post(url, body, headers, n)` does the same for POST
# requests; a fake without one answers no POSTs. The fake records every
# request's method, URL, headers, body and (fake) time.
#
# A request the fake isn't set up to answer (any POST without `post`, or a
# handler returning NULL) gets a network error, and with_fake_http() then
# fails the test, so that an unmocked call can't pass unnoticed.
fake_http <- function(handler = function(url, headers, n) respond(), post = NULL) {
  server <- new.env()
  server$handler <- handler
  server$post_handler <- post
  server$requests <- list()
  server$unanswered <- character()
  record <- function(method, url, headers, body = NULL) {
    server$requests[[length(server$requests) + 1]] <-
      list(method = method, url = url, headers = headers, body = body, time = server$clock$now)
    length(server$requests)
  }
  unanswered <- function(method, url) {
    server$unanswered <- c(server$unanswered, paste(method, url))
    list(status = NA_integer_, headers = list(), body = NULL,
         error = "the fake HTTP service wasn't set up to answer this request")
  }
  server$get <- function(urls, headers = list()) {
    lapply(urls, function(url) {
      n <- record("GET", url, headers)
      server$handler(url, headers, n) %||% unanswered("GET", url)
    })
  }
  server$post <- function(url, body, headers = list()) {
    n <- record("POST", url, headers, body)
    if (is.null(server$post_handler)) return(unanswered("POST", url))
    server$post_handler(url, body, headers, n) %||% unanswered("POST", url)
  }
  server$urls <- function() vapply(server$requests, `[[`, "", "url")
  server$posts <- function() Filter(function(r) identical(r$method, "POST"), server$requests)
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
# keys (by default including a DataONE token, test_token()), and logs in a
# temporary directory.
with_fake_http <- function(server, code,
                           keys = list(scopus = "test-key", springer = "test-key",
                                       dataone_token = test_token()),
                           env = parent.frame()) {
  clock <- new.env()
  clock$now <- as.POSIXct("2026-10-01 12:00:00", tz = "UTC")
  clock$sleeps <- numeric()
  server$clock <- clock
  testthat::local_mocked_bindings(
    http_get = server$get,
    http_post = server$post,
    http_now = function() clock$now,
    http_sleep = function(s) {
      clock$sleeps <- c(clock$sleeps, s)
      clock$now <- clock$now + s
    },
    scythe_get_key = function(source) keys[[source]] %||% NA,
    .package = "scythe",
    .env = env
  )
  result <- suppressMessages(force(code))
  if (length(server$unanswered)) {
    testthat::fail(paste0("The fake HTTP service got requests it wasn't set up to answer:\n",
                          paste(" ", utils::head(server$unanswered, 5), collapse = "\n")))
  }
  result
}

# The identifier a fake request searched for, decoded from its URL.
requested_id <- function(url) {
  q <- utils::URLdecode(sub("^.*[?&](q|term|query)=([^&]*).*$", "\\2", url))
  gsub("^ALL:|\"", "", q)
}

# A real HTTP server on 127.0.0.1, in a background process, that echoes each
# request back as JSON: its `method`, `path`, `content_type`,
# `authorization`, `user_agent` and `body`. A request to /status/<code>
# is answered with that status. It's only for testing http_get() and
# http_post() themselves, without a network; it stops when `env` exits.
local_echo_server <- function(env = parent.frame()) {
  testthat::skip_if_not_installed("httpuv")
  port <- httpuv::randomPort()
  proc <- callr::r_bg(function(port) {
    or <- function(x, y) if (is.null(x)) y else x
    httpuv::runServer("127.0.0.1", port, list(call = function(req) {
      body <- req$rook.input$read()
      body <- if (length(body)) rawToChar(body) else ""
      Encoding(body) <- "UTF-8"
      status <- suppressWarnings(as.integer(sub("^/status/", "", req$PATH_INFO)))
      list(status = if (is.na(status)) 200L else status,
           headers = list("Content-Type" = "application/json"),
           body = jsonlite::toJSON(list(
             method = req$REQUEST_METHOD, path = req$PATH_INFO,
             content_type = or(req$CONTENT_TYPE, ""), authorization = or(req$HTTP_AUTHORIZATION, ""),
             user_agent = or(req$HTTP_USER_AGENT, ""), body = body
           ), auto_unbox = TRUE))
    }))
  }, args = list(port = port))
  withr::defer(proc$kill(), envir = env)
  url <- paste0("http://127.0.0.1:", port)
  for (i in 1:50) {
    up <- tryCatch(curl::curl_fetch_memory(url)$status_code == 200, error = function(e) FALSE)
    if (up) return(url)
    if (!proc$is_alive()) break
    Sys.sleep(0.1)
  }
  testthat::skip(paste("the local test server didn't start:", proc$read_all_error()))
}

# The URL of a local port that nothing listens on, for network errors.
closed_port_url <- function() paste0("http://127.0.0.1:", httpuv::randomPort())
