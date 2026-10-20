# Tests of the shared rate limiter, using a fake clock.

with_fake_clock <- function(code, env = parent.frame()) {
  with_fake_http(fake_http(), code, env = env)
}

# Send `n` requests through limiter `l`, recording the fake time of each.
send <- function(l, n) {
  times <- numeric()
  for (k in seq_len(n)) {
    if (!limiter_ready(l)) break
    limiter_record_requests(l)
    limiter_success(l)
    times <- c(times, as.numeric(http_now()))
  }
  times
}

test_that("requests are spaced by the minimum interval", {
  with_fake_clock({
    l <- rate_limiter("svc", min_interval = 2)
    times <- send(l, 4)
    expect_equal(diff(times), c(2, 2, 2))
  })
})

test_that("request windows are kept to, waiting when the wait is short", {
  with_fake_clock({
    l <- rate_limiter("svc", windows = list(c(60, 3)))
    times <- send(l, 7)
    expect_length(times, 7)
    # Three requests at once, then the next three when the first minute ends.
    expect_equal(times - times[1], c(0, 0, 0, 60, 60, 60, 120))
  })
})

test_that("a long wait for a window stops the limiter and is recorded for later runs", {
  path <- file.path(withr::local_tempdir(), "state.json")
  with_fake_clock({
    l <- rate_limiter("plos", label = "PLOS", state_file = path,
                      windows = list(c(60, 10), c(86400, 12)))
    expect_warning(times <- send(l, 20), class = "scythe_rate_limited")
    expect_length(times, 12)
    expect_equal(l$stopped, "rate_limited")
    limiter_save(l)

    # A new run before the day is over sends nothing.
    later <- rate_limiter("plos", state_file = path, windows = list(c(60, 10), c(86400, 12)))
    expect_equal(later$stopped, "rate_limited")
    expect_length(send(later, 1), 0)
  })
})

test_that("the window warning says which limit was reached and until when", {
  with_fake_clock({
    l <- rate_limiter("plos", label = "PLOS", windows = list(c(3600, 2)))
    send(l, 2)
    expect_warning(limiter_ready(l), "PLOS has reached its request limit \\(2 an hour\\) until 2026-10-01T13:00:00Z")
  })
})

test_that("request times are saved, so a later run still counts them against a window", {
  path <- file.path(withr::local_tempdir(), "state.json")
  with_fake_clock({
    l <- rate_limiter("svc", state_file = path, windows = list(c(3600, 5)), max_wait = 10)
    send(l, 3)
    limiter_save(l)
    expect_length(read_service_state(path, "svc")$requests, 3)
    later <- rate_limiter("svc", state_file = path, windows = list(c(3600, 5)), max_wait = 10)
    expect_warning(times <- send(later, 5), class = "scythe_rate_limited")
    expect_length(times, 2)
  })
})

test_that("a rate limit doubles an adaptive interval, and successes shrink it back", {
  with_fake_clock({
    l <- rate_limiter("svc", min_interval = 0.5, max_wait = 1e6)
    limiter_rate_limited(l, 1, "HTTP 429")
    expect_equal(l$interval, 1)
    limiter_rate_limited(l, 1, "HTTP 429")
    expect_equal(l$interval, 2)
    for (k in 1:200) limiter_success(l)
    expect_equal(l$interval, 0.5)
  })
})

test_that("the learned interval is carried over to the next run", {
  path <- file.path(withr::local_tempdir(), "state.json")
  with_fake_clock({
    l <- rate_limiter("svc", state_file = path, min_interval = 0.5)
    limiter_rate_limited(l, 1, "HTTP 429")
    limiter_rate_limited(l, 1, "HTTP 429")
    limiter_save(l)
    expect_equal(rate_limiter("svc", state_file = path, min_interval = 0.5)$interval, 2)
  })
})

test_that("a non-adaptive limiter keeps its interval", {
  with_fake_clock({
    l <- rate_limiter("svc", adaptive = FALSE)
    limiter_rate_limited(l, 1, "HTTP 429")
    expect_equal(l$interval, 0)
  })
})

test_that("a short pause is waited out, then requests resume one at a time", {
  with_fake_clock({
    l <- rate_limiter("svc", concurrency = 4)
    limiter_rate_limited(l, 30, "HTTP 429")
    start <- http_now()
    expect_true(limiter_ready(l))
    expect_equal(as.numeric(http_now() - start, units = "secs"), 30)
    expect_equal(l$active_concurrency, 1)
    for (k in 1:10) limiter_success(l)
    expect_equal(l$active_concurrency, 4)
  })
})

test_that("rate limits with no reset time back off exponentially, up to max_backoff", {
  with_fake_clock({
    l <- rate_limiter("svc", backoff = 900, max_backoff = 3000, max_wait = 1e6)
    pauses <- vapply(1:4, function(k) {
      limiter_rate_limited(l, NA, "HTTP 429")
      wait <- as.numeric(difftime(l$paused_until, http_now(), units = "secs"))
      limiter_ready(l)
      wait
    }, 1)
    expect_equal(pauses, c(900, 1800, 3000, 3000))
  })
})

test_that("a reset time from the service isn't capped by max_backoff", {
  with_fake_clock({
    l <- rate_limiter("svc", max_backoff = 3600)
    limiter_rate_limited(l, 6 * 86400, "weekly quota")
    expect_equal(as.numeric(difftime(l$paused_until, http_now(), units = "days")), 6)
  })
})

test_that("an exhausted quota pauses until it resets", {
  with_fake_clock({
    l <- rate_limiter("svc", label = "Scopus")
    limiter_note_quota(l, 5, 3600)
    expect_null(l$paused_until)
    limiter_note_quota(l, 0, 3600)
    expect_warning(expect_false(limiter_ready(l)), "Scopus is rate limited until 2026-10-01T13:00:00Z")
  })
})

test_that("a run of failures opens the breaker; a success resets the count", {
  with_fake_clock({
    l <- rate_limiter("svc", label = "Springer", failure_breaker = 3, backoff = 900)
    limiter_failure(l)
    limiter_failure(l)
    limiter_success(l)
    limiter_failure(l)
    limiter_failure(l)
    expect_null(l$paused_until)
    limiter_failure(l)
    expect_equal(as.numeric(difftime(l$paused_until, http_now(), units = "secs")), 900)
    expect_warning(expect_false(limiter_ready(l)), "Springer is failing repeatedly")
    expect_equal(l$stopped, "failures")
  })
})

test_that("repeated breaker trips double the pause", {
  with_fake_clock({
    l <- rate_limiter("svc", failure_breaker = 2, backoff = 100, max_wait = 1e6)
    trip <- function() {
      limiter_failure(l)
      limiter_failure(l)
      wait <- as.numeric(difftime(l$paused_until, http_now(), units = "secs"))
      limiter_ready(l)
      wait
    }
    expect_equal(c(trip(), trip(), trip()), c(100, 200, 400))
  })
})

test_that("a rejected key stops the limiter with a scythe_auth_error warning", {
  with_fake_clock({
    l <- rate_limiter("scopus", label = "Scopus")
    expect_warning(limiter_auth_failed(l, "HTTP 401: Invalid API Key"),
                   class = "scythe_auth_error")
    expect_equal(l$stopped, "auth")
    expect_false(limiter_ready(l))
  })
})

test_that("each service has its own entry in a shared state file", {
  path <- file.path(withr::local_tempdir(), "state.json")
  with_fake_clock({
    update_service_state(path, "scopus", paused_until = "2026-10-08T00:00:00Z")
    update_service_state(path, "plos", requests = c(1, 2, 3))
    expect_equal(read_service_state(path, "scopus")$paused_until, "2026-10-08T00:00:00Z")
    expect_equal(read_service_state(path, "plos")$requests, c(1, 2, 3))
    expect_equal(rate_limiter("scopus", state_file = path)$stopped, "rate_limited")
    expect_null(rate_limiter("plos", state_file = path)$stopped)
  })
})

test_that("a pause recorded by another process during a run is honored", {
  path <- file.path(withr::local_tempdir(), "state.json")
  with_fake_clock({
    l <- rate_limiter("svc", state_file = path)
    expect_true(limiter_ready(l))
    update_service_state(path, "svc", paused_until = format_utc(http_now() + 3600))
    expect_warning(expect_false(limiter_ready(l)), class = "scythe_rate_limited")
  })
})

test_that("the wait estimate accounts for request windows", {
  plos <- paste(capture_messages(report_est_wait(28000, 6, plos_windows)), collapse = "")
  expect_match(plos, "at most 10 a minute, 300 an hour, 7200 a day")
  expect_match(plos, "28000 searches: 3.9 day")
  expect_match(paste(capture_messages(report_est_wait(100, 1)), collapse = ""), "1.7 minute")
})

test_that("http_post() sends the body as JSON, with the given headers", {
  url <- local_echo_server()
  body <- jsonlite::toJSON(list(source_id = "10.1/x", note = "café"), auto_unbox = TRUE)
  res <- http_post(paste0(url, "/citations"), body, headers = list(Authorization = "Bearer abc"))
  expect_equal(res$status, 200)
  echo <- jsonlite::fromJSON(res$body)
  expect_equal(echo$method, "POST")
  expect_equal(echo$path, "/citations")
  expect_equal(echo$content_type, "application/json")
  expect_equal(echo$authorization, "Bearer abc")
  expect_match(echo$user_agent, "^scythe")
  expect_equal(jsonlite::fromJSON(echo$body), list(source_id = "10.1/x", note = "café"))
  expect_equal(res$headers[["content-type"]], "application/json")
})

test_that("http_post() keeps a Content-Type the caller sets", {
  url <- local_echo_server()
  res <- http_post(url, "x=1", headers = list("Content-Type" = "text/plain"))
  expect_equal(jsonlite::fromJSON(res$body)$content_type, "text/plain")
})

test_that("http_post() returns error statuses with their body", {
  url <- local_echo_server()
  res <- http_post(paste0(url, "/status/500"), "{}")
  expect_equal(res$status, 500)
  expect_equal(jsonlite::fromJSON(res$body)$method, "POST")
})

test_that("http_post() reports a network error as status NA, with the message", {
  skip_if_not_installed("httpuv")
  res <- http_post(closed_port_url(), "{}")
  expect_true(is.na(res$status))
  expect_type(res$error, "character")
  expect_null(res$body)
})

test_that("the fake records POSTs, and fails a test on requests it can't answer", {
  server <- fake_http(post = function(url, body, headers, n) respond(202, list(message = "Registered")))
  res <- with_fake_http(server, http_post("https://example.org/citations", "{\"a\":1}",
                                          headers = list(Authorization = "Bearer t")))
  expect_equal(res$status, 202)
  posts <- server$posts()
  expect_length(posts, 1)
  expect_equal(posts[[1]]$body, "{\"a\":1}")
  expect_equal(posts[[1]]$headers$Authorization, "Bearer t")

  # Without a POST handler, or with a handler returning NULL, the request
  # gets a network error and the test fails.
  expect_failure(with_fake_http(fake_http(), http_post("https://example.org/x", "{}")),
                 "wasn't set up to answer")
  expect_failure(with_fake_http(fake_http(function(url, headers, n) NULL),
                                http_get("https://example.org/y")),
                 "wasn't set up to answer")
})
