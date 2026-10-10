# Offline tests of the per-identifier sources (Scopus, Springer, xDD, PLOS)
# and the runner they share, against a fake HTTP service.

ids <- sprintf("10.18739/A%04d", 1:6)

# A handler answering each identifier with `hits[[id]]` (a list of DOIs) in
# xDD's format, or no hits.
xdd_handler <- function(hits = list()) {
  function(url, headers, n) {
    id <- requested_id(url)
    data <- lapply(hits[[id]] %||% character(), function(doi) list(doi = doi, title = paste("About", doi)))
    respond(body = list(success = list(v = 1, data = data)))
  }
}

# --- The runner ---------------------------------------------------------------

test_that("each identifier is searched once and its citations attributed to it", {
  server <- fake_http(xdd_handler(list("10.18739/A0002" = c("10.1/x", "10.1/y"))))
  with_fake_http(server, {
    res <- citation_search_xdd(ids)
    expect_equal(requested_id(server$urls()), ids)
    expect_equal(res$article_id, c("10.1/x", "10.1/y"))
    expect_equal(res$dataset_id, c("10.18739/A0002", "10.18739/A0002"))
    expect_equal(unique(res$source), "xdd")
    status <- attr(res, "search_status")
    expect_true(status$complete)
    expect_equal(status$searched, 6)
  })
})

test_that("a search with no citations returns an empty data.frame with the usual columns", {
  with_fake_http(fake_http(xdd_handler()), {
    res <- citation_search_xdd(ids[1:2])
    expect_equal(nrow(res), 0)
    expect_named(res, c("article_id", "article_title", "dataset_id", "source"))
  })
})

test_that("server and network errors are retried before an identifier is skipped", {
  server <- fake_http(function(url, headers, n) {
    if (requested_id(url) == ids[2]) {
      if (n %% 2 == 0) respond(503) else list(status = NA, headers = list(), body = NULL, error = "timeout")
    } else {
      xdd_handler()(url, headers, n)
    }
  })
  with_fake_http(server, {
    expect_warning(res <- citation_search_xdd(ids[1:3]),
                   "1 of 3 identifiers could not be searched", class = "scythe_request_failed")
    # One request each for ids 1 and 3, and 4 attempts for id 2.
    expect_equal(table(requested_id(server$urls()))[[ids[2]]], 4)
    expect_length(server$urls(), 6)
    status <- attr(res, "search_status")
    expect_false(status$complete)
    expect_equal(status$failed, ids[2])
  })
})

test_that("a short rate limit is waited out and the same identifier retried", {
  server <- fake_http(function(url, headers, n) {
    if (n == 2) respond(429, headers = list(`retry-after` = "20")) else xdd_handler()(url, headers, n)
  })
  with_fake_http(server, {
    res <- citation_search_xdd(ids[1:3])
    expect_equal(requested_id(server$urls()), ids[c(1, 2, 2, 3)])
    expect_true(attr(res, "search_status")$complete)
    expect_true(20 %in% round(server$clock$sleeps))
  })
})

test_that("a long rate limit stops the search, and a rerun continues after it ends", {
  dir <- withr::local_tempdir()
  server <- fake_http(function(url, headers, n) {
    if (n == 3) respond(429, headers = list(`retry-after` = "7200")) else xdd_handler()(url, headers, n)
  })
  with_fake_http(server, {
    expect_warning(first <- citation_search_xdd(ids, checkpoint_dir = dir),
                   "xDD is rate limited until 2026-10-01T14:00", class = "scythe_rate_limited")
    status <- attr(first, "search_status")
    expect_equal(status$stopped, "rate_limited")
    expect_equal(status$searched, 2)
    expect_equal(status$pending, 4)
    expect_equal(status$paused_until, "2026-10-01T14:00:00Z")

    # Before the limit resets, a rerun sends nothing.
    citation_search_xdd(ids, checkpoint_dir = dir)
    expect_length(server$urls(), 3)

    # Afterwards it searches only what's left.
    server$clock$now <- server$clock$now + 7300
    second <- citation_search_xdd(ids, checkpoint_dir = dir)
    expect_equal(requested_id(server$urls()[-(1:3)]), ids[3:6])
    expect_true(attr(second, "search_status")$complete)
    expect_equal(attr(second, "search_status")$cached, 2)
  })
})

test_that("a resumed search returns the citations found before it stopped", {
  dir <- withr::local_tempdir()
  hits <- list("10.18739/A0001" = "10.1/a", "10.18739/A0005" = "10.1/e")
  server <- fake_http(function(url, headers, n) {
    if (n == 3) respond(429, headers = list(`retry-after` = "7200")) else xdd_handler(hits)(url, headers, n)
  })
  with_fake_http(server, {
    first <- suppressWarnings(citation_search_xdd(ids, checkpoint_dir = dir))
    expect_equal(first$article_id, "10.1/a")
    server$clock$now <- server$clock$now + 7300
    second <- citation_search_xdd(ids, checkpoint_dir = dir)
    expect_equal(second$article_id, c("10.1/a", "10.1/e"))
    expect_equal(second$dataset_id, ids[c(1, 5)])
  })
})

test_that("identifiers that failed are retried on the next run", {
  dir <- withr::local_tempdir()
  fail <- TRUE
  server <- fake_http(function(url, headers, n) {
    if (fail && requested_id(url) == ids[2]) respond(400) else xdd_handler()(url, headers, n)
  })
  with_fake_http(server, {
    expect_warning(citation_search_xdd(ids[1:3], checkpoint_dir = dir),
                   "retried on the next run", class = "scythe_request_failed")
    fail <- FALSE
    res <- citation_search_xdd(ids[1:3], checkpoint_dir = dir)
    expect_equal(requested_id(server$urls()[-(1:3)]), ids[2])
    expect_true(attr(res, "search_status")$complete)
  })
})

test_that("a run of failures is treated as an unreported rate limit", {
  server <- fake_http(function(url, headers, n) respond(404))
  with_fake_http(server, {
    withr::local_options(scythe.xdd.failure_breaker = 4)
    warnings <- list()
    res <- withCallingHandlers(
      citation_search_xdd(sprintf("10.18739/B%04d", 1:20)),
      warning = function(w) {
        warnings[[length(warnings) + 1]] <<- w
        invokeRestart("muffleWarning")
      }
    )
    expect_length(server$urls(), 4)
    expect_equal(attr(res, "search_status")$stopped, "failures")
    classes <- vapply(warnings, function(w) class(w)[1], "")
    expect_setequal(classes, c("scythe_rate_limited", "scythe_request_failed"))
  })
})

test_that("a response that can't be read skips the identifier", {
  server <- fake_http(function(url, headers, n) {
    if (n == 1) respond(body = "{\"success\": {") else xdd_handler()(url, headers, n)
  })
  with_fake_http(server, {
    expect_warning(res <- citation_search_xdd(ids[1:2]), "unreadable response",
                   class = "scythe_request_failed")
    expect_equal(attr(res, "search_status")$failed, ids[1])
  })
})

test_that("a 403 that mentions a quota is a rate limit, not a rejected key", {
  server <- fake_http(function(url, headers, n) {
    respond(403, body = list(message = "Daily quota exceeded"))
  })
  with_fake_http(server, {
    expect_warning(res <- citation_search_springer(ids[1:2]), class = "scythe_rate_limited")
    expect_equal(attr(res, "search_status")$stopped, "rate_limited")
  })
})

# --- Scopus -------------------------------------------------------------------

test_that("Scopus sends its key in a header, never in the URL", {
  server <- fake_http(function(url, headers, n) respond(body = fixture("scopus-empty.json")))
  with_fake_http(server, {
    citation_search_scopus(ids[1])
    expect_false(grepl("test-key", server$urls()))
    expect_equal(server$requests[[1]]$headers$`X-ELS-APIKey`, "test-key")
    expect_match(server$urls(), "^https://api.elsevier.com/content/search/scopus\\?query=ALL:10.18739%2FA0001$")
  })
})

test_that("Scopus results are parsed, skipping entries without a DOI", {
  server <- fake_http(function(url, headers, n) {
    respond(body = fixture(if (n == 1) "scopus-hit.json" else "scopus-empty.json"))
  })
  with_fake_http(server, {
    res <- citation_search_scopus(ids[1:2])
    expect_equal(res$article_id, "10.5194/essd-11-1957-2019")
    expect_equal(res$article_title, "Arctic soil carbon")
    expect_equal(res$dataset_id, ids[1])
  })
})

test_that("an exhausted Scopus quota stops the search until it resets", {
  dir <- withr::local_tempdir()
  reset_at <- as.numeric(as.POSIXct("2026-10-05 00:00:00", tz = "UTC"))
  server <- fake_http(function(url, headers, n) {
    respond(body = fixture("scopus-empty.json"),
            headers = list(`x-ratelimit-limit` = "20000",
                           `x-ratelimit-remaining` = if (n < 2) "1" else "0",
                           `x-ratelimit-reset` = as.character(reset_at)))
  })
  with_fake_http(server, {
    expect_warning(res <- citation_search_scopus(ids, checkpoint_dir = dir),
                   "Scopus is rate limited until 2026-10-05T00:00:00Z", class = "scythe_rate_limited")
    expect_length(server$urls(), 2)
    expect_equal(attr(res, "search_status")$searched, 2)
    expect_equal(read_service_state(file.path(dir, "scopus-state.json"), "scopus")$paused_until,
                 "2026-10-05T00:00:00Z")
  })
})

test_that("a 429 from Scopus waits until the reset time it reports", {
  reset_at <- as.numeric(as.POSIXct("2026-10-01 12:01:00", tz = "UTC"))
  server <- fake_http(function(url, headers, n) {
    if (n == 1) {
      respond(429, body = fixture("scopus-empty.json"),
              headers = list(`x-ratelimit-remaining` = "0", `x-ratelimit-reset` = as.character(reset_at)))
    } else {
      respond(body = fixture("scopus-empty.json"))
    }
  })
  with_fake_http(server, {
    res <- citation_search_scopus(ids[1:2])
    expect_true(attr(res, "search_status")$complete)
    expect_equal(server$requests[[2]]$time, as.POSIXct("2026-10-01 12:01:00", tz = "UTC"))
  })
})

test_that("a rejected Scopus key stops the search with a scythe_auth_error", {
  server <- fake_http(function(url, headers, n) respond(401, body = fixture("scopus-auth.json")))
  with_fake_http(server, {
    expect_warning(res <- citation_search_scopus(ids),
                   "Scopus rejected the request \\(HTTP 401: Invalid API Key\\)",
                   class = "scythe_auth_error")
    expect_length(server$urls(), 1)
    expect_equal(attr(res, "search_status")$stopped, "auth")
  })
})

test_that("without a key, Scopus and Springer are skipped with a warning", {
  with_fake_http(fake_http(), keys = list(), {
    expect_warning(res <- citation_search_scopus(ids), "missing API key")
    expect_equal(nrow(res), 0)
    expect_warning(citation_search_springer(ids), "missing API key")
  })
})

# --- Springer -----------------------------------------------------------------

test_that("Springer uses https and parses records, dropping the doi: prefix", {
  server <- fake_http(function(url, headers, n) respond(body = fixture("springer-hit.json")))
  with_fake_http(server, {
    res <- citation_search_springer(ids[1])
    expect_match(server$urls(), "^https://api.springernature.com/meta/v2/json\\?q=10.18739%2FA0001&api_key=test-key$")
    expect_equal(res$article_id, "10.1007/s13280-019-01295-7")
    expect_equal(res$article_title, "Arctic change and its consequences")
  })
})

test_that("Springer's 404 for a search without results is an empty result, not a failure", {
  dir <- withr::local_tempdir()
  # Recorded from the live API: Springer's answer when nothing matches.
  server <- fake_http(function(url, headers, n) respond(404, body = fixture("springer-empty.json")))
  with_fake_http(server, {
    many <- sprintf("10.18739/D%04d", 1:15)
    expect_no_warning(res <- citation_search_springer(many, checkpoint_dir = dir))
    # More than failure_breaker (10) of them in a row doesn't pause the search.
    expect_length(server$urls(), 15)
    expect_equal(nrow(res), 0)
    status <- attr(res, "search_status")
    expect_true(status$complete)
    expect_null(status$stopped)
    # They are recorded as searched, so a rerun sends nothing.
    citation_search_springer(many, checkpoint_dir = dir)
    expect_length(server$urls(), 15)
  })
})

test_that("other Springer 404s are still failures", {
  server <- fake_http(function(url, headers, n) respond(404, body = list(message = "Endpoint not found")))
  with_fake_http(server, {
    expect_warning(res <- citation_search_springer(ids[1:2]), "Endpoint not found",
                   class = "scythe_request_failed")
    expect_equal(attr(res, "search_status")$failed, ids[1:2])
  })
})

test_that("a rejected Springer key stops the search", {
  server <- fake_http(function(url, headers, n) respond(401, body = list(message = "Invalid API key")))
  with_fake_http(server, {
    expect_warning(res <- citation_search_springer(ids), class = "scythe_auth_error")
    expect_length(server$urls(), 1)
  })
})

# --- xDD ----------------------------------------------------------------------

test_that("xDD responses are parsed (recorded from the live API)", {
  server <- fake_http(function(url, headers, n) {
    respond(body = fixture(if (n == 1) "xdd-hit.json" else "xdd-miss.json"))
  })
  with_fake_http(server, {
    res <- citation_search_xdd(c("10.18739/A29K97", "10.18739/A2D08X"))
    expect_equal(res$article_id, "10.1029/2018JG004528")
    expect_match(res$article_title, "^Ice Wedge Degradation")
    expect_match(server$urls()[1], "^https://xdd.wisc.edu/api/snippets\\?term=10.18739%2FA29K97&corpus=all$")
  })
})

test_that("an error in an xDD response skips the identifier", {
  server <- fake_http(function(url, headers, n) respond(body = list(error = list(message = "bad term"))))
  with_fake_http(server, {
    expect_warning(citation_search_xdd(ids[1]), "bad term", class = "scythe_request_failed")
  })
})

# --- PLOS ---------------------------------------------------------------------

test_that("PLOS sends one phrase query per identifier, for full articles only", {
  server <- fake_http(function(url, headers, n) respond(body = fixture("plos-hit.json")))
  with_fake_http(server, {
    res <- citation_search_plos("10.18739/A22274")
    expect_length(server$urls(), 1)
    expect_equal(server$urls(), paste0(
      "https://api.plos.org/search?q=%2210.18739%2FA22274%22&fl=id%2Ctitle",
      "&fq=doc_type%3Afull&rows=1000&wt=json"
    ))
    expect_equal(res$article_id, "10.1371/journal.pone.0213037")
    expect_equal(res$dataset_id, "10.18739/A22274")
  })
})

test_that("PLOS keeps to its per-minute limit and stops at its daily limit", {
  server <- fake_http(function(url, headers, n) respond(body = list(response = list(numFound = 0, docs = list()))))
  with_fake_http(server, {
    withr::local_options(scythe.plos.windows = list(c(60, 10), c(86400, 25)))
    expect_warning(res <- citation_search_plos(sprintf("10.18739/C%04d", 1:40)),
                   "PLOS has reached its request limit", class = "scythe_rate_limited")
    times <- as.numeric(vapply(server$requests, function(r) as.numeric(r$time), 1))
    expect_length(times, 25)
    # Never more than 10 requests in any minute.
    expect_true(all(vapply(times, function(t) sum(times >= t & times < t + 60), 1) <= 10))
    expect_equal(attr(res, "search_status")$pending, 15)
  })
})

# --- citation_search() ----------------------------------------------------------

test_that("citation_search() keeps going when one source is rate limited", {
  server <- fake_http(function(url, headers, n) {
    if (grepl("elsevier", url)) {
      respond(429, headers = list(`retry-after` = "86400"))
    } else if (grepl("xdd", url)) {
      xdd_handler(list("10.18739/A0001" = "10.1/found-by-xdd"))(url, headers, n)
    } else {
      respond(body = list(records = list()))
    }
  })
  with_fake_http(server, {
    expect_warning(res <- citation_search(ids[1:2], sources = c("scopus", "xdd", "springer")),
                   class = "scythe_rate_limited")
    expect_equal(res$article_id, "10.1/found-by-xdd")
  })
})
