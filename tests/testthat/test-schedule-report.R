# Tests of the scheduled search's reporting: summary lines, alerts, and the
# status summary, against fake services (see helper-schedule.R).

nodes <- c("urn:node:ARCTIC", "urn:node:KNB")

run_all <- function(dir, ...) {
  scheduled_search(nodes, dir, other_nodes = NULL, parallel = FALSE,
                   metrics_check_hours = 0, ...)
}

# The messages of a run that start with "ACTION NEEDED" or "RESOLVED".
alert_messages <- function(messages) {
  grep("^(ACTION NEEDED|RESOLVED)", trimws(messages), value = TRUE)
}

alerts_log <- function(dir) readLines(file.path(dir, "state", "alerts.log"))
status_md <- function(dir) paste(readLines(file.path(dir, "state", "status.md")), collapse = "\n")

test_that("every run logs a summary line for OpenAlex and each track", {
  dir <- withr::local_tempdir()
  server <- fake_services(works, list(xdd = function(doi, k) found_by("xdd", "10.1/x")))
  with_fake_http(server, with_fake_dataone({
    withr::local_options(scythe.plos.windows = list(c(86400, 1)))
    messages <- trimws(capture_messages(suppressWarnings(run_all(dir))))
    expect_true(any(grepl("^OpenAlex: cycle 2026-10-01, priority phases done; [0-9]+ credits this run", messages)))
    expect_true("xDD: done with cycle 2026-10-01's DOIs; 2 citations" %in% messages)
    expect_true(any(grepl(paste0("^PLOS: on cycle 2026-10-01's DOIs; KNB 0/1, overall 1/2; 0 citations; ",
                                 "paused \\(rate limited\\) until 2026-10-02T12:00:0[0-9]Z$"), messages)))
  }))
})

test_that("a missing key is alerted once, and resolved when it's set", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, keys = list(scopus = "test-key"), with_fake_dataone({
    first <- alert_messages(capture_messages(run_all(dir, text_sources = "springer")))
    expect_equal(first, paste0(
      "ACTION NEEDED: No API key is set for Springer, so the Springer track isn't running. ",
      "Add the 'springer' key to the api-keys Kubernetes secret (or set it with scythe_set_key())."))
    # Still open an hour later, but not logged again.
    server$clock$now <- server$clock$now + 3600
    expect_length(alert_messages(capture_messages(run_all(dir, text_sources = "springer"))), 0)
    expect_match(status_md(dir), "ACTION NEEDED\\*\\* \\(since 2026-10-01T12:00:00Z\\): No API key is set for Springer")
  }))
  with_fake_http(server, with_fake_dataone({
    resolved <- alert_messages(capture_messages(run_all(dir, text_sources = "springer")))
    expect_match(resolved, "^RESOLVED: No API key is set for Springer")
    log <- alerts_log(dir)
    expect_length(log, 2)
    expect_match(log[1], "ACTION NEEDED \\[key:springer\\]")
    expect_match(log[2], "RESOLVED \\[key:springer\\]")
    expect_match(status_md(dir), "## Alerts\n\nNone.")
  }))
})

test_that("a rejected key is alerted, naming the service's message", {
  dir <- withr::local_tempdir()
  server <- fake_services(works, list(
    scopus = function(doi, k) respond(401, body = fixture("scopus-auth.json"))))
  with_fake_http(server, with_fake_dataone({
    alerts <- alert_messages(capture_messages(suppressWarnings(run_all(dir, text_sources = "scopus"))))
    expect_match(alerts, paste0("^ACTION NEEDED: Scopus rejected the API key: Scopus rejected the request ",
                                "\\(HTTP 401: Invalid API Key\\).*Fix the 'scopus' key in the api-keys ",
                                "Kubernetes secret; the Scopus track is stopped until then\\.$"))
  }))
})

test_that("a track paused for more than 3 days is alerted", {
  dir <- withr::local_tempdir()
  reset <- as.numeric(as.POSIXct("2026-10-07 00:00:00", tz = "UTC"))
  server <- fake_services(works, list(scopus = function(doi, k) {
    r <- found_by("scopus", character())
    r$headers <- list(`x-ratelimit-remaining` = "0", `x-ratelimit-reset` = as.character(reset))
    r
  }))
  with_fake_http(server, with_fake_dataone({
    suppressWarnings(run_all(dir, text_sources = "scopus"))
    server$clock$now <- as.POSIXct("2026-10-04 11:00:00", tz = "UTC")
    expect_length(alert_messages(capture_messages(run_all(dir, text_sources = "scopus"))), 0)
    server$clock$now <- as.POSIXct("2026-10-04 13:00:00", tz = "UTC")
    alerts <- alert_messages(capture_messages(run_all(dir, text_sources = "scopus")))
    expect_match(alerts, paste0("^ACTION NEEDED: Scopus has been paused since 2026-10-01T12:00:00Z ",
                                "\\(now until 2026-10-07T00:00:00Z\\) because it is rate limited\\. ",
                                "Check the service's status, and the key's quota\\.$"))
  }))
})

test_that("a pass that spans two cycles is alerted", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, with_fake_dataone({
    # PLOS may send one request every 60 days.
    withr::local_options(scythe.plos.windows = list(c(60 * 86400, 1)))
    suppressWarnings(run_all(dir, text_sources = "plos"))
    # (It is also alerted as paused for more than 3 days, waiting for its limit.)
    still_on_pass <- function(messages) grep("still on its pass", alert_messages(messages), value = TRUE)
    server$clock$now <- as.POSIXct("2026-10-15 01:00:00", tz = "UTC")
    expect_length(still_on_pass(capture_messages(run_all(dir, text_sources = "plos"))), 0)
    server$clock$now <- as.POSIXct("2026-11-01 01:00:00", tz = "UTC")
    alerts <- still_on_pass(capture_messages(run_all(dir, text_sources = "plos")))
    expect_match(alerts, paste0("^ACTION NEEDED: PLOS is still on its pass over cycle 2026-10-01's DOIs, ",
                                "2 cycles later \\(1 of 2 searched\\)\\. Its rate limits are slower than ",
                                "the cycle: consider fewer priority nodes, less frequent cycles, or leaving ",
                                "plos out of the full-text sources\\.$"))
  }))
})

test_that("failing to collect DOIs is alerted, and resolved once collection works", {
  dir <- withr::local_tempdir()
  fail <- TRUE
  with_fake_http(fake_services(works), {
    local_mocked_bindings(collect_dataone_dois = function(...) {
      if (fail) stop("CN unavailable") else priority_dois
    })
    alerts <- alert_messages(capture_messages(run_all(dir, text_sources = character())))
    expect_match(alerts, paste0("^ACTION NEEDED: Collecting the priority nodes' DOIs failed ",
                                "\\(CN unavailable\\), so the new cycle hasn't started\\. It is retried on ",
                                "each run; check the DataONE CN and member nodes\\.$"))
    fail <- FALSE
    # (Captured first: expect_match() evaluates its argument twice.)
    resolved <- alert_messages(capture_messages(run_all(dir, text_sources = character())))
    expect_match(resolved, "^RESOLVED: Collecting the priority nodes' DOIs failed")
  })
})

test_that("a cycle with far fewer DOIs than the last is alerted", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, {
    many <- rbind(priority_dois, doi_rows(c("10.18739/p3", "10.18739/p4"), "urn:node:ARCTIC", "priority"))
    dois <- many
    local_mocked_bindings(collect_dataone_dois = function(...) dois)
    run_all(dir, text_sources = character())
    dois <- priority_dois[1, ]
    server$clock$now <- as.POSIXct("2026-10-15 01:00:00", tz = "UTC")
    alerts <- alert_messages(capture_messages(run_all(dir, text_sources = character())))
    expect_match(alerts, paste0("^ACTION NEEDED: Cycle 2026-10-15 collected 1 priority DOIs, far fewer ",
                                "than the 4 of the previous cycle\\."))
  })
})

test_that("a track that fails is alerted", {
  dir <- withr::local_tempdir()
  with_fake_http(fake_services(works), with_fake_dataone({
    local_mocked_bindings(track_search = function(source) function(...) stop("unexpected response"))
    alerts <- alert_messages(capture_messages(run_all(dir, text_sources = "xdd")))
    expect_match(alerts, paste0("^ACTION NEEDED: The xDD track failed at 2026-10-01T12:00:00Z: the track ",
                                "failed: unexpected response\\. Check the job log; the track is retried on ",
                                "each run\\.$"))
  }))
})

test_that("a slow first full pass of the OpenAlex full-text search is alerted", {
  text <- list(full = TRUE, total = 900, done = 20, elapsed_days = 1.5, projected_days = 67.5)
  alerts <- current_alerts(list(id = "2026-10-01", number = 1), list(), text, list(), Sys.time())
  expect_equal(alerts[["openalex-text-slow"]], paste0(
    "The OpenAlex full-text search's first full pass over cycle 2026-10-01 is projected to take ",
    "68 days (20 of 900 batches done in 1.5 days). Consider a larger OpenAlex budget or fewer ",
    "priority nodes."))
  # Not for a pass that searches only recent works.
  text$full <- FALSE
  expect_length(current_alerts(list(id = "x", number = 1), list(), text, list(), Sys.time()), 0)
})

test_that("the OpenAlex full-text progress projects the length of a pass", {
  dois <- doi_rows(sprintf("10.1/d%04d", 1:250), "urn:node:ARCTIC", "priority")
  dir <- withr::local_tempdir()
  write_checkpoint(dir, "text all a|b", empty_pairs())
  p <- text_progress(dois, dir, TRUE, "2026-10-01T00:00:00Z", as.POSIXct("2026-10-03 00:00:00", tz = "UTC"))
  expect_equal(p$total, 3)
  expect_equal(p$done, 1)
  expect_equal(p$projected_days, 6)
})

test_that("the status summary describes the cycle, tracks, nodes and citations", {
  dir <- withr::local_tempdir()
  server <- fake_services(works, list(xdd = function(doi, k) found_by("xdd", "10.1/x")))
  with_fake_http(server, with_fake_dataone({
    local_mocked_bindings(collect_dataone_dois = function(...) {
      d <- priority_dois
      attr(d, "doi_sources") <- data.frame(node = c("urn:node:ARCTIC", "urn:node:KNB"), group = "priority",
                                           index = c("member", "cn"), cn_count = c(1, 1), records = 1,
                                           dois = 1, excluded = c(0, 3), test_dois = c(0, 1))
      d
    })
    run_all(dir)
    status <- status_md(dir)
    expect_match(status, "## Alerts\n\nNone.", fixed = TRUE)
    expect_match(status, "Cycle 2026-10-01 (number 1), started 2026-10-01T12:00:00Z", fixed = TRUE)
    expect_match(status, "- xDD: done with cycle 2026-10-01's DOIs; 2 citations", fixed = TRUE)
    expect_match(status, "| KNB | 1 | CN (fallback) | 4 | 1 |", fixed = TRUE)
    expect_match(status, "No pass over the other nodes has started yet.", fixed = TRUE)
    expect_match(status, "5 in total (results/citations.csv): openalex-graph 2, ", fixed = TRUE)
  }))
})
