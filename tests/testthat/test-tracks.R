# Tests of the full-text tracks (xDD, Springer, Scopus, PLOS) in the scheduled
# search, run one after another in this process against fake services (see
# helper-schedule.R). The tracks' parallel processes are exercised by the live
# check described in tools/README.md.

nodes <- c("urn:node:ARCTIC", "urn:node:KNB")

run_all <- function(dir, ...) {
  scheduled_search(nodes, dir, other_nodes = NULL, parallel = FALSE,
                   metrics_check_hours = 0, ...)
}

track <- function(dir, source) read_state(file.path(dir, "state", "tracks", paste0(source, ".json")))

test_that("the tracks search every priority DOI after the graph search, and their citations are recorded", {
  dir <- withr::local_tempdir()
  server <- fake_services(works, list(
    xdd = function(doi, k) found_by("xdd", if (doi == "10.18739/p1") "10.1/x1" else character()),
    plos = function(doi, k) found_by("plos", if (doi == "10.5063/p2") "10.1/a2" else character())
  ))
  with_fake_http(server, with_fake_dataone({
    status <- run_all(dir)
    for (source in track_sources) {
      expect_equal(track(dir, source)$status, "done")
      expect_equal(track(dir, source)$pass, "2026-10-01")
    }
    expect_setequal(requested(server, "xdd.wisc.edu"), c("10.18739/p1", "10.5063/p2"))
    results <- file.path(dir, "results", "2026-10-01")
    expect_equal(pairs(file.path(results, "priority-xdd.csv")), "10.1/x1 10.18739/p1")
    # Text citations the graph search didn't find, from any full-text search.
    expect_equal(pairs(file.path(results, "priority-text-new.csv")),
                 c("10.1/a2 10.5063/p2", "10.1/x1 10.18739/p1"))
    t <- total(dir)
    expect_equal(t$sources[t$article_id == "10.1/a2"], "openalex-text;plos")
    expect_equal(t$sources[t$article_id == "10.1/x1"], "xdd")
  }))
})

test_that("the tracks don't start until the cycle's graph search is done", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  rate_limit_first(server, "filter=doi")
  with_fake_http(server, with_fake_dataone({
    expect_warning(run_all(dir), "rate limited until")
    expect_length(requested(server, "xdd.wisc.edu"), 0)
    expect_null(track(dir, "xdd"))
  }))
})

test_that("tracks search in node priority order", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, with_fake_dataone({
    # Collected with KNB first, but ARCTIC is the higher priority.
    local_mocked_bindings(collect_dataone_dois = function(...) priority_dois[2:1, ])
    run_all(dir, text_sources = "xdd")
    expect_equal(requested(server, "xdd.wisc.edu"), c("10.18739/p1", "10.5063/p2"))
  }))
})

test_that("with one source rate limited and another failing, the others finish, and the limited one resumes after its reset", {
  dir <- withr::local_tempdir()
  server <- fake_services(works, list(
    xdd = function(doi, k) {
      if (k == 1) respond(429, headers = list(`retry-after` = "7200")) else found_by("xdd", "10.1/x")
    },
    springer = function(doi, k) respond(500)
  ))
  with_fake_http(server, with_fake_dataone({
    status <- suppressWarnings(run_all(dir))
    expect_equal(track(dir, "plos")$status, "done")
    expect_equal(track(dir, "scopus")$status, "done")
    # Springer's DOIs failed (after retries), but its pass completed.
    expect_equal(track(dir, "springer")$status, "done")
    expect_equal(track(dir, "springer")$failed, 2)
    xdd <- track(dir, "xdd")
    expect_equal(xdd$status, "running")
    expect_equal(xdd$stopped, "rate_limited")
    expect_equal(xdd$paused_until, "2026-10-01T14:00:00Z")

    # An hour later it's still paused, and sends nothing.
    server$clock$now <- as.POSIXct("2026-10-01 13:30:00", tz = "UTC")
    run_all(dir)
    expect_length(requested(server, "xdd.wisc.edu"), 1)

    # After the reset it finishes, searching each DOI once more at most.
    server$clock$now <- as.POSIXct("2026-10-01 14:30:00", tz = "UTC")
    run_all(dir)
    expect_equal(track(dir, "xdd")$status, "done")
    expect_equal(requested(server, "xdd.wisc.edu"), c("10.18739/p1", "10.18739/p1", "10.5063/p2"))
  }))
})

test_that("a slow track finishes its pass after the next cycle starts, then searches the new list", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, with_fake_dataone({
    # PLOS may send one request a day.
    withr::local_options(scythe.plos.windows = list(c(86400, 1)))
    suppressWarnings(run_all(dir))
    expect_equal(track(dir, "plos")$status, "running")
    expect_equal(requested(server, "api.plos.org"), "10.18739/p1")

    # The next cycle starts; PLOS first finishes cycle 2026-10-01's DOIs.
    server$clock$now <- as.POSIXct("2026-10-15 00:30:00", tz = "UTC")
    status <- suppressWarnings(run_all(dir))
    expect_equal(status$cycle, "2026-10-15")
    plos <- track(dir, "plos")
    expect_equal(plos$pass, "2026-10-01")
    expect_equal(plos$status, "done")
    expect_equal(requested(server, "api.plos.org"), c("10.18739/p1", "10.5063/p2"))

    # A day later it starts the new cycle's list.
    server$clock$now <- as.POSIXct("2026-10-16 01:00:00", tz = "UTC")
    suppressWarnings(run_all(dir))
    expect_equal(track(dir, "plos")$pass, "2026-10-15")
    expect_equal(requested(server, "api.plos.org"), c("10.18739/p1", "10.5063/p2", "10.18739/p1"))
  }))
})

test_that("a track stops at its time limit and continues on the next run", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, with_fake_dataone({
    # PLOS waits 6 s between requests; with a 0.1-minute limit, one request fits.
    run_all(dir, text_sources = "plos", track_minutes = 0.1)
    plos <- track(dir, "plos")
    expect_equal(plos$status, "running")
    expect_equal(plos$stopped, "deadline")
    expect_equal(plos$progress$searched, 1)
    server$clock$now <- server$clock$now + 3600
    run_all(dir, text_sources = "plos")
    expect_equal(track(dir, "plos")$status, "done")
    expect_equal(requested(server, "api.plos.org"), c("10.18739/p1", "10.5063/p2"))
  }))
})

test_that("a track without its API key waits, and continues once the key is set", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, keys = list(scopus = "test-key"), with_fake_dataone({
    run_all(dir, text_sources = "springer")
    expect_equal(track(dir, "springer")$stopped, "no_key")
    expect_length(requested(server, "springernature"), 0)
  }))
  with_fake_http(server, with_fake_dataone({
    run_all(dir, text_sources = "springer")
    expect_equal(track(dir, "springer")$status, "done")
  }))
})

test_that("every so many cycles, the OpenAlex full-text search searches all works", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, with_fake_dataone({
    run_all(dir, text_sources = character(), full_text_every_cycles = 2)
    server$clock$now <- as.POSIXct("2026-10-15 01:00:00", tz = "UTC")
    n <- length(server$urls())
    run_all(dir, text_sources = character(), full_text_every_cycles = 2)
    text <- grep("fulltext.search", server$urls()[-seq_len(n)], value = TRUE, fixed = TRUE)
    # Cycle 2 of every 2: no date filter.
    expect_true(length(text) > 0)
    expect_false(any(grepl("from_publication_date", text, fixed = TRUE)))
  }))
})
