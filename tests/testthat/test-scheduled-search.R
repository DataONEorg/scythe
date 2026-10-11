# Tests of the scheduled search's cycles and phases, with a fake clock, a fake
# OpenAlex, and fixed DOI lists in place of the DataONE indexes (see
# helper-schedule.R).

# The OpenAlex phases on their own; test-tracks.R tests the full-text tracks.
run <- function(dir, ...) {
  scheduled_search(c("urn:node:ARCTIC", "urn:node:KNB"), dir, text_sources = character(),
                   parallel = FALSE,
                   metrics_check_hours = 0, ...)
}

test_that("a first run does the priority phases in order, then the other nodes", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  with_fake_openalex(server, with_fake_dataone({
    status <- run(dir)
    expect_equal(status$ran, c("priority_graph", "priority_text", "others_graph"))
    expect_equal(read_state(file.path(dir, "state", "cycle.json"))$phase, "done")
    expect_equal(read_state(file.path(dir, "state", "others.json"))$status, "done")

    results <- file.path(dir, "results", "2026-10-01")
    expect_equal(pairs(file.path(results, "priority-graph.csv")),
                 c("10.1/a1 10.18739/p1", "10.1/a3 10.18739/p1"))
    expect_equal(pairs(file.path(results, "priority-text.csv")),
                 c("10.1/a2 10.5063/p2", "10.1/a3 10.18739/p1"))
    # Only the full-text citation the graph search missed.
    expect_equal(pairs(file.path(results, "priority-text-new.csv")), "10.1/a2 10.5063/p2")
    expect_equal(pairs(file.path(dir, "results", "others-2026-10-01", "others-graph.csv")),
                 c("10.1/b1 10.6085/o1", "10.1/b2 10.1594/pangaea.1"))

    t <- total(dir)
    expect_equal(nrow(t), 5)
    a3 <- t[t$article_id == "10.1/a3", ]
    expect_equal(a3$sources, "openalex-graph;openalex-text")
    expect_equal(a3$first_phase, "priority_graph")
    expect_equal(t$node[t$article_id == "10.1/a2"], "urn:node:KNB")
    expect_equal(t$first_cycle[t$article_id == "10.1/b1"], "others-2026-10-01")
  }))
})

test_that("the other nodes' DOIs put the last nodes at the end", {
  dir <- withr::local_tempdir()
  with_fake_openalex(fake_openalex(works), with_fake_dataone({
    run(dir)
    saved <- read_doi_list(file.path(dir, "state", "dois", "others-2026-10-01.csv"))
    expect_equal(saved$node, c("urn:node:PISCO", "urn:node:PANGAEA"))
  }))
})

test_that("a run with nothing due sends no requests", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  with_fake_openalex(server, with_fake_dataone({
    run(dir)
    sent <- length(server$urls)
    server$clock$now <- server$clock$now + 3600
    status <- run(dir)
    expect_length(server$urls, sent)
    expect_length(status$ran, 0)
    expect_equal(collected$priority, 1)
  }))
})

test_that("the next cycle starts on the next cycle day, searching recent works in full text", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  with_fake_openalex(server, with_fake_dataone({
    run(dir)
    server$clock$now <- as.POSIXct("2026-10-14 23:00:00", tz = "UTC")
    expect_length(run(dir)$ran, 0)
    sent <- length(server$urls)
    server$clock$now <- as.POSIXct("2026-10-15 00:05:00", tz = "UTC")
    status <- run(dir)
    expect_equal(status$cycle, "2026-10-15")
    # The other nodes' pass isn't due again until 30 days after the last.
    expect_equal(status$ran, c("priority_graph", "priority_text"))
    text <- grep("fulltext.search", server$urls[-seq_len(sent)], value = TRUE, fixed = TRUE)
    expect_true(length(text) > 0)
    # 90 days before the start of the last cycle that completed (2026-10-01).
    expect_true(all(grepl("from_publication_date%3A2026-07-03", text, fixed = TRUE)))
    expect_true(file.exists(file.path(dir, "results", "2026-10-15", "priority-text-new.csv")))
  }))
})

test_that("a cycle stopped by OpenAlex's rate limit resumes after the reset", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  rate_limit_once(server, "filter=doi")
  with_fake_openalex(server, with_fake_dataone({
    expect_warning(status <- run(dir), "rate limited until")
    expect_equal(status$phase, "priority_graph")
    expect_equal(status$ran, "priority_graph")
    expect_false(file.exists(file.path(dir, "results", "2026-10-01", "priority-graph.csv")))

    # Within the pause, a run sends nothing.
    server$clock$now <- server$clock$now + 3600
    sent <- length(server$urls)
    run(dir)
    expect_length(server$urls, sent)

    # After it, the cycle continues with the same DOIs.
    server$clock$now <- server$clock$now + 3700
    status <- run(dir)
    expect_equal(status$phase, "done")
    expect_equal(collected$priority, 1)
    expect_equal(nrow(total(dir)), 5)
  }))
})

test_that("a new cycle pauses the other nodes' pass until its OpenAlex phases are done", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  # The pass over the other nodes is rate-limited on its first request.
  rate_limit_once(server, "10.6085%2Fo1", seconds = 20 * 86400)
  with_fake_openalex(server, with_fake_dataone({
    expect_warning(status <- run(dir), "rate limited until")
    expect_equal(status$others_status, "running")

    # On the 15th the recorded pause is still in force: the new cycle starts
    # but sends no requests, and the pass stays where it was.
    server$clock$now <- as.POSIXct("2026-10-15 01:00:00", tz = "UTC")
    sent <- length(server$urls)
    status <- run(dir)
    expect_length(server$urls, sent)
    expect_equal(status$cycle, "2026-10-15")
    expect_equal(status$phase, "priority_graph")
    expect_false("others_graph" %in% status$ran)

    # After the pause, the cycle's phases run first, then the pass resumes
    # (it isn't restarted: its DOIs aren't collected again).
    server$clock$now <- as.POSIXct("2026-10-21 13:00:00", tz = "UTC")
    status <- run(dir)
    expect_equal(status$ran, c("priority_graph", "priority_text", "others_graph"))
    expect_equal(status$others, "2026-10-01")
    expect_equal(status$others_status, "done")
    expect_equal(collected$others, 1)
  }))
})

test_that("a cycle that finishes after the next cycle day is followed straight away", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  rate_limit_once(server, "filter=doi", seconds = 17 * 86400)
  with_fake_openalex(server, with_fake_dataone({
    expect_warning(run(dir), "rate limited until")
    server$clock$now <- as.POSIXct("2026-10-18 13:00:00", tz = "UTC")
    expect_equal(run(dir)$phase, "done")
    expect_equal(read_state(file.path(dir, "state", "cycle.json"))$id, "2026-10-01")
    # The next run starts the cycle that was due on the 15th.
    expect_equal(run(dir)$cycle, "2026-10-18")
  }))
})

test_that("a new pass over the other nodes starts once the last is 30 days old", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  with_fake_openalex(server, with_fake_dataone({
    run(dir)
    # 29 days later: the cycle due on the 15th runs, but no new pass.
    server$clock$now <- as.POSIXct("2026-10-30 12:00:00", tz = "UTC")
    status <- run(dir)
    expect_equal(status$ran, c("priority_graph", "priority_text"))
    server$clock$now <- as.POSIXct("2026-10-31 12:00:00", tz = "UTC")
    status <- run(dir)
    expect_equal(status$ran, "others_graph")
    expect_equal(status$others, "2026-10-31")
    expect_equal(collected$others, 2)
  }))
})

test_that("a failure to collect DOIs is retried on the next run", {
  dir <- withr::local_tempdir()
  fail <- TRUE
  with_fake_openalex(fake_openalex(works), {
    local_mocked_bindings(collect_dataone_dois = function(priority_nodes = character(), ...) {
      if (fail) stop("CN unavailable")
      priority_dois
    })
    expect_message(status <- run(dir, other_nodes = NULL), "Couldn't collect the priority nodes' DOIs",
                   class = "message")
    expect_null(read_state(file.path(dir, "state", "cycle.json")))
    fail <- FALSE
    expect_equal(run(dir, other_nodes = NULL)$phase, "done")
  })
})

test_that("cycle days are found in this month or the last", {
  d <- as.Date
  expect_equal(latest_cycle_day(d("2026-10-01"), c(1, 15)), d("2026-10-01"))
  expect_equal(latest_cycle_day(d("2026-10-14"), c(1, 15)), d("2026-10-01"))
  expect_equal(latest_cycle_day(d("2026-10-31"), c(1, 15)), d("2026-10-15"))
  expect_equal(latest_cycle_day(d("2026-11-10"), 15), d("2026-10-15"))
  # A day that a month doesn't have is skipped.
  expect_equal(latest_cycle_day(d("2026-03-05"), c(1, 30)), d("2026-03-01"))
  expect_equal(latest_cycle_day(d("2026-03-05"), 30), d("2026-01-30"))
})

test_that("a run's OpenAlex searches share one credit allowance", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  with_fake_openalex(server, with_fake_dataone({
    # The graph search costs 2 credits, leaving the full-text search 19: its
    # 10-credit screening search fits, but not the next one. (With its own
    # 21 credits, it would spend 30 and the run 32.)
    expect_warning(status <- run(dir, max_credits = 21), "Stopped after spending")
    expect_equal(status$ran, c("priority_graph", "priority_text"))
    expect_equal(status$openalex$credits_used, 12)
    expect_equal(status$phase, "priority_text")
    # The next run continues with a new allowance.
    server$clock$now <- server$clock$now + 3600
    status <- run(dir, max_credits = 100)
    expect_equal(status$phase, "done")
  }))
})

test_that("once a run's credits are spent, the other searches wait for the next run", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(works)
  with_fake_openalex(server, with_fake_dataone({
    # Exactly enough for the priority searches (2 + 30 credits).
    messages <- capture_messages(status <- run(dir, max_credits = 32))
    expect_equal(status$ran, c("priority_graph", "priority_text"))
    expect_equal(status$openalex$credits_used, 32)
    expect_true(any(grepl("This run's OpenAlex credits \\(32\\) are spent; others_graph continues", messages)))
    # The pass over the other nodes is started (its DOIs collected), but its
    # search waits.
    expect_equal(status$others_status, "running")
    server$clock$now <- server$clock$now + 3600
    status <- run(dir, max_credits = 32)
    expect_equal(status$ran, "others_graph")
    expect_equal(status$others_status, "done")
  }))
})
