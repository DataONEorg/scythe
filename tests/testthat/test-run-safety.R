# Tests that the scheduled search's results survive concurrent processes and
# crashes: atomic writes, re-adding a finished pass, stopping hung tracks, the
# run lock, and cleaning up temporary files.

nodes <- c("urn:node:ARCTIC", "urn:node:KNB")

run_all <- function(dir, ...) {
  scheduled_search(nodes, dir, other_nodes = NULL, parallel = FALSE,
                   metrics_check_hours = 0, ...)
}

test_that("a failed write leaves the previous file intact", {
  path <- file.path(withr::local_tempdir(), "results", "citations.csv")
  write_csv_atomic(data.frame(a = "old"), path)
  # A list column can't be written as CSV, so the write fails part-way.
  bad <- data.frame(a = "new")
  bad$b <- list(list(1, 2))
  expect_error(write_csv_atomic(bad, path))
  expect_equal(read.csv(path)$a, "old")
})

test_that("results files are replaced whole, never written in place", {
  path <- file.path(withr::local_tempdir(), "priority-xdd.csv")
  written <- character()
  local_mocked_bindings(write_csv_atomic = function(df, path) {
    written <<- c(written, path)
    invisible(path)
  })
  write_citations(data.frame(article_id = "a", dataset_id = "d"), path)
  write_doi_list(priority_dois, path)
  expect_equal(written, c(path, path))
})

test_that("the running total isn't rewritten when nothing is new", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "citations.csv")
  found <- data.frame(article_id = "10.1/a", article_title = "A", dataset_id = "10.5/d",
                      target_id = "doi:10.5/D", target_type = "pid", node = "urn:node:KNB")
  add_to_total(found, path, "xdd", "priority_text", "2026-10-01", "2026-10-01T12:00:00Z")
  writes <- 0
  local_mocked_bindings(write_csv_atomic = function(df, path) writes <<- writes + 1)
  add_to_total(found, path, "xdd", "priority_text", "2026-10-01", "2026-10-01T13:00:00Z")
  expect_equal(writes, 0)
  # A new source for a known pair is recorded.
  add_to_total(found, path, "plos", "priority_text", "2026-10-01", "2026-10-01T13:00:00Z")
  expect_equal(writes, 1)
})

test_that("a pass finished by a run that died before adding its citations is added on the next run", {
  dir <- withr::local_tempdir()
  server <- fake_services(works, list(xdd = function(doi, k) found_by("xdd", "10.1/x")))
  with_fake_http(server, with_fake_dataone({
    run_all(dir, text_sources = character())
    # The xDD track finishes its pass, but the run dies before adding its
    # citations to the total.
    run_track("xdd", dir, nodes)
    expect_equal(read_state(file.path(dir, "state", "tracks", "xdd.json"))$status, "done")
    expect_false("10.1/x" %in% total(dir)$article_id)

    run_all(dir, text_sources = "xdd")
    t <- total(dir)
    expect_equal(sort(t$dataset_id[t$article_id == "10.1/x"]), c("10.18739/p1", "10.5063/p2"))
    expect_equal(unique(t$sources[t$article_id == "10.1/x"]), "xdd")
  }))
})

test_that("a track still running after its time limit is stopped and reported", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  hung <- callr::r_bg(function() Sys.sleep(120))
  h <- list(sources = "xdd", dir = dir, priority_nodes = nodes, deadline = Sys.time() + 1,
            procs = list(hung))
  started <- Sys.time()
  results <- finish_tracks(h, grace = 2)
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 30)
  expect_false(hung$is_alive())
  track <- results$xdd$track
  expect_match(track$last_error, "still running 0 minutes after its time limit, so it was stopped")
  expect_false(is.null(track$crashed))
})

test_that("only one run at a time works in a directory", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, with_fake_dataone({
    # Another process holds a fresh lock.
    write_state(list(host = "other-host", pid = 12345, started = format_utc(http_now() - 600)),
                file.path(dir, "state", "run.lock", "info.json"))
    messages <- capture_messages(status <- run_all(dir))
    expect_true(status$locked)
    expect_length(server$urls(), 0)
    expect_true(any(grepl("Another run \\(other-host process 12345, started 2026-10-01T11:50:00Z\\) is in progress",
                          messages)))
    expect_true(dir.exists(run_lock_path(dir)))
  }))
})

test_that("a lock left by a run that died is taken over", {
  dir <- withr::local_tempdir()
  server <- fake_services(works)
  with_fake_http(server, with_fake_dataone({
    write_state(list(host = "other-host", pid = 12345, started = format_utc(http_now() - 25 * 3600)),
                file.path(dir, "state", "run.lock", "info.json"))
    messages <- capture_messages(status <- run_all(dir, text_sources = character()))
    expect_null(status$locked)
    expect_equal(status$phase, "done")
    expect_true(any(grepl("Taking over the lock left by a run that didn't finish", messages)))
    # Released at the end of the run.
    expect_false(dir.exists(run_lock_path(dir)))
  }))
})

test_that("a run can't take a lock it already holds, and releases only its own", {
  dir <- withr::local_tempdir()
  with_fake_http(fake_http(), {
    expect_true(acquire_run_lock(dir))
    expect_false(suppressMessages(acquire_run_lock(dir)))
    release_run_lock(dir)
    expect_false(dir.exists(run_lock_path(dir)))
    write_state(list(host = "other-host", pid = 12345, started = format_utc(http_now())),
                file.path(run_lock_path(dir), "info.json"))
    release_run_lock(dir)
    expect_true(dir.exists(run_lock_path(dir)))
  })
})

test_that("temporary files left by a run that died are removed", {
  dir <- withr::local_tempdir()
  left <- c(file.path(dir, "results", "citations.csv.tmp4242"),
            file.path(dir, "state", "tracks", "plos.json.tmp4242"))
  for (f in left) {
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    writeLines("partial", f)
  }
  with_fake_http(fake_services(works), with_fake_dataone({
    run_all(dir, text_sources = character())
  }))
  expect_false(any(file.exists(left)))
})
