# Integration tests of submitting citations, against a development metrics
# service. They post citations, so they're skipped unless a developer sets
# SCYTHE_TEST_CITATIONS_URL (see skip_unless_citations_server() and
# CONTRIBUTING.md), and they refuse DataONE's production service. Several
# document the service's behaviour at metrics-service c16bf23 (S1-S4 in
# DataONEorg/scythe#49); if the service is fixed, those tests will fail and
# should be updated.
#
# Citations of real datasets come from inst/testdata/test-citations.csv.
# Others use a synthetic target identifier unique to each run, so a test's
# citations can't already be in the service's database.

test_that("live: a single citation is accepted and recorded", {
  token <- local_test_metrics_service()
  input <- data.frame(source_id = "10.1371/journal.pone.0213037", target_id = "doi:10.18739/A22274")
  out <- submit_citations(input)
  expect_true(out$submit_status %in% c("registered", "queued"))
  expect_equal(times_recorded(input$source_id, input$target_id), 1)
})

test_that("live: several citations are sent one at a time, and all are recorded", {
  token <- local_test_metrics_service()
  input <- data.frame(source_id = c("10.1371/journal.pone.0199789", "10.1371/journal.pone.0162907"),
                      target_id = c("doi:10.5063/F1T151VR", "doi:10.5063/F13T9F5P"))
  out <- submit_citations(input)
  expect_equal(attr(out, "submit_status")$requests, 2)
  expect_true(all(out$submit_status %in% c("registered", "queued")))
  for (i in seq_len(nrow(input))) {
    expect_equal(times_recorded(input$source_id[i], input$target_id[i]), 1)
  }
})

test_that("live: a request with two citations fails with HTTP 500 (S1)", {
  token <- local_test_metrics_service()
  rows <- prepare_submissions(data.frame(source_id = c("10.1371/journal.pone.0090826", "10.1371/journal.pone.0213037"),
                                         target_id = c(synthetic_target("batch-1"), synthetic_target("batch-2"))),
                              "isCitedBy")
  body <- request_json(citation_request(rows, token_claims(token)$sub))
  res <- http_post(default_citations_url(), body, list(Authorization = paste("Bearer", token)))
  expect_equal(res$status, 500)
})

test_that("live: only the first of a citation's related identifiers is recorded (S2)", {
  token <- local_test_metrics_service()
  first <- synthetic_target("related-1")
  second <- synthetic_target("related-2")
  source <- "10.1371/journal.pone.0090826"
  request <- list(request_type = "dataset", submitter = token_claims(token)$sub,
                  citations = list(list(
                    source_id = source,
                    related_identifiers = list(list(identifier = first, relation_type = "isCitedBy"),
                                               list(identifier = second, relation_type = "isCitedBy")),
                    link_publication_date = format(Sys.Date()))))
  res <- http_post(default_citations_url(), request_json(request),
                   list(Authorization = paste("Bearer", token)))
  expect_equal(res$status, 202)
  expect_equal(times_recorded(source, first), 1)
  expect_equal(times_recorded(source, second), 0)
})

test_that("live: a pair sent again with the same date is recorded once (S4)", {
  token <- local_test_metrics_service()
  input <- data.frame(source_id = "10.1371/journal.pone.0213037", target_id = synthetic_target("again"),
                      link_publication_date = "2026-01-15")
  submit_citations(input)
  submit_citations(input)
  expect_equal(times_recorded(input$source_id, input$target_id), 1)
})

test_that("live: a citing work without authors in Crossref is accepted but never recorded (S3)", {
  token <- local_test_metrics_service()
  # A Crossref "Editorial Board" record, which has no authors.
  input <- data.frame(source_id = "10.1298/ptr.editor2603", target_id = synthetic_target("no-authors"))
  out <- submit_citations(input)
  expect_equal(out$submit_status, "registered")
  expect_equal(times_recorded(input$source_id, input$target_id), 0)
})

test_that("live: throughput, with requests back to back and a few at once", {
  token <- local_test_metrics_service()
  n <- as.integer(Sys.getenv("SCYTHE_TEST_THROUGHPUT_N", "20"))
  make <- function(label) {
    data.frame(source_id = "10.1371/journal.pone.0213037",
               target_id = vapply(seq_len(n), function(i) synthetic_target(paste0(label, i)), ""))
  }

  # Back to back, through submit_citations().
  start <- Sys.time()
  out <- submit_citations(make("seq"))
  sequential <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  expect_true(all(out$submit_status %in% c("registered", "queued")))

  # Several in flight at once.
  rows <- prepare_submissions(make("par"), "isCitedBy")
  pool <- curl::new_pool(total_con = 3, host_con = 3)
  statuses <- integer()
  for (i in seq_len(nrow(rows))) {
    handle <- curl::new_handle(post = TRUE, postfields = request_json(citation_request(rows[i, ], token_claims(token)$sub)))
    curl::handle_setheaders(handle, "Content-Type" = "application/json",
                            Authorization = paste("Bearer", token))
    curl::curl_fetch_multi(default_citations_url(), handle = handle, pool = pool,
                           done = function(res) statuses <<- c(statuses, res$status_code),
                           fail = function(msg) statuses <<- c(statuses, NA_integer_))
  }
  start <- Sys.time()
  curl::multi_run(pool = pool)
  concurrent <- as.numeric(difftime(Sys.time(), start, units = "secs"))
  expect_true(all(statuses == 202, na.rm = FALSE))

  message(sprintf(paste("Throughput for %d citations: back to back %.1f s (%.2f s each, %.1f a minute);",
                        "3 at once %.1f s (%.2f s each, %.1f a minute)"),
                  n, sequential, sequential / n, 60 * n / sequential,
                  concurrent, concurrent / n, 60 * n / concurrent))
})
