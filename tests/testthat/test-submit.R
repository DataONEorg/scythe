# Tests of submit_citations(), against a fake metrics service and a fake
# clock; none of them uses the network.

test_that("each citation is sent in its own request, in the service's format", {
  server <- fake_citations_service()
  out <- with_fake_http(server, submit_citations(submission_pairs()))
  posts <- server$posts()
  expect_length(posts, 2)
  expect_equal(unique(vapply(posts, `[[`, "", "url")), citations_url_default)
  expect_equal(posts[[1]]$headers$Authorization, paste("Bearer", test_token()))
  body <- posted_bodies(server)[[1]]
  expect_equal(body$request_type, "dataset")
  expect_equal(body$submitter, "http://orcid.org/0000-0000-0000-0001")
  expect_length(body$citations, 1)
  citation <- body$citations[[1]]
  expect_equal(citation$source_id, "10.1016/j.marenvres.2009.12.006")
  expect_equal(citation$related_identifiers,
               list(list(identifier = "doi:10.5063/F1JS9NCJ", relation_type = "isCitedBy")))
  expect_equal(citation$link_publication_date, "2026-09-15")
  # The target is the exact DataONE identifier, whatever it is.
  expect_equal(posted_bodies(server)[[2]]$citations[[1]]$related_identifiers[[1]]$identifier,
               "ess-dive-abc-123")
  expect_equal(out$submit_status, c("registered", "registered"))
  expect_equal(out$http_status, c(202L, 202L))
  expect_equal(out$message, c("Registered", "Registered"))
  expect_equal(out$submitted_at, rep("2026-10-01T12:00:00Z", 2))
})

test_that("the request keeps the arrays the service documents, for future batches", {
  rows <- prepare_submissions(submission_pairs(), "isCitedBy")
  req <- citation_request(rows, "me")
  expect_length(req$citations, 2)
  json <- jsonlite::fromJSON(request_json(citation_request(rows[1, ], "me")), simplifyVector = FALSE)
  expect_type(json$citations, "list")
  expect_type(json$citations[[1]]$related_identifiers, "list")
})

test_that("a CSV file such as results/to-submit.csv can be submitted as it is", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(data.frame(source_id = "10.1234/abc", target_id = "doi:10.5063/X",
                              target_type = "pid", node = "urn:node:KNB",
                              first_found = "2026-08-01T00:00:00Z"),
                   path, row.names = FALSE)
  server <- fake_citations_service()
  out <- with_fake_http(server, submit_citations(path))
  expect_equal(out$node, "urn:node:KNB")
  expect_equal(out$submit_status, "registered")
  expect_equal(posted_bodies(server)[[1]]$citations[[1]]$link_publication_date, "2026-08-01")
})

test_that("article_id works as the citing work's column, and dates default sensibly", {
  input <- data.frame(article_id = c("10.1234/a", "10.1234/b"), target_id = c("t1", "t2"),
                      link_publication_date = c("2025-01-02", NA), stringsAsFactors = FALSE)
  server <- fake_citations_service()
  with_fake_http(server, submit_citations(input))
  dates <- vapply(posted_bodies(server), function(b) b$citations[[1]]$link_publication_date, "")
  expect_equal(dates, c("2025-01-02", "2026-10-01"))
})

test_that("input without the required columns is refused", {
  expect_error(submit_citations(data.frame(source_id = "10.1234/a")), "target_id")
  expect_error(submit_citations(data.frame(target_id = "x")), "source_id")
  expect_error(submit_citations(list(a = 1)), "data frame")
  expect_error(submit_citations(tempfile(fileext = ".csv")), "There is no file")
})

test_that("the relation type can be set for the whole set or per row", {
  server <- fake_citations_service()
  input <- submission_pairs(relation_type = c(NA, "isCitedBy"))
  with_fake_http(server, submit_citations(input, relation_type = "isReferencedBy"))
  relations <- vapply(posted_bodies(server),
                      function(b) b$citations[[1]]$related_identifiers[[1]]$relation_type, "")
  expect_equal(relations, c("isReferencedBy", "isCitedBy"))
})

test_that("unknown relation types stop the call before anything is sent", {
  server <- fake_citations_service()
  expect_error(with_fake_http(server, submit_citations(submission_pairs(), relation_type = "cites")),
               "relation_type must be one of")
  expect_error(with_fake_http(server, submit_citations(submission_pairs(relation_type = c("isCitedBy", "uses")))),
               "Unknown relation_type \"uses\"")
  expect_length(server$posts(), 0)
})

test_that("each kind of answer from the service is classified", {
  network_error <- list(status = NA_integer_, headers = list(), body = NULL, error = "timed out")
  responses <- list(
    respond(202, list(message = "Registered", status_code = "202")),
    respond(202, list(message = "Successful", status_code = "202")),
    respond(500, list(message = "Not a valid relation type", status_code = "500")),
    respond(400, "{}"),
    respond(500, "<html>Internal Server Error</html>"),
    network_error
  )
  input <- data.frame(source_id = paste0("10.1234/", 1:6), target_id = paste0("t", 1:6))
  res <- collect_warnings(with_fake_http(fake_citations_service(responses),
                                         submit_citations(input)))
  expect_equal(res$value$submit_status,
               c("registered", "queued", "rejected", "rejected", "failed", "failed"))
  expect_equal(res$value$http_status, c(202L, 202L, 500L, 400L, 500L, NA))
  expect_equal(res$value$message[3], "Not a valid relation type")
  expect_match(res$value$message[6], "network error: timed out")
  expect_equal(res$warnings, "scythe_request_failed")
  status <- attr(res$value, "submit_status")
  expect_equal(status[c("registered", "queued", "rejected", "failed", "requests")],
               list(registered = 1L, queued = 1L, rejected = 2L, failed = 2L, requests = 6))
})

test_that("rows that can't be sent are skipped, and duplicates are sent once", {
  input <- data.frame(source_id = c("10.1234/a", "not-a-doi", "10.1234/c", "DOI:10.1234/A"),
                      target_id = c("t1", "t2", NA, "t1"), stringsAsFactors = FALSE)
  server <- fake_citations_service()
  out <- with_fake_http(server, submit_citations(input))
  expect_length(server$posts(), 1)
  expect_equal(out$submit_status, c("registered", "skipped", "skipped", "skipped"))
  expect_equal(out$message[2:4], c("the citing work's identifier isn't a DOI",
                                   "no DataONE identifier for the cited dataset",
                                   "the same citation as an earlier row"))
})

test_that("the ledger records each submission, and nothing in it is sent twice", {
  ledger <- withr::local_tempfile(fileext = ".csv")
  responses <- list(respond(202, list(message = "Registered")),
                    respond(500, "<html>oops</html>"))
  suppressWarnings(with_fake_http(fake_citations_service(responses),
                                  submit_citations(submission_pairs(), ledger = ledger)))
  known <- read_ledger(ledger)
  expect_equal(known$source_id, c("10.1016/j.marenvres.2009.12.006", "10.5194/essd-14-955-2022"))
  expect_equal(known$target_id, c("doi:10.5063/F1JS9NCJ", "ess-dive-abc-123"))
  expect_equal(known$status, c("registered", "failed"))
  expect_equal(known$attempts, c("1", "1"))
  expect_equal(known$link_publication_date, c("2026-09-15", "2026-09-20"))

  # A later call skips the registered pair, matching it however its DOIs are
  # written, and sends the failed one again.
  again <- submission_pairs()
  again$source_id[1] <- "10.1016/j.marenvres.2009.12.006"
  server <- fake_citations_service()
  out <- with_fake_http(server, submit_citations(again, ledger = ledger))
  expect_length(server$posts(), 1)
  expect_equal(out$submit_status, c("skipped", "registered"))
  expect_equal(out$message[1], "already submitted (registered)")
  known <- read_ledger(ledger)
  expect_equal(known$status, c("registered", "registered"))
  expect_equal(known$attempts, c("1", "2"))
  expect_equal(nrow(known), 2)
})

test_that("a dry run builds the requests and sends nothing, without needing a token", {
  server <- fake_http()  # answers no POSTs, so any request would fail the test
  log_dir <- withr::local_tempdir()
  out <- with_fake_http(server, submit_citations(submission_pairs(), submitter = "me",
                                                 dry_run = TRUE, log_dir = log_dir),
                        keys = list())
  expect_length(server$requests, 0)
  expect_equal(out$submit_status, c("dry_run", "dry_run"))
  requests <- lapply(attr(out, "requests"), jsonlite::fromJSON, simplifyVector = FALSE)
  expect_equal(requests[[1]]$submitter, "me")
  expect_equal(requests[[2]]$citations[[1]]$source_id, "10.5194/essd-14-955-2022")
  expect_true(file.exists(file.path(log_dir, "dry-run-2026-10-01.jsonl")))
})

test_that("a missing or expired token stops the call before anything is sent", {
  server <- fake_http()
  expect_error(with_fake_http(server, submit_citations(submission_pairs()), keys = list()),
               "No DataONE token was found", class = "scythe_auth_error")
  expect_error(with_fake_http(server, submit_citations(submission_pairs()),
                              keys = list(dataone_token = expired_token())),
               "expired at 2026-09-01", class = "scythe_auth_error")
  expect_length(server$requests, 0)
})

test_that("the submitter comes from the token, or must be given", {
  server <- fake_citations_service()
  with_fake_http(server, submit_citations(submission_pairs()[1, ]),
                 keys = list(dataone_token = test_token(sub = "CN=me")))
  expect_equal(posted_bodies(server)[[1]]$submitter, "CN=me")
  expect_error(with_fake_http(fake_http(), submit_citations(submission_pairs()),
                              keys = list(dataone_token = "not-a-jwt")),
               "submitter isn't known")
  server <- fake_citations_service()
  with_fake_http(server, submit_citations(submission_pairs()[1, ], submitter = "CN=you"),
                 keys = list(dataone_token = "not-a-jwt"))
  expect_equal(posted_bodies(server)[[1]]$submitter, "CN=you")
})

test_that("a rejected token stops sending, with a scythe_auth_error warning", {
  input <- data.frame(source_id = paste0("10.1234/", 1:3), target_id = paste0("t", 1:3))
  server <- fake_citations_service(list(respond(401, list(message = "Unauthorized"))))
  res <- collect_warnings(with_fake_http(server, submit_citations(input)))
  expect_length(server$posts(), 1)
  expect_equal(res$value$submit_status, c("failed", "not_sent", "not_sent"))
  expect_equal(res$value$message[2], "not sent: the service rejected the token")
  expect_true("scythe_auth_error" %in% res$warnings)
  expect_equal(attr(res$value, "submit_status")$stopped, "auth")
})

test_that("five failures in a row stop sending; the rest wait for a later call", {
  input <- data.frame(source_id = paste0("10.1234/", 1:8), target_id = paste0("t", 1:8))
  server <- fake_citations_service(list(respond(502, "Bad Gateway")))
  res <- collect_warnings(with_fake_http(server, submit_citations(input)))
  expect_length(server$posts(), 5)
  expect_equal(res$value$submit_status, c(rep("failed", 5), rep("not_sent", 3)))
  expect_setequal(res$warnings, c("scythe_rate_limited", "scythe_request_failed"))
  expect_equal(attr(res$value, "submit_status")$stopped, "failures")
})

test_that("requests go out back to back, waiting only when rate limited", {
  input <- data.frame(source_id = paste0("10.1234/", 1:3), target_id = paste0("t", 1:3))
  server <- fake_citations_service(list(
    respond(202, list(message = "Registered")),
    respond(429, "{}", headers = list(`retry-after` = "10")),
    respond(202, list(message = "Registered"))
  ))
  res <- collect_warnings(with_fake_http(server, submit_citations(input)))
  expect_equal(server$clock$sleeps, 10)
  expect_equal(res$value$submit_status, c("registered", "failed", "registered"))

  # A long rate limit stops the call.
  server <- fake_citations_service(list(respond(429, "{}", headers = list(`retry-after` = "3600"))))
  res <- collect_warnings(with_fake_http(server, submit_citations(input)))
  expect_length(server$posts(), 1)
  expect_equal(res$value$submit_status, c("failed", "not_sent", "not_sent"))
  expect_true("scythe_rate_limited" %in% res$warnings)
})

test_that("max limits the requests sent", {
  input <- data.frame(source_id = paste0("10.1234/", 1:4), target_id = paste0("t", 1:4))
  server <- fake_citations_service()
  out <- with_fake_http(server, submit_citations(input, max = 2))
  expect_length(server$posts(), 2)
  expect_equal(out$submit_status, c("registered", "registered", "not_sent", "not_sent"))
  expect_equal(attr(out, "submit_status")$stopped, "max")
})

test_that("the endpoint comes only from scythe.citations_url, with a warning if it differs from the read host", {
  expect_false("url" %in% names(formals(submit_citations)))
  withr::local_options(scythe.citations_url = "https://metrics-dev.example.org/citations")
  server <- fake_citations_service()
  res <- collect_warnings(with_fake_http(server, submit_citations(submission_pairs()[1, ])))
  expect_equal(server$posts()[[1]]$url, "https://metrics-dev.example.org/citations")
  expect_equal(res$warnings, "simpleWarning")

  withr::local_options(scythe.metrics_url = "https://metrics-dev.example.org/metrics")
  server <- fake_citations_service()
  res <- collect_warnings(with_fake_http(server, submit_citations(submission_pairs()[1, ])))
  expect_length(res$warnings, 0)
})

test_that("each request and answer is logged", {
  log_dir <- withr::local_tempdir()
  with_fake_http(fake_citations_service(),
                 submit_citations(submission_pairs(), log_dir = log_dir))
  lines <- readLines(file.path(log_dir, "2026-10-01.jsonl"))
  expect_length(lines, 2)
  entry <- jsonlite::fromJSON(lines[1], simplifyVector = FALSE)
  expect_equal(entry$submit_status, "registered")
  expect_equal(entry$http_status, 202)
  expect_equal(entry$request$citations[[1]]$source_id, "10.1016/j.marenvres.2009.12.006")
})

test_that("the token can't be passed in; it's read from the environment or the keyring", {
  expect_false("token" %in% names(formals(submit_citations)))
  expect_error(submit_citations(submission_pairs(), token = test_token()), "unused argument")

  # Without the fake key store, the real scythe_get_key() finds the token in
  # the dataone_token environment variable.
  server <- fake_citations_service()
  server$clock <- list(now = as.POSIXct("2026-10-01 12:00:00", tz = "UTC"))
  local_mocked_bindings(http_post = server$post, http_now = function() server$clock$now)
  withr::local_envvar(dataone_token = test_token(sub = "CN=from-env"))
  suppressMessages(submit_citations(submission_pairs()[1, ]))
  expect_equal(posted_bodies(server)[[1]]$submitter, "CN=from-env")
  expect_equal(server$posts()[[1]]$headers$Authorization,
               paste("Bearer", test_token(sub = "CN=from-env")))
})

test_that("token claims are read from a JWT, and anything else gives none", {
  expect_equal(token_claims(test_token(sub = "CN=x"))$sub, "CN=x")
  expect_equal(token_expires(token_claims(test_token())), as.POSIXct("2026-12-01", tz = "UTC"))
  expect_null(token_claims(NA))
  expect_null(token_claims("abc"))
  expect_null(token_claims("abc.!!!.def"))
  expect_null(token_expires(token_claims(test_token(exp = NULL))))
})

test_that("the integration tests refuse DataONE's production metrics service", {
  expect_error(check_test_citations_url(citations_url_default), "production metrics service")
  expect_error(check_test_citations_url("https://METRICS.dataone.org/citations"),
               "production metrics service")
  expect_equal(check_test_citations_url("http://localhost:8080/citations"),
               "http://localhost:8080/citations")
  expect_equal(url_host("https://Example.org:8443/x?y=1"), "example.org")
})
