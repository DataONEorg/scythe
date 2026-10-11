# Tests of the comparison with DataONE's metrics service, against a response
# recorded from the service and fakes of it and of the CN's index.

# A fake metrics service answering each repository with `recorded[[node]]`
# (a list mapping citing DOI to target identifiers), in front of `server`'s
# other answers.
with_fake_metrics <- function(server, recorded = list(), status = 200) {
  handler <- server$handler
  server$handler <- function(url, headers, n) {
    if (startsWith(url, metrics_url_default)) {
      if (status != 200) return(respond(status))
      query <- jsonlite::fromJSON(utils::URLdecode(sub("^.*[?]q=", "", url)), simplifyVector = FALSE)
      node <- query$filterBy[[1]]$values[[1]]
      cites <- recorded[[node]] %||% list()
      body <- list(resultDetails = list(citations = lapply(stats::setNames(names(cites), names(cites)),
        function(src) list(source_id = src, target_id = as.list(cites[[src]]), title = paste("Work", src)))))
      return(respond(body = body))
    }
    handler(url, headers, n)
  }
  server
}

test_that("the metrics service's citations are read, one row per citing work and dataset", {
  server <- fake_http(function(url, headers, n) respond(body = fixture("metrics-si.json")))
  recorded <- with_fake_http(server, get_metrics_citations("urn:node:SI", from = as.Date("2000-01-01"),
                                                          to = as.Date("2026-10-10")))
  expect_equal(nrow(recorded), 6)
  expect_equal(unique(recorded$node), "urn:node:SI")
  two <- recorded[recorded$source_id == "10.1111/1365-2435.12085", ]
  expect_equal(two$target_id, c("urn:uuid:99a5737c-c660-45d7-9adc-f735b32c429f",
                                "urn:uuid:67f12f26-19ba-4d9b-b432-bd495c41374c"))
  expect_equal(unique(two$journal), "Functional Ecology")
  expect_true("10.60635/C3MS3K" %in% recorded$target_id)
  # One request, for the repository and the range of months.
  query <- jsonlite::fromJSON(utils::URLdecode(sub("^.*[?]q=", "", server$urls())), simplifyVector = FALSE)
  expect_equal(query$filterBy[[1]]$values[[1]], "urn:node:SI")
  expect_equal(unlist(query$filterBy[[2]]$values), c("01/01/2000", "10/10/2026"))
  expect_match(server$urls(), paste0("^", metrics_url_default, "\\?q="))
})

test_that("each node is a separate request, and the address can be changed", {
  server <- fake_http(function(url, headers, n) respond(body = list(resultDetails = list(citations = list()))))
  withr::local_options(scythe.metrics_url = "https://metrics.example.org/metrics")
  recorded <- with_fake_http(server, get_metrics_citations(c("urn:node:A", "urn:node:B")))
  expect_equal(nrow(recorded), 0)
  expect_named(recorded, c("node", "source_id", "target_id", metrics_fields))
  expect_length(server$urls(), 2)
  expect_true(all(startsWith(server$urls(), "https://metrics.example.org/metrics?q=")))
})

test_that("server errors from the metrics service are retried, then reported", {
  server <- fake_http(function(url, headers, n) respond(503))
  expect_error(with_fake_http(server, get_metrics_citations("urn:node:SI")),
               "The metrics service query failed \\(HTTP 503\\)")
  expect_length(server$urls(), 4)
})

test_that("citations not in the metrics service are kept, comparing DOIs in any serialization", {
  found <- data.frame(article_id = c("10.1234/A", "10.1234/b", "10.1234/c"),
                      target_id = c("doi:10.18739/P1", "https://doi.org/10.18739/P1", "knb.298.2"))
  recorded <- data.frame(source_id = c("10.1234/a", "10.1234/c"), target_id = c("10.18739/p1", "KNB.298.2"))
  # b is new; c's target isn't a DOI, so its case matters.
  expect_equal(citations_not_in_metrics(found, recorded)$article_id, c("10.1234/b", "10.1234/c"))
})

test_that("identifiers are compared as DOIs when they are DOIs, and as exact literals otherwise", {
  expect_equal(identifier_key(c("doi:10.18739/A2X", "https://doi.org/10.18739/a2x", "10.18739/A2X")),
               rep("10.18739/a2x", 3))
  expect_equal(identifier_key(c("urn:uuid:ABC", "knb.298.2", " arctic-data.7792.4 ")),
               c("urn:uuid:ABC", "knb.298.2", "arctic-data.7792.4"))
})

test_that("a citation is recorded when the service has the same citing work and target", {
  total <- data.frame(
    article_id = c("10.1234/doi", "10.1234/sid", "10.1234/two", "10.1234/new", "10.1234/lost"),
    target_id = c("doi:10.18739/A2X", "doi:10.5063/SERIES1", "doi:10.18739/A2Y", "doi:10.18739/A2X", NA),
    node = c("urn:node:ARCTIC", "urn:node:KNB", "urn:node:ARCTIC", "urn:node:ARCTIC", "urn:node:ARCTIC")
  )
  recorded <- data.frame(
    node = c("urn:node:ARCTIC", "urn:node:KNB", "urn:node:ARCTIC"),
    source_id = c("10.1234/DOI", "10.1234/sid", "10.1234/two"),
    # The same DOI, bare; a version PID where scythe found the series's SID
    # (a different target, so a different citation); another dataset cited
    # by the same work.
    target_id = c("10.18739/a2x", "urn:uuid:v3", "10.18739/A2Z")
  )
  expect_equal(metrics_status(total, recorded), c("yes", "no", "no", "no", "unmapped"))
})

nodes <- c("urn:node:ARCTIC", "urn:node:KNB")
run_all <- function(dir, ...) {
  scheduled_search(nodes, dir, other_nodes = NULL, parallel = FALSE, text_sources = character(), ...)
}

test_that("the scheduled search compares with the metrics service daily, and lists citations to submit", {
  dir <- withr::local_tempdir()
  # The service has A2 (to p2's SID, recorded as a bare DOI), and A3 citing a
  # different target; A1 is new.
  server <- with_fake_metrics(fake_services(works),
    recorded = list("urn:node:KNB" = list("10.1/a2" = "10.5063/P2"),
                    "urn:node:ARCTIC" = list("10.1/a3" = "urn:uuid:v3")))
  with_fake_http(server, with_fake_dataone({
    run_all(dir)
    t <- total(dir)
    row <- function(article) t[t$article_id == article, ]
    expect_equal(row("10.1/a1")$in_metrics, "no")
    expect_equal(row("10.1/a2")$in_metrics, "yes")
    # A3's citation of p1 is a different tuple from the one recorded.
    expect_equal(row("10.1/a3")$in_metrics, "no")
    # Targets are the identifiers exactly as DataONE records them.
    expect_equal(row("10.1/a1")$target_id, "doi:10.18739/P1")
    expect_equal(row("10.1/a2")$target_id, "https://doi.org/10.5063/P2")
    expect_equal(row("10.1/a2")$target_type, "sid")
    submit <- read.csv(file.path(dir, "results", "to-submit.csv"), colClasses = "character")
    expect_equal(names(submit)[1:4], c("source_id", "target_id", "target_type", "node"))
    expect_equal(sort(submit$source_id), c("10.1/a1", "10.1/a3"))
    expect_equal(unique(submit$target_id), "doi:10.18739/P1")
    expect_match(paste(readLines(file.path(dir, "state", "status.md")), collapse = "\n"),
                 "1 already recorded, 2 to submit (results/to-submit.csv).", fixed = TRUE)

    # Not again within a day.
    metrics_requests <- function() sum(startsWith(server$urls(), metrics_url_default))
    before <- metrics_requests()
    server$clock$now <- server$clock$now + 3600
    run_all(dir)
    expect_equal(metrics_requests(), before)
    server$clock$now <- server$clock$now + 24 * 3600
    run_all(dir)
    expect_gt(metrics_requests(), before)
  }))
})

test_that("every dataset a work cites is submitted, even when the work is already recorded for another", {
  total <- data.frame(
    article_id = rep("10.1234/survey", 3),
    target_id = c("doi:10.18739/A2X", "doi:10.18739/A2Y", "doi:10.18739/A2SERIES"),
    target_type = c("pid", "pid", "sid"),
    node = "urn:node:ARCTIC"
  )
  # The service has the work citing X, and a version of the series.
  recorded <- data.frame(node = "urn:node:ARCTIC", source_id = "10.1234/survey",
                         target_id = c("10.18739/A2X", "doi:10.18739/A2SERIES.V3"))
  expect_equal(metrics_status(total, recorded), c("yes", "no", "no"))
  expect_equal(citations_not_in_metrics(total, recorded)$target_id,
               c("doi:10.18739/A2Y", "doi:10.18739/A2SERIES"))
})

test_that("a citation whose DOI isn't in the identifier list is never submitted", {
  found <- data.frame(article_id = "10.1234/a", article_title = "A", dataset_id = "10.9999/unknown")
  annotated <- suppressMessages(annotate_citations(found, priority_dois))
  expect_true(is.na(annotated$target_id))
  expect_message(annotate_citations(found, priority_dois), "Couldn't find the DataONE identifier of 1 cited DOIs")
  expect_equal(metrics_status(annotated, data.frame(node = character(), source_id = character(),
                                                    target_id = character())), "unmapped")
})

test_that("a failing metrics service doesn't stop the run, and is alerted after 3 days", {
  dir <- withr::local_tempdir()
  server <- with_fake_metrics(fake_services(works), status = 503)
  with_fake_http(server, with_fake_dataone({
    messages <- capture_messages(status <- run_all(dir))
    expect_equal(status$phase, "done")
    expect_true(any(grepl("Couldn't compare with the metrics service", messages)))
    expect_false(file.exists(file.path(dir, "results", "to-submit.csv")))
    expect_null(status$alerts)
    server$clock$now <- server$clock$now + 3.5 * 86400
    status <- run_all(dir)
    expect_equal(status$alerts, "metrics:stale")
    expect_match(paste(readLines(file.path(dir, "state", "alerts.log")), collapse = "\n"),
                 "hasn't succeeded yet \\(last error: The metrics service query failed \\(HTTP 503\\)\\)")
  }))
})

test_that("citations added after a comparison wait for the next one", {
  dir <- withr::local_tempdir()
  path <- file.path(dir, "citations.csv")
  found <- data.frame(article_id = "10.1/a", article_title = "A", dataset_id = "10.5/d",
                      target_id = "doi:10.5/D", target_type = "pid", node = "urn:node:KNB")
  total <- add_to_total(found, path, "xdd", "priority_text", "c1", "t1")
  total$in_metrics <- "yes"
  write_csv_atomic(total, path)
  found$article_id <- "10.1/b"
  total <- add_to_total(found, path, "xdd", "priority_text", "c1", "t2")
  expect_equal(total$in_metrics, c("yes", NA))
})
