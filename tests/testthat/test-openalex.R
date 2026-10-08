test_that("normalize_doi handles DOI prefixes, schemes and encodings", {
  variants_file <- system.file("testdata", "doi-variants.csv", package = "scythe")
  variants <- read.csv(variants_file, stringsAsFactors = FALSE, na.strings = "")
  expect_equal(normalize_doi(variants$input), variants$expected)
})

test_that("check_identifiers warns about identifiers that are not DOIs or uuids", {
  expect_warning(check_identifiers("not-a-doi"), "does not appear")
  expect_no_warning(check_identifiers(c("DOI:10.18739/A22274", "urn:uuid:1234")))
  expect_equal(check_identifiers(c("DOI: 10.18739/A22274", "urn:uuid:1234")),
               c("10.18739/A22274", "1234"))
})

test_that("text search attributes each matching work to the DOI it mentions", {
  dois <- sprintf("10.9999/d%03d", 1:40)
  server <- fake_openalex(list(
    fake_work("W1", "10.1/a", text = dois[3]),
    fake_work("W2", "10.1/b", text = c(dois[3], dois[29])),
    fake_work("W3", "10.1/c", text = "10.9999/unrelated")
  ))
  with_fake_openalex(server, {
    res <- citation_search_openalex(dois, methods = "text")
    pairs <- res[order(res$dataset_id, res$article_id), c("article_id", "dataset_id")]
    expect_equal(pairs$article_id, c("10.1/a", "10.1/b", "10.1/b"))
    expect_equal(pairs$dataset_id, dois[c(3, 3, 29)])
    expect_true(attr(res, "openalex_status")$complete)
    # One screening search, then two searches per level of bisection for
    # each of the two branches with matches (40 -> 20 -> 10 -> 5 -> 3 -> 2 -> 1).
    expect_lte(length(server$urls), 1 + 2 * 2 * 6)
  })
})

test_that("text search sends a single search when no DOI is mentioned", {
  server <- fake_openalex(list(fake_work("W1", "10.1/a", text = "10.9999/other")))
  with_fake_openalex(server, {
    res <- citation_search_openalex(sprintf("10.9999/d%03d", 1:50), methods = "text")
    expect_equal(nrow(res), 0)
    expect_named(res, c("article_id", "article_title", "dataset_id", "source"))
    expect_length(server$urls, 1)
    expect_match(server$urls, "fulltext.search.exact", fixed = TRUE)
    expect_match(server$urls, "include_xpac=true", fixed = TRUE)
  })
})

test_that("graph search joins citing works to the datasets they reference", {
  server <- fake_openalex(list(
    fake_work("D1", "10.18739/ds1", type = "dataset"),
    fake_work("D2", "10.18739/ds2", type = "dataset"),
    fake_work("A1", "10.1/a1", cites = c("D1", "X9")),
    fake_work("A2", "10.1/a2", cites = c("D1", "D2"))
  ))
  with_fake_openalex(server, {
    res <- citation_search_openalex(c("10.18739/DS1", "10.18739/DS2", "10.18739/missing"),
                                    methods = "graph")
    pairs <- res[order(res$dataset_id, res$article_id), c("article_id", "dataset_id")]
    expect_equal(pairs$article_id, c("10.1/a1", "10.1/a2", "10.1/a2"))
    expect_equal(pairs$dataset_id, c("10.18739/DS1", "10.18739/DS1", "10.18739/DS2"))
    unresolved <- jsonlite::stream_in(file(file.path(attr(res, "openalex_status")$log_dir,
                                                     "openalex-unresolved.jsonl")), verbose = FALSE)
    expect_equal(unresolved$identifier, "10.18739/missing")
  })
})

test_that("graph search follows pagination of citing works", {
  citers <- lapply(1:450, function(i) fake_work(paste0("A", i), paste0("10.1/a", i), cites = "D1"))
  server <- fake_openalex(c(list(fake_work("D1", "10.18739/ds1", type = "dataset")), citers))
  with_fake_openalex(server, {
    res <- citation_search_openalex("10.18739/ds1", methods = "graph")
    expect_equal(nrow(res), 450)
    expect_length(server$urls, 1 + 3)
  })
})

test_that("matches are reported once, under each input form of the dataset DOI", {
  server <- fake_openalex(list(
    fake_work("D1", "10.18739/ds1", type = "dataset"),
    fake_work("A1", "10.1/a1", cites = "D1", text = "10.18739/ds1")
  ))
  with_fake_openalex(server, {
    res <- citation_search_openalex(c("doi:10.18739/DS1", "https://doi.org/10.18739/ds1"))
    expect_equal(sort(res$dataset_id), sort(c("10.18739/DS1", "https://doi.org/10.18739/ds1")))
    matches <- jsonlite::stream_in(file(file.path(attr(res, "openalex_status")$log_dir,
                                                  "openalex-matches.jsonl")), verbose = FALSE)
    expect_equal(matches$match_method, "both")
  })
})

test_that("datasets and works without DOIs go to review logs, not results", {
  server <- fake_openalex(list(
    fake_work("A1", "10.1/a1", text = "10.18739/ds1"),
    fake_work("V2", "10.18739/ds1.v2", type = "dataset", text = "10.18739/ds1"),
    fake_work("E1", "10.6073/pasta/abc", type = "article", text = "10.18739/ds1"),
    fake_work("N1", NULL, type = "article", text = "10.18739/ds1"),
    fake_work("S1", "10.18739/ds1", type = "dataset", text = "10.18739/ds1")
  ))
  with_fake_openalex(server, {
    local_mocked_bindings(datacite_related = function(doi) {
      if (doi == "10.18739/ds1.v2") {
        data.frame(doi = "10.18739/ds1", relation_type = c("References", "IsNewVersionOf"))
      }
    })
    res <- citation_search_openalex("10.18739/ds1", methods = "text")
    expect_equal(res$article_id, "10.1/a1")

    log_dir <- attr(res, "openalex_status")$log_dir
    read_log <- function(name) {
      jsonlite::stream_in(file(file.path(log_dir, name)), verbose = FALSE)
    }
    relations <- read_log("openalex-data-relations.jsonl")
    relations <- relations[order(relations$work_doi), ]
    expect_equal(relations$work_doi, c("10.18739/ds1.v2", "10.6073/pasta/abc"))
    expect_equal(relations$relation_class, c("version", "unknown"))
    expect_equal(relations$relation_type, c("References;IsNewVersionOf", "openalex:mentions"))
    expect_equal(relations$evidence[1], "datacite:10.18739/ds1.v2")

    no_doi <- read_log("openalex-no-doi-citers.jsonl")
    expect_equal(no_doi$work_id, "N1")
    expect_equal(no_doi$dataset_doi, "10.18739/ds1")
  })
})

test_that("relation types map to relation classes", {
  expect_equal(unname(relation_classes[c("IsVersionOf", "IsDerivedFrom", "HasPart", "Cites")]),
               c("version", "derivation", "composition", "citation"))
})

test_that("the cost check stops before any request is sent", {
  server <- fake_openalex()
  with_fake_openalex(server, {
    expect_error(citation_search_openalex(sprintf("10.9999/d%04d", 1:5000), max_credits = 100),
                 "exceeds max_credits")
    expect_length(server$urls, 0)

    res <- citation_search_openalex(sprintf("10.9999/d%04d", 1:5000), max_credits = Inf)
    expect_true(attr(res, "openalex_status")$complete)
  })
})

test_that("without the preflight check, a search stops once max_credits is spent", {
  server <- fake_openalex()
  with_fake_openalex(server, {
    expect_warning(
      res <- citation_search_openalex(sprintf("10.9999/d%04d", 1:5000), methods = "text",
                                      max_credits = 50, preflight = FALSE),
      "Stopped after spending 50"
    )
    expect_length(server$urls, 5)
    status <- attr(res, "openalex_status")
    expect_false(status$complete)
    expect_equal(status$stopped, "max_credits")
  })
})

test_that("a short rate limit is waited out, then requests resume one at a time", {
  server <- fake_openalex(
    works = list(fake_work("A1", "10.1/a1", text = "10.9999/d0001")),
    responses = list(list(status = 429, headers = list(`retry-after` = "2")))
  )
  with_fake_openalex(server, {
    res <- citation_search_openalex(sprintf("10.9999/d%04d", 1:300), methods = "text",
                                    max_credits = Inf)
    expect_equal(res$article_id, "10.1/a1")
    expect_equal(server$clock$sleeps, 2)
    expect_true(attr(res, "openalex_status")$complete)
  })
})

test_that("a long rate limit stops all requests and is shared through the state file", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(
    responses = list(list(status = 429, headers = list(`x-ratelimit-reset` = "5545")))
  )
  with_fake_openalex(server, {
    withr::local_options(scythe.openalex.concurrency = 1)
    expect_warning(
      res <- citation_search_openalex(sprintf("10.9999/d%04d", 1:300), methods = "text",
                                      checkpoint_dir = dir, max_credits = Inf),
      "rate limited until"
    )
    expect_length(server$urls, 1)
    expect_length(server$clock$sleeps, 0)
    status <- attr(res, "openalex_status")
    expect_false(status$complete)
    expect_equal(status$stopped, "rate_limited")
    state <- jsonlite::read_json(file.path(dir, "openalex-state.json"))
    expect_equal(state$paused_until, "2026-10-01T13:32:25Z")

    # A later run (or another process) sharing the state file sends nothing.
    res <- citation_search_openalex("10.9999/d0001", methods = "text", checkpoint_dir = dir)
    expect_length(server$urls, 1)
    expect_equal(attr(res, "openalex_status")$stopped, "rate_limited")

    # Once the pause has passed, searches resume.
    server$clock$now <- server$clock$now + 6000
    res <- citation_search_openalex("10.9999/d0001", methods = "text", checkpoint_dir = dir)
    expect_length(server$urls, 2)
  })
})

test_that("running out of daily budget stops requests before a 429", {
  server <- fake_openalex(headers = list(`x-ratelimit-limit` = "1000",
                                         `x-ratelimit-remaining` = "40",
                                         `x-ratelimit-reset` = "3600"))
  with_fake_openalex(server, {
    withr::local_options(scythe.openalex.concurrency = 1)
    expect_warning(
      res <- citation_search_openalex(sprintf("10.9999/d%04d", 1:500), methods = "text",
                                      max_credits = Inf),
      "rate limited until"
    )
    # The first response shows 40 credits left, below the 5% reserve of 50.
    expect_length(server$urls, 1)
    expect_false(attr(res, "openalex_status")$complete)
  })
})

test_that("repeated 429s double the pause", {
  server <- fake_openalex(responses = rep(list(list(status = 429, headers = list(`retry-after` = "10"))), 4))
  with_fake_openalex(server, {
    withr::local_options(scythe.openalex.concurrency = 1)
    res <- citation_search_openalex("10.9999/d0001", methods = "text")
    expect_equal(server$clock$sleeps, c(10, 10, 20, 40))
    expect_length(server$urls, 5)
    expect_true(attr(res, "openalex_status")$complete)
  })
})

test_that("server errors are retried, then the batch is logged as failed", {
  server <- fake_openalex(responses = rep(list(list(status = 500)), 4))
  with_fake_openalex(server, {
    res <- citation_search_openalex("10.9999/d0001", methods = "text")
    expect_length(server$urls, 4)
    expect_false(attr(res, "openalex_status")$complete)
    failed <- jsonlite::stream_in(file(file.path(attr(res, "openalex_status")$log_dir,
                                                 "openalex-failed-batches.jsonl")), verbose = FALSE)
    expect_equal(failed$dois, "10.9999/d0001")
  })
})

test_that("a rerun with the same checkpoint directory skips completed batches", {
  dir <- withr::local_tempdir()
  server <- fake_openalex(list(fake_work("A1", "10.1/a1", text = "10.9999/d0001")))
  with_fake_openalex(server, {
    dois <- sprintf("10.9999/d%04d", 1:150)
    first <- citation_search_openalex(dois, checkpoint_dir = dir, max_credits = Inf)
    sent <- length(server$urls)
    second <- citation_search_openalex(dois, checkpoint_dir = dir, max_credits = Inf)
    expect_length(server$urls, sent)
    expect_equal(second, first, ignore_attr = TRUE)
  })
})

test_that("since adds a publication date filter", {
  server <- fake_openalex()
  with_fake_openalex(server, {
    citation_search_openalex("10.9999/d0001", since = as.Date("2025-10-01"))
    text_search <- grepl("fulltext.search.exact", server$urls, fixed = TRUE)
    expect_match(server$urls[text_search], "from_publication_date%3A2025-10-01", fixed = TRUE)
    # Resolving dataset DOIs is not limited by date; only their citing works are.
    expect_no_match(server$urls[!text_search], "from_publication_date", fixed = TRUE)
  })
})

test_that("openalex results combine with other sources in citation_search", {
  server <- fake_openalex(list(fake_work("A1", "10.1/a1", text = "10.9999/d0001")))
  with_fake_openalex(server, {
    res <- citation_search("10.9999/d0001", sources = "openalex")
    expect_equal(res$article_id, "10.1/a1")
    expect_true(all(vapply(res, is.character, TRUE)))
  })
})

# Live tests against the OpenAlex API, using a few dozen credits.

test_that("openalex finds known citations by text and by citation graph", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  citations <- read.csv(system.file("testdata", "test-citations.csv", package = "scythe"),
                        stringsAsFactors = FALSE)
  citations <- citations[citations$source == "openalex", ]
  res <- citation_search_openalex(unique(citations$dataone_pid), log_dir = withr::local_tempdir())
  expect_true(all(tolower(citations$pub_id) %in% res$article_id))

  matches <- jsonlite::stream_in(file(file.path(attr(res, "openalex_status")$log_dir,
                                                "openalex-matches.jsonl")), verbose = FALSE)
  method <- matches$match_method[match(tolower(citations$pub_id), matches$work_doi)]
  expect_true("text" %in% method)
  expect_true("graph" %in% method)
})

test_that("openalex does not return known non-citations", {
  skip_on_cran()
  skip_if_offline("api.openalex.org")
  negatives <- read.csv(system.file("testdata", "openalex-negatives.csv", package = "scythe"),
                        stringsAsFactors = FALSE)
  res <- citation_search_openalex(unique(negatives$dataset_id), log_dir = withr::local_tempdir())
  found <- paste(res$article_id, normalize_doi(res$dataset_id))
  expect_false(any(paste(negatives$article_id, normalize_doi(negatives$dataset_id)) %in% found))
})
