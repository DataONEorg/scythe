# Tests of classify_data_relations(). The offline tests replace DataCite with
# fakes; the live test, which sends a few DataCite requests, never runs on CRAN
# or CI.

# Write `relations` as the data relations log of a search, in a temporary
# log directory, and return the directory.
local_relations_log <- function(relations, env = parent.frame()) {
  log_dir <- withr::local_tempdir(.local_envir = env)
  write_jsonl(relations, file.path(log_dir, "openalex-data-relations.jsonl"))
  log_dir
}

relation_pairs <- function(dataset_doi, work_doi, match_method = "graph") {
  data.frame(dataset_doi = dataset_doi, work_doi = work_doi, match_method = match_method,
             stringsAsFactors = FALSE)
}

test_that("data relations are classified from DataCite relations in either direction", {
  local_mocked_bindings(datacite_related = function(doi) {
    switch(doi,
      "10.18739/ds1.v2" = data.frame(doi = "10.18739/ds1",
                                     relation_type = c("References", "IsNewVersionOf")),
      "10.18739/ds3" = data.frame(doi = "10.18739/derived", relation_type = "IsSourceOf"),
      NULL
    )
  })
  log_dir <- local_relations_log(relation_pairs(
    dataset_doi = c("10.18739/ds1", "10.18739/ds3", "10.18739/ds4"),
    work_doi = c("10.18739/ds1.v2", "10.18739/derived", "10.6073/pasta/abc"),
    match_method = c("text", "graph", "both")
  ))
  out <- classify_data_relations(log_dir)
  expect_equal(out$relation_class, c("version", "derivation", "unknown"))
  # A pair with several relations keeps them all and takes the most specific.
  expect_equal(out$relation_type, c("References;IsNewVersionOf", "IsSourceOf", "openalex:cites"))
  expect_equal(out$evidence, c("datacite:10.18739/ds1.v2", "datacite:10.18739/ds3",
                               "openalex:both"))

  classified <- jsonlite::stream_in(
    file(file.path(log_dir, "openalex-data-relations-classified.jsonl")), verbose = FALSE
  )
  expect_equal(classified$relation_class, out$relation_class)
})

test_that("each pair is classified once, from its latest log record", {
  lookups <- character()
  local_mocked_bindings(datacite_related = function(doi) {
    lookups <<- c(lookups, doi)
    NULL
  })
  log_dir <- local_relations_log(relation_pairs(
    dataset_doi = c("10.18739/ds1", "10.18739/ds1"),
    work_doi = c("10.18739/ds2", "10.18739/ds2"),
    match_method = c("text", "both")
  ))
  out <- classify_data_relations(log_dir)
  expect_equal(nrow(out), 1)
  expect_equal(out$match_method, "both")
  expect_setequal(lookups, c("10.18739/ds1", "10.18739/ds2"))
  expect_length(lookups, 2)

  # Running it again replaces the classified log rather than appending to it.
  classify_data_relations(log_dir)
  classified <- jsonlite::stream_in(
    file(file.path(log_dir, "openalex-data-relations-classified.jsonl")), verbose = FALSE
  )
  expect_equal(nrow(classified), 1)
})

test_that("without a data relations log, there is nothing to classify", {
  local_mocked_bindings(datacite_related = function(doi) stop("no lookups expected"))
  out <- classify_data_relations(withr::local_tempdir())
  expect_equal(nrow(out), 0)
  expect_true(all(c("relation_class", "evidence") %in% names(out)))
})

test_that("DataCite lookups retry rate limits and report failures", {
  record <- charToRaw(jsonlite::toJSON(list(data = list(attributes = list(
    relatedIdentifiers = list(list(relatedIdentifier = "10.18739/DS1",
                                   relationType = "IsNewVersionOf"))
  ))), auto_unbox = TRUE))
  respond <- function(codes) {
    calls <- 0
    function(url) {
      calls <<- calls + 1
      code <- codes[min(calls, length(codes))]
      list(status_code = code, headers = raw(0),
           content = if (code %in% 200) record else raw(0))
    }
  }
  sleeps <- numeric()
  local_mocked_bindings(openalex_sleep = function(s) sleeps <<- c(sleeps, s))

  local_mocked_bindings(datacite_fetch = respond(c(429, 503, 200)))
  related <- datacite_related("10.18739/ds1.v2")
  expect_equal(related$doi, "10.18739/ds1")
  expect_equal(related$relation_type, "IsNewVersionOf")
  expect_equal(sleeps, c(2, 4))

  local_mocked_bindings(datacite_fetch = respond(404))
  expect_null(datacite_related("10.1/crossref"))

  local_mocked_bindings(datacite_fetch = respond(NA_integer_))
  expect_identical(datacite_related("10.18739/ds1.v2"), NA)
})

test_that("relations whose DataCite lookup failed are marked for review", {
  local_mocked_bindings(datacite_related = function(doi) {
    if (doi == "10.18739/ds2") NA
  })
  relations <- data.frame(dataset_doi = c("10.18739/ds1", "10.18739/ds2"),
                          work_doi = c("10.18739/other", "10.18739/other"),
                          match_method = "graph", stringsAsFactors = FALSE)
  out <- suppressMessages(classify_relations(relations))
  expect_equal(out$relation_class, c("unknown", "unknown"))
  expect_equal(out$evidence, c("openalex:graph",
                               "openalex:graph;datacite:lookup_failed:10.18739/ds2"))
  expect_message(classify_relations(relations), "DataCite lookups failed for 1 DOIs")
})

test_that("relation types map to relation classes", {
  expect_equal(unname(relation_classes[c("IsVersionOf", "IsDerivedFrom", "HasPart", "Cites")]),
               c("version", "derivation", "composition", "citation"))
})

test_that("a known DataCite version relation is classified (live)", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline("api.datacite.org")
  # In DataCite, 10.18739/A2P26Q584 IsNewVersionOf 10.18739/A2GF0MZ7C.
  log_dir <- local_relations_log(relation_pairs(
    dataset_doi = c("10.18739/a2gf0mz7c", "10.18739/a2d50g069"),
    work_doi = c("10.18739/a2p26q584", "10.18739/a2p26q584"),
    match_method = "text"
  ))
  out <- classify_data_relations(log_dir)
  expect_equal(out$relation_class, c("version", "unknown"))
  expect_match(out$relation_type[1], "IsNewVersionOf")
  expect_equal(out$evidence[1], "datacite:10.18739/a2p26q584")
})
