# Tests of collecting DataONE DOIs, against fake Solr indexes that reproduce
# what the real ones do (see DataONEorg/dataone#29): replicas in a member
# node's index, records whose authority moved to another node, GOA's quoted
# authoritativeMN values, ESS-DIVE refusing queries, and KNB's index listing
# LTER and PISCO DOIs as its own.

cn <- "https://cn.test/cn"

record <- function(id, node, seriesId = NA, obsoletes = NA, obsoletedBy = NA,
                   formatType = "METADATA", dateUploaded = "2020-01-01T00:00:00Z") {
  data.frame(id = id, seriesId = seriesId, obsoletes = obsoletes, obsoletedBy = obsoletedBy,
             authoritativeMN = node, formatType = formatType, dateUploaded = dateUploaded,
             stringsAsFactors = FALSE)
}

knb_records <- rbind(
  # A version chain linked through a version with a UUID identifier.
  record("doi:10.5063/AA/v1", "urn:node:KNB", obsoletedBy = "urn:uuid:k2", dateUploaded = "2001-01-01T00:00:00Z"),
  record("doi:10.5063/AA/v3", "urn:node:KNB", obsoletes = "urn:uuid:k2", dateUploaded = "2003-01-01T00:00:00Z"),
  # DOIs as both identifier and series identifier.
  record("doi:10.5063/F1SER1", "urn:node:KNB", seriesId = "doi:10.5063/SERIES1"),
  record("urn:uuid:k9", "urn:node:KNB", seriesId = "doi:10.5063/SERIES2"),
  # Retired LTER and PISCO records that KNB's index lists as its own.
  record("doi:10.6073/AA/knb-lter-x.1.1", "urn:node:KNB"),
  record("doi:10.6085/AA/PISCO1", "urn:node:KNB"),
  # A replica of an ARCTIC record, and a record whose authority moved to CA_OPC.
  record("doi:10.18739/A2REPLICA", "urn:node:ARCTIC"),
  record("doi:10.5063/OPC1", "urn:node:CA_OPC"),
  # A DOI under DataCite's test prefix.
  record("doi:10.5072/FK2TEST", "urn:node:KNB"),
  # DOIs serialized as URLs; one is also registered in doi: form.
  record("https://doi.org/10.5063/URL1", "urn:node:KNB"),
  record("http://dx.doi.org/10.5063/TWOFORMS", "urn:node:KNB", dateUploaded = "2001-01-01T00:00:00Z"),
  record("doi:10.5063/TWOFORMS", "urn:node:KNB", dateUploaded = "2005-01-01T00:00:00Z")
)
cn_records <- rbind(
  knb_records[c(1:4, 8), ],
  record("doi:10.18739/A2REPLICA", "urn:node:ARCTIC"),
  record("doi:10.18739/A2CN", "urn:node:ARCTIC"),
  record("doi:10.5063/GOA1", "urn:node:GOA"),
  record("doi:10.15485/ESS1", "urn:node:ESS_DIVE"),
  record("doi:10.6085/AA/PISCO1", "urn:node:PISCO"),
  record("doi:10.1594/PANGAEA.1", "urn:node:PANGAEA")
)
indexes <- list(
  "https://cn.test/cn" = cn_records,
  "https://knb.test/mn" = knb_records,
  "https://arctic.test/mn" = rbind(
    record("doi:10.18739/A2REPLICA", "urn:node:ARCTIC"),
    record("doi:10.18739/A2CN", "urn:node:ARCTIC"),
    # New upload not yet on the CN, with an identifier in doi.org/ form.
    record("doi.org/10.18739/A2NEW", "urn:node:ARCTIC")
  ),
  # GOA's index quotes its authoritativeMN values.
  "https://goa.test/mn" = record("doi:10.5063/GOA1", "\"urn:node:GOA\""),
  # SFWMD has no DOIs yet.
  "https://sfwmd.test/mn" = record("x", "urn:node:SFWMD")[0, ]
)

node_list <- paste0(
  '<?xml version="1.0"?><ns3:nodeList xmlns:ns3="http://ns.dataone.org/service/types/v2.0">',
  '<node type="cn" state="up"><identifier>urn:node:CN</identifier><baseURL>https://cn.test/cn</baseURL></node>',
  paste0('<node replicate="true" type="mn" state="up"><identifier>urn:node:', c("KNB", "ARCTIC", "GOA", "ESS_DIVE", "SFWMD"),
         '</identifier><name>x</name><baseURL>https://', c("knb", "arctic", "goa", "essdive", "sfwmd"),
         '.test/mn</baseURL></node>', collapse = ""),
  '</ns3:nodeList>'
)

# A fake Solr service answering the DOI record queries for `indexes`, with
# the given HTTP status for some base URLs.
fake_solr <- function(indexes, status = list("https://essdive.test/mn" = 403)) {
  fake_http(function(url, headers, n) {
    if (grepl("/v2/node$", url)) return(respond(body = node_list))
    base <- sub("/v2/query/solr/.*$", "", url)
    if (!is.null(status[[base]])) return(respond(status[[base]], body = list(message = "Forbidden")))
    query <- strsplit(sub("^[^?]*\\?", "", url), "&")[[1]]
    params <- stats::setNames(lapply(query, function(q) utils::URLdecode(sub("^[^=]*=", "", q))),
                              sub("=.*$", "", query))
    docs <- indexes[[base]]
    fq <- params$fq
    if (fq != "*:*") {
      nodes <- regmatches(fq, gregexpr('"[^"]*"', fq))[[1]]
      nodes <- gsub('^"|"$', "", nodes)
      hit <- docs$authoritativeMN %in% nodes
      docs <- docs[if (startsWith(fq, "-")) !hit else hit, , drop = FALSE]
    }
    docs <- docs[order(docs$id), , drop = FALSE]
    rows <- as.integer(params$rows)
    start <- if (is.null(params$cursorMark) || params$cursorMark == "*") 0 else as.integer(params$cursorMark)
    page <- utils::head(utils::tail(docs, max(0, nrow(docs) - start)), rows)
    body <- list(response = list(numFound = nrow(docs), docs = lapply(seq_len(nrow(page)), function(i) {
      as.list(Filter(Negate(is.na), unlist(page[i, ])))
    })))
    if (!is.null(params$cursorMark)) {
      body$nextCursorMark <- if (nrow(page) == 0) params$cursorMark else as.character(start + nrow(page))
    }
    respond(body = body)
  })
}

collect <- function(server, ...) {
  with_fake_http(server, collect_dataone_dois(..., cn_url = cn))
}

test_that("a priority node's DOIs come from its own index, without replicas or transferred records", {
  server <- fake_solr(indexes)
  dois <- collect(server, c("urn:node:KNB", "urn:node:ARCTIC"))
  knb <- dois[dois$node == "urn:node:KNB", ]
  expect_setequal(knb$doi, c("10.5063/aa/v1", "10.5063/aa/v3", "10.5063/f1ser1", "10.5063/series1",
                             "10.5063/series2", "10.6073/aa/knb-lter-x.1.1", "10.6085/aa/pisco1",
                             "10.5063/url1", "10.5063/twoforms"))
  expect_true(all(knb$index == "member"))
  # The replica of an ARCTIC record is counted under ARCTIC, and the record
  # transferred to CA_OPC isn't counted under KNB.
  expect_equal(sum(dois$doi == "10.18739/a2replica"), 1)
  expect_equal(dois$node[dois$doi == "10.18739/a2replica"], "urn:node:ARCTIC")
  expect_false("10.5063/opc1" %in% dois$doi)
  # ARCTIC's new upload, not yet on the CN, is found in its own index.
  expect_true("10.18739/a2new" %in% dois$doi)
  expect_equal(dois$identifier[dois$doi == "10.18739/a2new"], "doi.org/10.18739/A2NEW")
})

test_that("excluded prefixes are skipped for their node", {
  server <- fake_solr(indexes)
  dois <- collect(server, "urn:node:KNB",
                  exclude_prefixes = list("urn:node:KNB" = c("10.6073/AA/", "doi:10.6085/AA/")))
  expect_false(any(startsWith(dois$doi, "10.6073/aa/") | startsWith(dois$doi, "10.6085/aa/")))
  sources <- attr(dois, "doi_sources")
  expect_equal(sources$excluded[sources$node == "urn:node:KNB"], 2)
  expect_equal(sources$dois[sources$node == "urn:node:KNB"], 7)
})

test_that("DOIs under DataCite's test prefix are skipped unless asked for", {
  server <- fake_solr(indexes)
  dois <- collect(server, "urn:node:KNB")
  expect_false("10.5072/fk2test" %in% dois$doi)
  expect_equal(attr(dois, "doi_sources")$test_dois, 1)
  dois <- collect(server, "urn:node:KNB", skip_test_dois = FALSE)
  expect_true("10.5072/fk2test" %in% dois$doi)
})

test_that("each DOI identifier is kept as its literal in DataONE, PID or SID", {
  server <- fake_solr(indexes)
  dois <- collect(server, "urn:node:KNB")
  identifier <- function(doi) dois$identifier[dois$doi == doi]
  expect_equal(identifier("10.5063/aa/v1"), "doi:10.5063/AA/v1")
  expect_equal(identifier("10.5063/series1"), "doi:10.5063/SERIES1")
  expect_equal(dois$field[dois$doi == "10.5063/series1"], "seriesId")
  expect_equal(dois$field[dois$doi == "10.5063/f1ser1"], "id")
  # A repository's own serialization is kept.
  expect_equal(identifier("10.5063/url1"), "https://doi.org/10.5063/URL1")
  expect_false("dataset" %in% names(dois))
})

test_that("when two literals are the same DOI, the doi: one is kept and the other noted", {
  server <- fake_solr(indexes)
  dois <- collect(server, "urn:node:KNB")
  row <- dois[dois$doi == "10.5063/twoforms", ]
  expect_equal(nrow(row), 1)
  expect_equal(row$identifier, "doi:10.5063/TWOFORMS")
  expect_equal(row$other_identifiers, "http://dx.doi.org/10.5063/TWOFORMS")
})

test_that("a node whose index refuses queries falls back to the CN", {
  server <- fake_solr(indexes)
  expect_warning(dois <- collect(server, "urn:node:ESS_DIVE"),
                 "urn:node:ESS_DIVE: using the CN's index.*refused the query.*HTTP 403",
                 class = "scythe_index_fallback")
  expect_equal(dois$doi, "10.15485/ess1")
  expect_equal(dois$index, "cn")
})

test_that("a node whose index has fewer authoritative records than the CN falls back to it", {
  server <- fake_solr(indexes)
  expect_warning(dois <- collect(server, "urn:node:GOA"),
                 "its index has 0 authoritative DOI records, but the CN has 1",
                 class = "scythe_index_fallback")
  expect_equal(dois$doi, "10.5063/goa1")
  expect_equal(attr(dois, "doi_sources")$index, "cn")
})

test_that("the other nodes come from the CN, leaving out the priority and excluded nodes", {
  server <- fake_solr(indexes)
  dois <- collect(server, "urn:node:KNB", other_nodes = "all", exclude_nodes = "urn:node:PANGAEA",
                  exclude_prefixes = list("urn:node:KNB" = c("10.6073/AA/", "10.6085/AA/")))
  others <- dois[dois$group == "other", ]
  expect_setequal(others$node, c("urn:node:ARCTIC", "urn:node:GOA", "urn:node:ESS_DIVE",
                                 "urn:node:PISCO", "urn:node:CA_OPC"))
  expect_true(all(others$index == "cn"))
  expect_false(any(dois$node == "urn:node:PANGAEA"))
  # KNB's mislabelled copy of the PISCO record is excluded, and the DOI is
  # collected once, under PISCO.
  expect_equal(dois$node[dois$doi == "10.6085/aa/pisco1"], "urn:node:PISCO")
})

test_that("listed other nodes are collected from the CN", {
  server <- fake_solr(indexes)
  dois <- collect(server, other_nodes = c("urn:node:PISCO", "urn:node:PANGAEA"))
  expect_setequal(dois$doi, c("10.6085/aa/pisco1", "10.1594/pangaea.1"))
})

test_that("a node missing from the node list falls back to the CN", {
  server <- fake_solr(indexes)
  expect_warning(collect(server, "urn:node:NOWHERE"), "isn't in the DataONE node list",
                 class = "scythe_index_fallback")
})

test_that("without exclusions, a priority node's copy of a DOI is preferred", {
  server <- fake_solr(indexes)
  dois <- collect(server, "urn:node:KNB", other_nodes = "urn:node:PISCO")
  expect_equal(dois$node[dois$doi == "10.6085/aa/pisco1"], "urn:node:KNB")
})

test_that("the summary covers every priority node, including ones with no DOIs", {
  server <- fake_solr(indexes)
  dois <- collect(server, c("urn:node:ARCTIC", "urn:node:SFWMD"))
  sources <- attr(dois, "doi_sources")
  expect_equal(sources$node, c("urn:node:ARCTIC", "urn:node:SFWMD"))
  expect_equal(sources$dois, c(3, 0))
  expect_equal(sources$cn_count, c(2, 0))
})

test_that("records are read a page at a time with a cursor", {
  server <- fake_solr(indexes)
  records <- with_fake_http(server, solr_doi_records(cn, "*:*", page_size = 4))
  expect_equal(nrow(records), nrow(cn_records))
  expect_setequal(records$id, cn_records$id)
  # 11 records in pages of 4, then an empty page that ends the cursor.
  expect_length(server$urls(), 4)
})

test_that("server errors from Solr are retried", {
  calls <- 0
  server <- fake_http(function(url, headers, n) {
    calls <<- calls + 1
    if (calls < 3) respond(503) else respond(body = list(response = list(numFound = 7, docs = list())))
  })
  expect_equal(with_fake_http(server, solr_count(cn, "*:*")), 7)
  expect_equal(calls, 3)
})

test_that("a DOI list can be saved and read back", {
  server <- fake_solr(indexes)
  dois <- collect(server, "urn:node:KNB")
  path <- file.path(withr::local_tempdir(), "cycle-1", "dois.csv")
  write_doi_list(dois, path)
  expect_equal(read_doi_list(path), dois, ignore_attr = TRUE)
})
