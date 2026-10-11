test_that("scythe finds multiple dois", {
  # Overall tests of the scythe package go here. These require the network, so
  # skip on CRAN
  skip_on_cran()
  
  # Springer and Scopus are only included locally, when their keys are set;
  # on CI they are rate limited, so this test uses only the keyless sources.
  keyed_sources <- live_keyed_sources()
  # OpenAlex is left out because it spends credits; test-openalex.R has its
  # own opt-in live tests.
  keyed_sources <- c(keyed_sources, "plos", "xdd")
  
  # Load a set of example citations that should be found by the API
  citations_file <-
    system.file("testdata", "test-citations.csv", package = "scythe")
  citations <- read.csv(citations_file, stringsAsFactors = FALSE)
  # Filter for test set contained within what we have keys set for
  citations <- dplyr::filter(citations, source %in% keyed_sources)
  expect_s3_class(citations, "data.frame")
  
  # remove non-doi identifiers
  doi <- citations %>%
    filter(grepl("^10\\.", dataone_pid))
  
  # stop test if NULL (needed API key not found)
  if (is.null(keyed_sources)) {
    message("API key is NULL for either scopus or springer")
  }
  # search for single known doi citation in specified library/source
  else{
    # Use scythe to search for each of the doi citations, and verify they are found
    # The sources' progress messages aren't needed; warnings still show.
    results_doi <-
      suppressMessages(citation_search(doi$dataone_pid, sources = keyed_sources))
    expect_true(all(doi$pub_id %in% results_doi$article_id))
    
  }
})
