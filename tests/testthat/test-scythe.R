test_that("scythe finds multiple dois", {
  # Overall tests of the scythe package go here. These require the network, so
  # skip on CRAN
  skip_on_cran()
  
  # Determine which keys have been set
  keyed_sources <-
    unlist(lapply(c("scopus", "springer"), function(x) {
      if (!is.na(scythe_get_key(x))) {
        return(x)
      }
      else
        return(NULL)
    }))
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
    results_doi <-
      citation_search(doi$dataone_pid, sources = keyed_sources)
    expect_true(all(doi$pub_id %in% results_doi$article_id))
    
  }
})
