test_that("scythe", {

    # Overall tests of the scythe package go here. These require the network, so
    # skip on CRAN
    skip_on_cran()

    # Determine which keys have been set
    keyed_sources <- unlist(lapply(c("scopus", "springer"), function(x){
        if (!is.na(scythe_get_key(x))){
            return(x)
        }
        else return(NULL)
    }))
    keyed_sources <- c(keyed_sources, "plos", "xdd")

    # Load a set of example citations that should be found by the API
    citations_file <- system.file("testdata","test-citations.csv", package="scythe")
    citations <- read.csv(citations_file, stringsAsFactors = FALSE)
    # Filter for test set contained within what we have keys set for
    citations <- dplyr::filter(citations, source %in% keyed_sources)
    expect_s3_class(citations, "data.frame")
    # filter doi identifiers only from test set
    doi <- citations %>% 
      filter(dataone_pid != "arctic-data.6185.2")
    
    # Use scopus function to search for single known dataset citation, verify it is found
    scopus_res <- citation_search_scopus("10.18739/A2ZS2KC8C")
    expect_true("10.1029/2018JG004528" %in% scopus_res$article_id)
    
    # Use springer function to search for single known citation, verify it is found
    springer_res <- citation_search_springer("10.18739/A2M32N95V")
    expect_true("10.1007/s13280-019-01295-7" %in% springer_res$article_id)
    
    # Use plos function to search for single known citation, verify it is found
    plos_res <- citation_search_plos("10.18739/A22274")
    expect_true("10.1371/journal.pone.0213037" %in% plos_res$article_id)
    
    # Use xdd function to search for single known citation, verify it is found
    xdd_res <- citation_search_xdd("10.18739/A29K97")
    expect_true("10.1029/2018JG004528" %in% xdd_res$article_id)
    
    # Use scythe to search for a single citation in plos, and verify it is found
    identifier <- "10.18739/A22274"
    results <- citation_search(identifier, sources = "plos")
    expect_true("10.1371/journal.pone.0213037" %in% results$article_id)

    # Use scythe to search for each of the doi citations, and verify they are found
    results_doi <- citation_search(doi$dataone_pid, sources = keyed_sources)
    expect_true(all(doi$pub_id %in% results_doi$article_id))
    
})
