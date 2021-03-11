test_that("scythe", {

    # Overall tests of the scythe package go here. These require the network, so
    # skip on CRAN
    skip_on_cran()

    library(purrr)
    
    # Determine which keys have been set
    t <- scythe_get_key()
    keyed_sources <- c(gsub("_key", "", names(which(t != ""))), "plos")

    # Load a set of example citations that should be found by the API
    citations_file <- system.file("testdata","test-citations.csv",package="scythe")
    citations <- read.csv(citations_file, stringsAsFactors = FALSE)
    # Filter for test set contained within what we have keys set for
    citations <- dplyr::filter(citations, source %in% keyed_sources)
    expect_s3_class(citations, "data.frame")

    # Use scythe to search for a single citation, and verify it is found
    identifier <- "10.18739/A22274"
    results <- citation_search(identifier, sources = "plos")
    expect_true("10.1371/journal.pone.0213037" %in% results$article_id)
    


    # Use scythe to search for each of the citations, and verify they are found
    pmap(citations, function(...) {
        current <- dplyr::tibble(...)
        results <- suppressWarnings(citation_search(current$dataone_pid, 
                                                    sources = keyed_sources))
        expect_true(current$pub_id %in% results$article_id)
    })
})
