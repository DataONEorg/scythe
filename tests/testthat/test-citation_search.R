test_that("citation_search identifer input looks like an identifier", {
    
    identifiers <- c("some text", "10.18739/A25D8NF4H")

    expect_error(citation_search(identifiers))
})
