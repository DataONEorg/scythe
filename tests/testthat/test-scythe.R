test_that("scythe", {

    # Overall tests of the scythe package go here. These require the network, so
    # skip on CRAN
    skip_on_cran()

    # For now, ensure the test framework works
    expect_equal(2 * 2, 4)

    # Load a set of example citations that should be found by the API
    citations_file <- system.file("testdata","test-citations.csv",package="scythe")
    citations <- read.csv(citations_file, stringsAsFactors = FALSE)
    expect_s3_class(citations, "data.frame")

    # TODO: Use scythe to search for each of these citations, and verify they are found

})
