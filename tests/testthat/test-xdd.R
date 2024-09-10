test_that("xdd finds single known doi", {
  skip_on_cran()
  
  # Pull single test doi citation from test-citations.csv
  test_cit <- get_test_doi("xdd")
  
  # Search for single test doi using citation_search_springer() function
  xdd_res <-
    citation_test_doi("xdd") # Need to key API keys in order for this to work
  
  expect_true(test_cit$pub_id %in% xdd_res$article_id)
})
