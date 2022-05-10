test_that("plos finds single known doi", {
  
  skip_on_cran()
  
  # Pull single test doi citation from test-citations.csv
  test_cit <- get_test_doi("plos")
  
  # Search for single test doi using citation_search_springer() function
  plos_res <- citation_test_doi("plos") # Need to key API keys in order for this to work
  
  expect_equal(test_cit$pub_id, plos_res$article_id)
})
