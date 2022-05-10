test_that("springer finds single known doi", {
  
  skip_on_cran()
  
  # Pull single test doi citation from test-citations.csv
  test_cit <- get_test_doi("springer")
  
  # Search for single test doi using citation_search_springer() function
  springer_res <- citation_test_doi("springer") # Need to key API keys in order for this to work
  
  expect_true(test_cit$pub_id %in% springer_res$article_id)
})

