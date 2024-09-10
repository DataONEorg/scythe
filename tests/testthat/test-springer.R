test_that("springer finds single known doi", {
  skip_on_cran()
  
  key <- scythe_get_key("springer")
  
  if (is.na(key)){
      skip()
      message("No Springer key set. Skipping.")
  }
    
  # Pull single test doi citation from test-citations.csv
  test_cit <- get_test_doi("springer")
  
  # Search for single test doi using citation_search_springer() function
  springer_res <-
    citation_test_doi("springer") # Need to key API keys in order for this to work
  # do not run test if API key NULL
  if (!is.null(springer_res)) {
    expect_true(test_cit$pub_id %in% springer_res$article_id)
  }
})
