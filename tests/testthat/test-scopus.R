test_that("scopus finds single known doi", {
  
  skip_on_cran()
  
  # Pull single test doi citation from test-citations.csv
  test_cit <- get_test_doi("scopus")
  
  # Search for single test doi using citation_search_springer() function
  scopus_res <- citation_test_doi("scopus") # Need to key API keys in order for this to work
  
  if(!is.null(scopus_res)){
    expect_true(test_cit$pub_id %in% scopus_res$article_id)
  }
})

