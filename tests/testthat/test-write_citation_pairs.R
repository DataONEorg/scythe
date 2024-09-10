test_that("write_citation_pairs checks input correctly", {
  pairs <- data.frame(test_name = "10.1371/journal.pone.0213037",
                      dataset_id = "10.18739/A22274")
  
  expect_error(write_citation_pairs(citation_list = pairs, path = t))
  
})
