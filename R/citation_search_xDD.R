#' Search for citations in xDD
#'
#' This function searches for citations in xDD. Not sure if Requests are throttled
#' x requests/second so as to not overload the xDD API.
#'
#' @param identifiers a vector of identifiers to be searched for
#'
#' @return tibble of matching dataset and publication identifiers
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_xDD(identifiers)
#' }
test_citations <- read_csv("inst/testdata/test-citations.csv")
identifiers <- test_citations[[1,6]] # [row,column]
results_test <- jsonlite::fromJSON(curl::curl(paste0("https://xdd.wisc.edu/api/snippets?term=", identifiers[1]))) #list


citation_search_xDD <- function(identifiers) {
  
  identifiers <- check_identifiers(identifiers)
  
 # if (length(identifiers) > 8){
 #   message(paste0("Your result will take ~", length(identifiers)/9 ," seconds to return, since this function is rate limited to 9 calls per second."))
 # }
  
  identifiers_enc <- lapply(identifiers, utils::URLencode, reserved = TRUE) # URLencode - percent-encode in URL
  identifiers_enc <- unlist(identifiers_enc) # given a list, produces a vector
  results <- list()
  for (i in 1:length(identifiers_enc)) {
    Sys.sleep(0.12)
    results[[i]] <-
      fromJSON(curl(paste0("https://xdd.wisc.edu/api/snippets?term=", identifiers[i])))
  }
  
  # initialize df for storing results in orderly fashion
  xDD_results <- data.frame(article_id = character(),
                            article_title = character(),
                            dataset_id = character())
  
  # extract relevant information from raw results 
  for (i in 1:length(results)) {
    num_citations <- as.numeric(length(results))
 # Check here for results extraction example https://github.com/trashbirdecology/bbl_xdd/blob/master/R/get_xdd_df.r   
    article_id <- results[[i]]$success$data$doi
    article_title <- results[[i]]$success$data$title
    dataset_id <- rep(identifiers[[i]], times = num_citations)
    xDD_results <- rbind(xDD_results, data.frame(article_id,article_title,dataset_id))
  }
  
  # clean up dois
  xDD_results$dataset_id <- gsub("ALL:", "", xDD_results$dataset_id)
  
  return(xDD_results)
}
