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
#' results_list <- jsonlite::fromJSON(curl::curl(paste0("https://xdd.wisc.edu/api/snippets?term=", "10.18739/A2D08X")))
#' }
#' 
#test_citations <- read_csv("inst/testdata/test-citations.csv")
#test_citations <- test_citations[,6]

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
  xDD_results <- data.frame(dataset_id = character(),
                            article_id = character(),
                            article_title = character())
  
  # extract relevant information from raw results 
  for (i in 1:length(results)) {
    if (length(results[[i]]$success$data) == 0 | is.null(results[[i]]$success$data)){
      df <- data.frame(dataset_id = identifiers[i],
                                article_id = NA,
                                article_title = NA)
    }
    else if (length(results[[i]]$success$data > 0)){    
    dataset_id <- identifiers[i] #rep(identifiers[[i]], times = num_citations)
    article_id <- results[[i]]$success$data$doi
    article_title <- results[[i]]$success$data$title
    df <- data.frame(dataset_id, article_id, article_title)
    }
    
  xDD_results <- rbind(xDD_results, df)
  }
  
  return(xDD_results)
}
