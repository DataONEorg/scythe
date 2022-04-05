#' Search for citations in xDD
#'
#' This function searches for citations in xDD. Uses 'snippets/term' function in xDD API
#'
#' @param identifiers a vector of identifiers to be searched for. Without hypertext
#' Transfer Protocol "https://" or "http://" 
#'
#' @return tibble of publications and identifiers that site input dataset and publication identifiers
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_xDD(identifiers)
#' }
#'

citation_search_xDD <- function(identifiers) {
  
  identifiers <- check_identifiers(identifiers)
  
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
  
  # extract select information from results 
  for (i in 1:length(results)) {
    if (length(results[[i]]$success$data) == 0){ # if no returned results, do this
      df <- data.frame(dataset_id = identifiers[i],
                                article_id = NA,
                                article_title = NA)
    }
    else if (nrow(results[[i]]$success$data) > 0){ # if results returned, do this
    dataset_id <- identifiers[i]
    article_id <- results[[i]]$success$data$doi
    article_title <- results[[i]]$success$data$title
    df <- data.frame(dataset_id, article_id, article_title)
    }
    
    else (print("input does not meet either conditions within if else if statements")) # need closing else statement?
    
  xDD_results <- rbind(xDD_results, df) # bind subsequent results to previous results data.frame
  }
  
  return(xDD_results)
}
