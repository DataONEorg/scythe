#' Search for citations in text across all APIs
#'
#' @param identifiers a vector of identifiers to be searched for
#'
#' @return tibble of matching dataset and publication identifiers
#' @export
#' @examples
citation_search <- function(identifiers) {
    
    results <- rbind(citation_search_plos(identifiers) 
                     #, citation_search_scopus(idenfiers), ...
    )
    
}


#' Search for citations in PLOS
#'
#' @param identifiers a vector of identifiers to be searched for
#'
#' @return tibble of matching dataset and publication identifiers
#' @export
#' @importFrom rplos searchplos
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @examples
#' 
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_plos(identifiers)
#' 
citation_search_plos <- function(identifiers) {
  
    # search for identifier
    results <- lapply(identifiers, function(x){
     rplos::searchplos(q = x,
                       fl = "id",
                       fq = c("body", "supporting_information", "reference"),
                       limit = 1000)
        }
    )
     
    plos_results <- list()
    # assign dataset identifier to each result
    for (i in 1:length(identifier)){
        plos_results[[i]] <- results[[i]]$data %>% 
            dplyr::mutate(dataset_id = identifier[i])
    }
    # bind resulting tibbles
    plos_results <- do.call(rbind, plos_results) %>% 
        rename(article_id = id)
    
    return(plos_results)
}