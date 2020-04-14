#' Search for citations in text across all APIs
#'
#' @param identifiers a vector of identifiers to be searched for
#'
#' @return tibble of matching dataset and publication identifiers
#' @export
#' @examples
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_plos(identifiers)
citation_search <- function(identifiers) {
    
    if (any(!grepl("10\\.|urn:uuid", identifiers))){
        warning(call. = FALSE, "One or more identifiers does not appear to be a DOI or uuid")
    }
    
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
#' @examples
#' 
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_plos(identifiers)
#' 
citation_search_plos <- function(identifiers) {
    
    if (any(!grepl("10\\.|urn:uuid", identifiers))){
        warning(call. = FALSE, "One or more identifiers does not appear to be a DOI or uuid")
    }
  
    # search for identifier
    results <- lapply(identifiers, function(x){
     rplos::searchplos(q = x,
                       fl = "id",
                       limit = 1000)
        }
    )
    
    plos_results <- list()
    # assign dataset identifier to each result
    for (i in 1:length(results)){
        if (results[[i]]$meta$numFound == 0){
            plos_results[[i]] <- data.frame(id = NA, dataset_id = identifiers[i])
        }
        else if (results[[i]]$meta$numFound > 0){
            plos_results[[i]] <- results[[i]]$data
            plos_results[[i]]$dataset_id <- identifiers[i]
        }

    }
    # bind resulting tibbles
    plos_results <- do.call(rbind, plos_results)
    names(plos_results)[which(names(plos_results) == "id")] <- "article_id"
    
    return(plos_results)
}