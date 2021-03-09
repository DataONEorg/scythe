#' Search for citations in PLOS
#' 
#' This function searches for citations in PLOS. Requests are throttled
#' at one identifier every 6 seconds so as to not overload the PLOS
#' API.
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
    if (length(identifiers) > 1){
        message(paste0("Your result will take ~", length(identifiers)*6 ," seconds to return,
                   since this function is rate limited to one call every 6 seconds."))
    }
    
    identifiers <- check_identifiers(identifiers)
    
    # search for identifier
    results <- lapply(identifiers, function(x){
        Sys.sleep(6)
        v <- rplos::searchplos(q = x,
                               fl = c("id","title"),
                               limit = 1000)
        return(v)
        
    }
    )
    
    plos_results <- list()
    # assign dataset identifier to each result
    for (i in 1:length(results)){
        if (results[[i]]$meta$numFound == 0 | is.null(results[[i]])){
            plos_results[[i]] <- data.frame(id = NA,
                                            dataset_id = identifiers[i],
                                            title = NA)
        }
        else if (results[[i]]$meta$numFound > 0){
            plos_results[[i]] <- results[[i]]$data
            plos_results[[i]]$dataset_id <- identifiers[i]
        }
        
    }
    
    # bind resulting tibbles
    plos_results <- do.call(rbind, plos_results)
    names(plos_results)[which(names(plos_results) == "id")] <- "article_id"
    names(plos_results)[which(names(plos_results) == "title")] <- "article_title"
    
    return(plos_results)
}