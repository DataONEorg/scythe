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
    
    results <- rbind(citation_search_plos(identifiers), 
                     citation_search_bmc(identifiers)
                     )
    
}


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

#' Search for citations in BMC
#' 
#' This function searches for citations in BMC. It requires that an API key
#' be obtained from [Springer](https://dev.springernature.com/) and set using
#' `scythe_set_key()`. Requests are throttled at one identifier every second
#' so as to not overload the PLOS API.
#'
#' @param identifiers a vector of identifiers to be searched for
#'
#' @return tibble of matching dataset and publication identifiers
#' @importFrom jsonlite fromJSON
#' @importFrom curl curl
#' @export

#' @examples
#' 
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_bmc(identifiers)
#' 
citation_search_bmc <- function(identifiers) {
  if (length(identifiers) > 1){
    message(paste0("Your result will take ~", length(identifiers)*1 ," seconds to return, since this function is rate limited to one call every second."))
  }
  
  identifiers <- check_identifiers(identifiers)
  
  tmp <- keyring::key_get("bmc_key", keyring = "scythe")
  
  results <- list()
  for (i in 1:length(identifiers)) {
    Sys.sleep(1)
    results[[i]] <- jsonlite::fromJSON(curl::curl(paste0("http://api.springernature.com/meta/v2/json?q=",
                                   identifiers[i],
                                   "&api_key=",
                                   tmp,
                                   "&p=100")
      ))
  }
  

  # assign dataset identifier to each result
  bmc_results <- list()
  for (i in 1:length(results)){
    if (as.numeric(results[[i]]$result$total) == 0 | is.null(results[[i]])){
      bmc_results[[i]] <- data.frame(article_id = NA,
                                      dataset_id = identifiers[i],
                                     article_title = NA)
    }
    else if (as.numeric(results[[i]]$result$total) > 0){
      
      bmc_results[[i]] <- data.frame(article_id = NA,
                                     dataset_id = NA,
                                     article_title = NA)
      
      bmc_results[[i]]$article_id <- results[[i]]$records$identifier
      bmc_results[[i]]$article_title <- results[[i]]$records$title
      bmc_results[[i]]$dataset_id <- identifiers[i]
    }
    
  }
  
  # bind resulting tibbles
  bmc_results <- do.call(rbind, bmc_results)
  
  # remove doi: prefix for consistency
  bmc_results$article_id <- gsub("doi:", "", bmc_results$article_id)
  
  return(bmc_results)
  
  
}

# Check identifiers to remove characters that interfere with query strings

check_identifiers <- function(identifiers){
  if (any(!grepl("10\\.|urn:uuid", identifiers))){
    warning(call. = FALSE,
            "One or more identifiers does not appear to be a DOI or uuid",
            immediate. = TRUE)
  }
  
  if (any(grepl("doi:|urn:uuid", identifiers))){
    identifiers <- gsub("(doi:)|(urn:uuid:)", "", identifiers)
  }
  if (any(grepl(":", identifiers))){ 
    identifiers <- gsub(":", "", identifiers) # handle other problematic colons
  }
  
  return(identifiers)
}


