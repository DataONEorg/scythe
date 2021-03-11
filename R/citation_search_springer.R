#' Search for citations from Springer
#'
#' This function searches for citations from Springer. It requires that an API key
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
#' result <- citation_search_springer(identifiers)
#'
citation_search_springer <- function(identifiers) {
    if (length(identifiers) > 1){
        message(paste0("Your result will take ~", length(identifiers)*1 ," seconds to return, since this function is rate limited to one call every second."))
    }

    identifiers <- check_identifiers(identifiers)

    if (Sys.getenv("springer_key") != ""){
        tmp <- Sys.getenv("springer_key")
    } else {
        tmp <- tryCatch(
            {
                keyring::key_get("springer_key", keyring = "scythe")
            },
            error=function(cond) {
                #message(paste("Key does not seem to exist in keyring"))
                return(NA)
            })
    }
    if (is.na(tmp)) {
        warning("Skipping Springer search due to missing API key. Set an API key using scythe_set_key() to include Springer results.")
        return()
    }

    identifiers_enc <- utils::URLencode(identifiers, reserved = T)
    
    results <- list()
    for (i in 1:length(identifiers_enc)) {
        Sys.sleep(1)
        results[[i]] <- jsonlite::fromJSON(curl::curl(paste0("http://api.springernature.com/meta/v2/json?q=",
                                                             identifiers[i],
                                                             "&api_key=",
                                                             tmp,
                                                             "&p=100")
        ))
    }


    # assign dataset identifier to each result
    springer_results <- list()
    for (i in 1:length(results)){
        if (as.numeric(results[[i]]$result$total) == 0 | is.null(results[[i]])){
            springer_results[[i]] <- data.frame(article_id = NA,
                                                dataset_id = identifiers[i],
                                                article_title = NA)
        }
        else if (as.numeric(results[[i]]$result$total) > 0){

            springer_results[[i]] <- data.frame(article_id = rep(NA, as.numeric(results[[i]]$result$total)),
                                                dataset_id = rep(NA, as.numeric(results[[i]]$result$total)),
                                                article_title = rep(NA, as.numeric(results[[i]]$result$total)))

            springer_results[[i]]$article_id <- results[[i]]$records$identifier
            springer_results[[i]]$article_title <- results[[i]]$records$title
            springer_results[[i]]$dataset_id <- identifiers[i]
        }

    }

    # bind resulting tibbles
    springer_results <- do.call(rbind, springer_results)

    # remove doi: prefix for consistency
    springer_results$article_id <- gsub("doi:", "", springer_results$article_id)

    return(springer_results)


}
