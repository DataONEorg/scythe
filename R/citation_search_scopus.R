#' Search for citations in Scopus
#'
#' This function searches for citations in Scopus. Requests are throttled
#' 9 requests/second so as to not overload the Scopus API.
#'
#' @param identifiers a vector of identifiers to be searched for
#'
#' @return tibble of matching dataset and publication identifiers
#' @export
#' @examples
#'
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_scopus(identifiers)
#'
citation_search_scopus <- function(identifiers) {

    check_identifiers(identifiers)

    if (length(identifiers) > 8){
        message(paste0("Your result will take ~", length(identifiers)/9 ," seconds to return, since this function is rate limited to 9 calls per second."))
    }

    tmp <- NA
    if (Sys.getenv("scopus_key") != ""){
        tmp <- Sys.getenv("scopus_key")
    } else {
        tmp <- tryCatch(
            {
                keyring::key_get("scopus_key", keyring = "scythe")
            },
            error=function(cond) {
                #message(paste("Key does not seem to exist in keyring"))
                return(NA)
            })
    }
    if (is.na(tmp)) {
        warning("Skipping Scopus search due to missing API key. Set an API key using scythe_set_key() to include Scopus results.")
        return()
    }
    identifiers_enc <- URLencode(identifiers, reserved = TRUE)
    results <- list()
    for (i in 1:length(identifiers_enc)) {
        Sys.sleep(0.12)
        results[[i]] <-
            fromJSON(curl(paste0("https://api.elsevier.com/content/search/scopus?query=ALL:", identifiers[i], paste("&APIKey=",tmp, sep=""))))
    }

    # initialize df for storing results in orderly fashion
    scopus_results <- data.frame(article_id = character(),
                                 article_title = character(),
                                 dataset_id = character())

    # extract relevant information from raw results
    for (i in 1:length(results)) {
        num_citations <- as.numeric(results[[i]][["search-results"]][["opensearch:totalResults"]])

        article_id <- results[[i]][["search-results"]][["entry"]][["prism:doi"]]
        article_title <- results[[i]][["search-results"]][["entry"]][["dc:title"]]
        dataset_id <- rep(identifiers[i], num_citations)
        scopus_results <- rbind(scopus_results, data.frame(article_id,article_title,dataset_id))
    }

    # clean up dois
    scopus_results$dataset_id <- gsub("ALL:", "", scopus_results$dataset_id)

    return(scopus_results)
}
