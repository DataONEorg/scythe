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
#' @importFrom stats complete.cases
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_springer(identifiers)
#' }
citation_search_springer <- function(identifiers) {
  wait_seconds <- 1
  report_est_wait(length(identifiers), wait_seconds)

  identifiers <- check_identifiers(identifiers)
  
  # initialize df for storing results in orderly fashion
  springer_results <- data.frame(
      article_id = character(),
      article_title = character(),
      dataset_id = character(),
      source = character()
  )

  key <- scythe_get_key("springer")
  if (is.na(key)) {
    warning(
      "Skipping Springer search due to missing API key. Set an API key using scythe_set_key() to include Springer results."
    )
    return(springer_results)
  }

  identifiers_enc <- utils::URLencode(identifiers, reserved = TRUE)

  results <- list()
  for (i in 1:length(identifiers_enc)) {
    Sys.sleep(wait_seconds)
    results[[i]] <-
      jsonlite::fromJSON(curl::curl(
        paste0(
          "http://api.springernature.com/meta/v2/json?q=",
          identifiers[i],
          "&api_key=",
          key
        )
      ))
  }


  # assign dataset identifier to each result
  springer_results <- list()
  for (i in 1:length(results)) {
    if (as.numeric(results[[i]]$result$total) == 0 |
      is.null(results[[i]])) {
      springer_results[[i]] <- data.frame(
        article_id = NA,
        article_title = NA,
        dataset_id = identifiers[i],
        source = "springer"
      )
    } else if (as.numeric(results[[i]]$result$total) > 0) {
      springer_results[[i]] <-
        data.frame(
          article_id = rep(NA, as.numeric(results[[i]]$result$total)),
          article_title = rep(NA, as.numeric(results[[i]]$result$total)),
          dataset_id = rep(NA, as.numeric(results[[i]]$result$total)),
          source = rep("springer", as.numeric(results[[i]]$result$total))
        )

      springer_results[[i]]$article_id <-
        results[[i]]$records$identifier
      springer_results[[i]]$article_title <-
        results[[i]]$records$title
      springer_results[[i]]$dataset_id <- identifiers[i]
    }
  }

  # bind resulting tibbles
  springer_results <- do.call(rbind, springer_results)

  # remove doi: prefix for consistency
  springer_results$article_id <-
    gsub("doi:", "", springer_results$article_id)

  # drop NA results
  springer_results <-
    springer_results[complete.cases(springer_results), ]

  return(springer_results)
}
