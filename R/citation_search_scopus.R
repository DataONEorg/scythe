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
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_scopus(identifiers)
#' }
citation_search_scopus <- function(identifiers) {
  identifiers <- check_identifiers(identifiers)

  wait_seconds <- 0.12
  report_est_wait(length(identifiers), wait_seconds)

  key <- scythe_get_key("scopus")
  if (is.na(key)) {
    warning(
      "Skipping Scopus search due to missing API key. Set an API key using scythe_set_key() to include Scopus results."
    )
    return()
  }
  identifiers_enc <- utils::URLencode(identifiers, reserved = TRUE)

  results <- list()
  for (i in 1:length(identifiers_enc)) {
    Sys.sleep(0.12)
    results[[i]] <-
      fromJSON(curl(
        paste0(
          "https://api.elsevier.com/content/search/scopus?query=ALL:",
          identifiers_enc[i],
          paste("&APIKey=", key, sep = "")
        )
      ))
  }

  # initialize df for storing results in orderly fashion
  scopus_results <- data.frame(
    article_id = character(),
    article_title = character(),
    dataset_id = character(),
    source = character()
  )

  # extract relevant information from raw results
  for (i in 1:length(results)) {
    article_id <-
      results[[i]][["search-results"]][["entry"]][["prism:doi"]]
    article_title <-
      results[[i]][["search-results"]][["entry"]][["dc:title"]]

    num_citations <- length(article_id)

    dataset_id <- rep(identifiers[i], num_citations)
    source <- rep("scopus", num_citations)
    scopus_results <- rbind(
      scopus_results,
      data.frame(article_id, article_title, dataset_id, source)
    )
  }

  # clean up dois
  scopus_results$dataset_id <-
    gsub("ALL:", "", scopus_results$dataset_id)

  return(scopus_results)
}
