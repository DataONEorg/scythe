#' Search for citations in xDD
#'
#' This function searches for citations in xDD. Uses 'snippets/term' function in xDD API and
#' searches through all of xDD corpus (not limited to full-text documents).
#'
#' @param identifiers a vector of identifiers to be searched for, without hypertext
#' transfer protocol: "https://" or "http://"
#'
#' @return tibble of publications and their identifiers that contain
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR", "10.18739/A29K97")
#' result <- citation_search_xdd(identifiers)
#' }
#'
citation_search_xdd <- function(identifiers) {
  identifiers <- check_identifiers(identifiers)

  identifiers_enc <- utils::URLencode(identifiers, reserved = TRUE)
results <- list()

for (i in seq_along(identifiers_enc)) {
  results[[i]] <- tryCatch({
    fromJSON(curl(
      paste0(
        "https://xdd.wisc.edu/api/snippets?term=",
        identifiers[i],
        "&corpus=all"
      )
    ))
  }, error = function(e) {
    message("Failed to fetch identifier ", identifiers[i], ": ", e$message)
    NULL  # store NULL for failed calls
  })
}

# initialize df for storing results in orderly fashion
xdd_results <- data.frame(
  article_id = character(),
  article_title = character(),
  dataset_id = character(),
  source = character()
)


  # extract select information from results
  for (i in 1:length(results)) {
    if (length(results[[i]]$success$data) == 0 || is.null(results[[i]])) {
      # if no returned results, do this
      df <- data.frame(
        article_title = NA,
        article_id = NA,
        dataset_id = identifiers[i],
        source = "xdd"
      )
    } else if (nrow(results[[i]]$success$data) > 0) {
      # if results returned, do this
      article_id <- results[[i]]$success$data$doi
      article_title <- results[[i]]$success$data$title
      dataset_id <- identifiers[i]
      source <- "xdd"
      df <-
        data.frame(article_id, article_title, dataset_id, source)
    }

    xdd_results <-
      rbind(xdd_results, df) # bind subsequent results to previous results data.frame
    xdd_results <-
      xdd_results[complete.cases(xdd_results), ] # remove incomplete cases (NAs)
  }

  return(xdd_results)
}
