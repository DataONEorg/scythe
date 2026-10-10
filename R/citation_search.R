#' Search for citations in text across all APIs
#'
#' @param identifiers a vector of identifiers to be searched for
#' @param sources a vector indicating which sources to query (one or
#' more of plos, scopus, springer, xdd, openalex). OpenAlex spends credits
#' from a daily budget, so it is only searched when named here; call
#' [citation_search_openalex()] directly to control its cost and to resume
#' an interrupted search.
#' @return tibble of matching dataset and publication identifiers
#'
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search(identifiers, sources = c("plos"))
#' }
citation_search <- function(identifiers,
                            sources = c("plos", "scopus", "springer", "xdd")) {
  stopifnot(is.character(identifiers))

  unid <- sources[!(sources %in% scythe_sources)]
  if (length(unid) > 0) {
    stop(paste("Source", unid, "is not recognized.", collapse = ". "))
  }

  search_funs <- sapply(sources, function(source) {
    get(paste0("citation_search_", source), mode = "function")
  })

  # Run each search, producing a list of dataframes
  result_df_list <- lapply(search_funs, function(search_fun) {
    df <- search_fun(identifiers)
    df[] <- lapply(df, as.character)  # Coerce all columns to character
    return(df)
  })

  # Combine the resulting data frames and return the result df
  if (all(sapply(result_df_list, function(x) nrow(x) == 0))) {
      # If all data frames are empty, create an empty data frame with the same structure
      result <- data.frame()
  } else {
      # Otherwise, bind the rows
      result <- dplyr::bind_rows(result_df_list)
  }

  return(result)
}

# The sources that citation_search() can query; each one has a matching
# citation_search_<source>() adapter.
scythe_sources <- c("plos", "scopus", "springer", "xdd", "openalex")

# Check identifiers to remove characters that interfere with query strings

check_identifiers <- function(identifiers) {
  is_uuid <- grepl("^urn:uuid:", identifiers, ignore.case = TRUE)
  if (any(is.na(normalize_doi(identifiers)) & !is_uuid)) {
    warning(
      call. = FALSE,
      "One or more identifiers does not appear to be a DOI or uuid",
      immediate. = TRUE
    )
  }

  identifiers <- gsub("(doi:\\s*)|(urn:uuid:)", "", identifiers, ignore.case = TRUE)


  return(identifiers)
}
