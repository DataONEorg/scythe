#' Search for citations in text across all APIs
#'
#' @param identifiers a vector of identifiers to be searched for
#' @param sources a vector indicating which sources to query (one or more of plos, scopus, springer)
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
    
    unid <- sources[!(sources %in% c("plos", "scopus", "springer", "xdd"))]
    if (length(unid) > 0) {
        stop(paste("Source", unid, "is not recognized.", collapse = ". "))
    }
  
    search_funs <- sapply(sources, function(source)
        get(paste0("citation_search_", source), mode="function"))
    
    # Run each search, producing a list of dataframes
    result_df_list <- lapply(search_funs,
                             function(search_fun) search_fun(identifiers))
    
    # Combine the resulting data frames and return the result df
    result <- dplyr::bind_rows(result_df_list)
  
  return(result)
  
}

# Check identifiers to remove characters that interfere with query strings

check_identifiers <- function(identifiers) {
  if (any(!grepl("10\\.|urn:uuid", identifiers))) {
    warning(
      call. = FALSE,
      "One or more identifiers does not appear to be a DOI or uuid",
      immediate. = TRUE
    )
  }
  
  if (any(grepl("doi:|urn:uuid", identifiers))) {
    identifiers <- gsub("(doi:)|(urn:uuid:)", "", identifiers)
  }
  
  
  return(identifiers)
}
