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
                            sources = c("plos", "scopus", "springer")) {
  
  if(!("character" %in% class(identifiers))){
    stop("Identifiers must be a character vector.")
  }
  
  # run the 'citation_search_*' function for each source
  for (source in sources) {
    search_function <- paste0(source, " <- citation_search_", source, "(identifiers)")
    eval(parse(text = search_function))
  }

  # combine all of the resulting data frames and return the result df
  bind_function <- paste0("rbind(", paste0(sources, collapse = ","), ")")
  result <- eval(parse(text = bind_function))

  return(result)

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
    message("Identifier prefix (doi: or urn:uuid) has been stripped out of the search term.")
  }


  return(identifiers)
}


