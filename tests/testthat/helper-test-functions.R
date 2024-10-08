#' Internal helper function for testing
#'
#' @param source a character string of a single source source ("xdd", "plos", "scopus", "springer")
#' @return tibble of single test doi citation known to occur in source library
#'

get_test_doi <- function(library) {
  # use library as function parameter in place of source to not mess with filter
  # Load a set of example citations that should be found by the API
  citations_file <-
    system.file("testdata", "test-citations.csv", package = "scythe")
  citations <- read.csv(citations_file, stringsAsFactors = FALSE)
  
  one_citation <- citations %>%
    filter(grepl("^10\\.", dataone_pid), # remove non-doi identifiers
           source == c(library)) # only need one doi
  # select only one example
  one_citation <- one_citation[1, ]
  
  return(one_citation)
}

#' Internal helper function for testing
#'
#' Switch designed to help testing sources that require API keys and stop R-CMD-check
#' from failing from its inability to access API key.
#'
#' @param source a character string of a single source source ("xdd", "plos", "scopus", "springer")
#' @return single character string of library/source. Will return NULL if require API key not set
#'
#'
get_test_key <- function(source) {
  if (source %in% c("scopus", "springer")) {
    if (!is.na(scythe_get_key(source))) {
      return(source)
    } else{
      NULL
    }
  }
  else
    return(source)
}


#' Internal helper function for testing
#'
#' @param source a character string of a single source source ("xdd", "plos", "scopus", "springer")
#' @return tibble of search results produced by single test doi citation
#'

citation_test_doi <- function(source) {
  # pull one known citation from specified library/source
  one_citation <- get_test_doi(source)
  # pull the dataset doi from known citation
  one_doi <- one_citation$dataone_pid
  # pull API keys for sources that need them
  keyed_source <- get_test_key(source)
  
  # stop test if NULL (needed API key not found)
  if (is.null(keyed_source)) {
    message(paste0("API key is NULL for ", source))
  }
  # search for single known doi citation in specified library/source
  else{
    search <-
      paste0(keyed_source,
             "<- citation_search_",
             keyed_source,
             "(one_doi)")
    result <- eval(parse(text = search))
    result
  }
}
