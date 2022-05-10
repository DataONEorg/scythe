#' Internal helper functions for testing 
#' 
#' @param library a character string of a single library source ("xdd", "plos", "scopus", "springer")
#' @return tibble of single test doi citation known to occur in source library
#'

get_test_doi <- function(library){
  # Load a set of example citations that should be found by the API
  citations_file <- system.file("testdata","test-citations.csv", package="scythe")
  citations <- read.csv(citations_file, stringsAsFactors = FALSE)
  
  one_citation <- citations %>% 
    filter(grepl("^10\\.", dataone_pid), # remove non-doi identifiers
           source == c(library)) # only need one doi
  # select only one example
  one_citation <- one_citation[1,] 
  
  return(one_citation)
}

#' Internal helper functions for testing 
#' 
#' @param library a character string of a single library source ("xdd", "plos", "scopus", "springer")
#' @return tibble of search results produced by single test doi citation
#'

citation_test_doi <- function(library){
  # pull one known citation from specified library/source
  one_citation <- get_test_doi(library)
  # pull the dataset doi from known citation
  one_doi <- one_citation$dataone_pid 
  # get api access key if needed for library
  if (!is.na(scythe_get_key(library))){
    message (paste0( library, " has API key set"))
  }
  else message(paste0(library, " does NOT have API key set, or is not required"))
  # search for single known doi citation in specified library/source
  search <- paste0(library, "<- citation_search_", library, "(one_doi)")
  result <- eval(parse(text = search))
  
  return(result)
}


