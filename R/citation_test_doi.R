citation_test_doi <- function(library){
  
  # Load a set of example citations that should be found by the API
  citations_file <- system.file("testdata","test-citations.csv", package="scythe")
  citations <- read.csv(citations_file, stringsAsFactors = FALSE)
  
  one_citation <- citations %>% 
    filter(grepl("^10\\.", dataone_pid), # remove non-doi identifiers
           source == c(library)) # only need one doi
  
  one_citation <- one_citation[1,] # select only one example
  
  one_doi <- one_citation %>% 
    select(dataone_pid)
  
  one_article <- one_citation %>% 
    select(pub_id)
  
  search <- paste0(library, "<- citation_search_", library, "(one_doi)")
  result <- eval(parse(text = search))
  
  return(result)
}