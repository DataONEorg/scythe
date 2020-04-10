#' Search for citations in text
#'
#' @param identifier a vector of identifiers to be searched for
#'
#' @return tibble of matching publication identifiers
#' @export
#' @importFrom fulltext ft_search
#' @examples
citation_search <- function(identifier) {
    results <- fulltext::ft_search(identifier, from=c("plos"))
    return(results$plos$data)
}
