#' Search for citations in text
#'
#' @param identifier a vector of identifiers to be searched for
#'
#' @return tibble of matching publication identifiers
#' @export
#' @import rplos
#' @examples
citation_search <- function(identifier) {
    #identifier = "10.18739/A22274"
    psearch <- searchplos(q=identifier, limit=1000)
    return(psearch$data)
}
