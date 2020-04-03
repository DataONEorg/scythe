#' Write citation pairs
#'
#' @import rcrossref
#' @import jsonlite
#' @import bib2df
#' @import dplyr
#' @importFrom utils read.csv page
#' @importFrom graphics title
write_citation_pairs <- function() {

    # manually generated citations list
    cit <- read.csv("DBO_citations/citationlist.csv", stringsAsFactors = F)

    # write list of citations to bib format
    bib <- cr_cn(dois = cit$publicationDOI, format = "bibtex")
    writeLines(unlist(bib), "DBO_citations/all_citations.bib" )

    # import as a dataframe
    df <- bib2df("~/dataone-citations/DBO_citations/all_citations.bib")
    df$datasetID <- cit$datasetID

    # rename for database ingest
    cit_full <- df %>%
        rename(target_id = .data$datasetID,
               source_id = .data$DOI,
               source_url = .data$URL,
               origin = .data$AUTHOR,
               title = .data$TITLE,
               publisher = .data$PUBLISHER,
               journal = .data$JOURNAL,
               volume = .data$VOLUME,
               page = .data$PAGES,
               year_of_publishing = .data$YEAR) %>%
        select(.data$target_id, .data$source_id, .data$source_url, .data$origin, .data$title, .data$publisher, .data$journal, .data$volume, .data$page, .data$year_of_publishing) %>%
        mutate(id = NA, report = NA, metadata = NA, link_publication_date = NA) %>%
        mutate(publisher = ifelse(.data$publisher == "Elsevier {BV", "Elsevier", "Copernicus"))

    write_json(cit_full, "~/dataone-citations/DBO_citations/citations_export.json")
}

