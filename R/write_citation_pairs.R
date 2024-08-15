#' Write citation pairs
#'
#' @param citation_list (data.frame) data.frame of citation pairs containing variables article_id and dataset_id
#' @param path (char) path to write JSON citation pairs to
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' pairs <- data.frame(article_id = "10.1371/journal.pone.0213037",
#'                     dataset_id = "10.18739/A22274")
#' write_citation_pairs(citation_list = pairs, path = "citation_pairs.json")
#' }

write_citation_pairs <- function(citation_list, path) {
    if (any(!(c("article_id", "dataset_id") %in% names(citation_list)))) {
        stop(
            .call = FALSE,
            "citations_list data.frame does not contain variables article_id and/or dataset_id"
        )
    }
    
    # write list of citations to bib format
    bib_full <- c()
    for (i in 1:nrow(citation_list)){
        bib <- tryCatch({
            trimws(rcrossref::cr_cn(dois = citation_list$article_id[i], format = "bibtex"))
        }, error = function(e) {
            paste0("@article{ERROR, title={", conditionMessage(e), "}, volume={ERROR}, ISSN={ERROR}, url={ERROR}, DOI={", citation_list$article_id[i], "}, number={ERROR}, journal={ERROR}, publisher={ERROR}, author={ERROR}, year={1900}, month=dec, pages={ERROR} }\n")
        }, warning = function(w) {
            paste0("@article{WARNING, title={", conditionMessage(w), "}, volume={WARNING}, ISSN={WARNING}, url={WARNING}, DOI={", citation_list$article_id[i], "}, number={WARNING}, journal={WARNING}, publisher={WARNING}, author={WARNING}, year={1900}, month=dec, pages={WARNING} }\n")
        })
        bib_full <- paste(bib_full, bib, sep = "\n")
    }
    
    
    t <- tempfile()
    writeLines(bib_full, t)
    
    # import as a dataframe
    df <- bib2df::bib2df(t)
    
    # assign article_id to data.frame
    df$dataset_id <- citation_list$dataset_id
    
    # rename for database ingest
    cit_full <- df %>%
        dplyr::rename(
            target_id = .data$dataset_id,
            source_id = .data$DOI,
            source_url = .data$URL,
            origin = .data$AUTHOR,
            title = .data$TITLE,
            publisher = .data$PUBLISHER,
            journal = .data$JOURNAL,
            volume = .data$VOLUME,
            page = .data$PAGES,
            year_of_publishing = .data$YEAR
        ) %>%
        dplyr::select(
            .data$target_id,
            .data$source_id,
            .data$source_url,
            .data$origin,
            .data$title,
            .data$publisher,
            .data$journal,
            .data$volume,
            .data$page,
            .data$year_of_publishing
        ) %>%
        dplyr::mutate(
            id = NA,
            report = NA,
            metadata = NA,
            link_publication_date = Sys.Date()
        ) #%>%
    #dplyr::mutate(publisher = ifelse(.data$publisher == "Elsevier {BV", "Elsevier", "Copernicus"))
    
    jsonlite::write_json(cit_full, path)
}
