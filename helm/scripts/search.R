# search.R
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(scythe)
library(dataone)
library(jsonlite)
suppressPackageStartupMessages(library(lubridate))

sources <- c("plos", "xdd", "scopus", "springer")
nodes <- commandArgs(trailingOnly = TRUE)

get_node_dois <- function(node_id) {
    mn <- getMNode(CNode("PROD"), node_id)
    queryParamList <- list(q="id:doi*", 
                           fl="id",
                           start ="0",
                           rows = "10")
    result <- query(mn, solrQuery=queryParamList, as="data.frame", parse=FALSE)
    return(result$id)
}

get_metrics_citations <- function(from = as.POSIXct("2000-01-01"), to = as.POSIXct(Sys.Date())){
    
    from <- as.Date(from); to <- as.Date(to)
    from_q <- paste(stringr::str_pad(month(from), 2, side = "left", pad = "0"),
                    stringr::str_pad(day(from), 2, side = "left", pad = "0"),
                    stringr::str_pad(year(from), 2, side = "left", pad = "0"),
                    sep = "/")
    
    to_q <- paste(stringr::str_pad(month(to), 2, side = "left", pad = "0"),
                  stringr::str_pad(day(to), 2, side = "left", pad = "0"),
                  stringr::str_pad(year(to), 2, side = "left", pad = "0"),
                  sep = "/")
    
    d <- fromJSON(paste0('https://logproc-stage-ucsb-1.test.dataone.org/metrics?q={%22metricsPage%22:{%22total%22:0,%22start%22:0,%22count%22:0},%22metrics%22:[%22citations%22],%22filterBy%22:[{%22filterType%22:%22repository%22,%22values%22:[%22urn:node:ARCTIC%22],%22interpretAs%22:%22list%22},{%22filterType%22:%22month%22,%22values%22:[%22', from_q,'%22,%22', to_q, '%22],%22interpretAs%22:%22range%22}],%22groupBy%22:[%22month%22]}'))
    
    output_json <- d$resultDetails$citations # pulls citation info
    output_df <- as.data.frame(do.call(rbind, output_json), row.names = FALSE) # binds nested cit info into dataframe
    
    output_df <- output_df %>% 
        unnest_longer(target_id) %>% 
        unnest_longer(source_id)
    
    return(output_df)
}

dois <- c()
for (node in nodes){
    node_dois <- get_node_dois(node)
    dois <- c(dois, node_dois)
}

# set up file to write to
today <- format(Sys.Date(), "%Y%m%d")
fp <- paste0("scythe-citations-", today, ".json")

found_citations <- citation_search(dois, sources)

if (is.null(found_citations) || nrow(found_citations) == 0){
    writeLines("No citations found.", fp)
} else {
    existing_citations <- get_metrics_citations()
    new_citations <- anti_join(found_citations, existing_citations, by = c("dataset_id" = "target_id", "article_id" = "source_id"))
    if (nrow(new_citations) > 0) {
        write_citation_pairs(new_citations, fp)
    } else {
        writeLines("No citations found.", fp)
    }
}