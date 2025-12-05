#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(dplyr))
library(tidyr)
library(scythe)
library(dataone)
library(jsonlite)
suppressPackageStartupMessages(library(lubridate))
library(optparse)

main <- function(){
    
    option_list <- list(
        make_option(c("-r", "--rows"), type="integer", default=100000,
                    help="Number of rows to return from query [default %default]"),
        make_option(c("-n", "--nodes"), type="character", help="Comma separated list of nodes to query"),
        make_option(c("-s", "--sources"), type="character", help="Comma separated list of sources to use")
    )
    
    # parse command-line arguments
    parser <- OptionParser(option_list=option_list)
    opts <- parse_args(parser)
    
    num_rows <- opts$rows
    nodes <- strsplit(opts$nodes, ",", fixed = TRUE)[[1]]
    sources <- strsplit(opts$sources, ",", fixed = TRUE)[[1]]
    
    dois <- c()
    for (node in nodes){
        message(paste("Gathering DOIs for: ", node)) 
        node_dois <- get_node_dois(node, num_rows)
        dois <- c(dois, node_dois)
    } 
    dois_unique <- unique(dois)
    
    # set up file to write to
    today <- format(Sys.Date(), "%Y%m%d")
    fp <- paste0("scythe-citations-", today, ".csv") 
    
    message("Beginning citations search.")
    found_citations <- citation_search(dois_unique, sources) 
    
    if (is.null(found_citations) || nrow(found_citations) == 0){
        writeLines("No citations found.", fp)
    } else {
        existing_citations <- get_metrics_citations()
        new_citations <- anti_join(found_citations, existing_citations, by = c("dataset_id" = "target_id", "article_id" = "source_id"))
        if (nrow(new_citations) > 0) {
            write.csv(new_citations, fp, row_names = FALSE)
        } else {
            writeLines("No new citations found.", fp)
        }
    }
}

get_node_dois <- function(node_id, num_rows) {
    mn <- getMNode(CNode("PROD"), node_id)
    result <- data.frame()
    for (i in seq(0, num_rows-1000, 1000)){
        res <- dataone::query(mn, list(q="id:doi* OR seriesId:doi*", 
                                                  fl="id, seriesId",
                                                  start = i,
                                                  rows = 1000),
                                         as="data.frame",
                                         parse=FALSE)
        result <- rbind(cd, res)
    }
    pids <- c(result$id, result$seriesId)
    dois <- grep("doi:", pids, value = TRUE)
    return(dois)
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

main()
