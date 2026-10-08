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
        make_option(c("-s", "--sources"), type="character", help="Comma separated list of sources to use"),
        make_option("--since", type="character", default=NULL,
                    help="OpenAlex: only find works published on or after this date (YYYY-MM-DD) [default: the last complete run minus --lookback days, or all works if there is none]"),
        make_option("--lookback", type="integer", default=365,
                    help="OpenAlex: days before the last complete run to search from [default %default]"),
        make_option("--shard", type="character", default=NULL,
                    help="OpenAlex: search only shard i of n of the DOIs, given as i/n"),
        make_option("--log-dir", type="character", default="openalex-logs",
                    help="OpenAlex: directory for the review logs [default %default]"),
        make_option("--checkpoint-dir", type="character", default="openalex-checkpoints",
                    help="OpenAlex: directory for checkpoints and the shared rate-limit state [default %default]"),
        make_option("--approve", type="character", default=Sys.getenv("SCYTHE_OPENALEX_APPROVE"),
                    help="OpenAlex: approve spending up to this many credits, or 'unlimited'; also read from SCYTHE_OPENALEX_APPROVE")
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
    found_citations <- data.frame()
    if ("springer" %in% sources){
        message("Searching Springer.")
        found_citations_springer <- citation_search(dois_unique, "springer") 
        found_citations <- dplyr::bind_rows(found_citations, found_citations_springer)
    }

    if ("scopus" %in% sources){
        message("Searching Scopus.")
        found_citations_scopus <- citation_search(dois_unique, "scopus") 
        found_citations <- dplyr::bind_rows(found_citations, found_citations_scopus)
    }

    if ("plos" %in% sources){
        message("Searching Plos.")
        found_citations_plos <- citation_search(dois_unique, "plos") 
        found_citations <- dplyr::bind_rows(found_citations, found_citations_plos)
    }

    if ("xdd" %in% sources){
        message("Searching xdd.")
        found_citations_xdd <- citation_search(dois_unique, "xdd") 
        found_citations <- dplyr::bind_rows(found_citations, found_citations_xdd)
    }

    if ("openalex" %in% sources){
        message("Searching OpenAlex.")
        found_citations_openalex <- search_openalex(dois_unique, opts)
        found_citations <- dplyr::bind_rows(found_citations, found_citations_openalex)
    }
    
    if (is.null(found_citations) || nrow(found_citations) == 0){
        writeLines("No citations found.", fp)
    } else {
        #tryCatch({
        #  existing_citations <- get_metrics_citations()
        #}, error = function(e) {
        #  print(e)
        #})
        
       # if (nrow(new_citations) > 0) {
            write.csv(found_citations, fp, row.names = FALSE)
       # } else {
       #     writeLines("No new citations found.", fp)
       # }
    }
}

# Search OpenAlex, resuming an interrupted run from its checkpoints and
# recording when the last complete run happened so that the next one only
# searches recent works.
search_openalex <- function(dois, opts) {
    if (!is.null(opts$shard)) {
        shard <- as.integer(strsplit(opts$shard, "/", fixed = TRUE)[[1]])
        # Assign DOIs to shards by a hash of the DOI, so a DOI stays in the
        # same shard as the set of DOIs changes between runs.
        hash <- vapply(dois, function(d) sum(utf8ToInt(d) * seq_len(nchar(d))), 1)
        dois <- dois[hash %% shard[2] == shard[1] - 1]
        message("OpenAlex shard ", opts$shard, ": ", length(dois), " DOIs")
    }
    shard_label <- if (is.null(opts$shard)) "all" else sub("/", "-of-", opts$shard, fixed = TRUE)
    checkpoint_dir <- file.path(opts$`checkpoint-dir`, shard_label)
    # Rate-limit pauses are shared by every shard using the same API key.
    state_file <- file.path(opts$`checkpoint-dir`, "openalex-state.json")
    run_file <- file.path(checkpoint_dir, "openalex-run.json")
    run <- if (file.exists(run_file)) jsonlite::read_json(run_file, simplifyVector = TRUE) else list()

    # Resume an interrupted run with the date filter it started with.
    since <- opts$since
    if (is.null(since) && !is.null(run$pending_since)) {
        since <- if (run$pending_since == "all") NULL else run$pending_since
    } else if (is.null(since) && !is.null(run$last_run)) {
        since <- format(as.Date(run$last_run) - opts$lookback)
    }
    if (is.null(since)) message("OpenAlex: no previous complete run, so searching all works (a backfill).")

    approve <- opts$approve
    max_credits <- if (identical(approve, "unlimited")) {
        Inf
    } else if (nzchar(approve)) {
        as.numeric(approve)
    } else {
        getOption("scythe.openalex.max_credits", 500)
    }
    preflight <- !nzchar(approve)
    if (preflight && interactive()) {
        estimate <- scythe:::estimate_openalex_cost(length(dois))
        if (estimate > max_credits &&
            tolower(readline(paste0("Spend about ", estimate, " OpenAlex credits? [y/N] "))) == "y") {
            max_credits <- Inf
        }
    }

    dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
    started <- format(Sys.Date())
    found <- tryCatch(
        citation_search_openalex(dois, since = since, log_dir = opts$`log-dir`,
                                 checkpoint_dir = checkpoint_dir, state_file = state_file,
                                 max_credits = max_credits, preflight = preflight),
        error = function(e) {
            message("OpenAlex search not run: ", conditionMessage(e),
                    " Pass --approve (or set SCYTHE_OPENALEX_APPROVE) to approve it.")
            NULL
        }
    )
    if (is.null(found)) return(data.frame())

    status <- attr(found, "openalex_status")
    if (isTRUE(status$complete)) {
        run <- list(last_run = run$pending_started %||% started)
        unlink(file.path(checkpoint_dir, "openalex-checkpoint.jsonl"))
    } else {
        run$pending_since <- since %||% "all"
        run$pending_started <- run$pending_started %||% started
        message("OpenAlex search is incomplete (", status$stopped %||% "some batches failed",
                "); the next run will continue from the checkpoints in ", checkpoint_dir)
    }
    jsonlite::write_json(run, run_file, auto_unbox = TRUE, pretty = TRUE)
    found
}

`%||%` <- function(x, y) if (is.null(x)) y else x

get_node_dois <- function(node_id, num_rows) {
    mn <- getMNode(CNode("PROD"), node_id)
    
    if (num_rows > 1000){
        result <- data.frame()
        for (i in seq(0, num_rows-1000, 1000)){
            res <- dataone::query(mn, list(q="id:doi* OR seriesId:doi*", 
                                                      fl="id, seriesId",
                                                      start = i,
                                                      rows = 1000),
                                             as="data.frame",
                                             parse=FALSE)
            result <- dplyr::bind_rows(result, res)
        }
    } else {
        result <- dataone::query(mn, list(q="id:doi* OR seriesId:doi*", 
                                          fl="id, seriesId",
                                          start = 0,
                                          rows = num_rows),
                                 as="data.frame",
                                 parse=FALSE)
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
