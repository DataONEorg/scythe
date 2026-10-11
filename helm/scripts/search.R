#!/usr/bin/env Rscript
# The scheduled citation search, run hourly by the helm chart's CronJob. Each
# run does whatever work is due and possible, then exits; see
# ?scythe::scheduled_search for the cycles, phases and files under --dir.
#
suppressPackageStartupMessages(library(scythe))
library(optparse)

main <- function() {
    option_list <- list(
        make_option("--priority-nodes", type = "character",
                    help = "Comma-separated member nodes searched every cycle"),
        make_option("--exclude-prefixes", type = "character", default = "",
                    help = paste("DOI prefixes to skip per node, as",
                                 "'urn:node:KNB=10.6073/AA/,10.6085/AA/;urn:node:X=...'")),
        make_option("--cycle-days", type = "character", default = "1,15",
                    help = "Days of the month a priority cycle may start [default %default]"),
        make_option("--other-nodes", type = "character", default = "all",
                    help = "Nodes for the monthly citation-graph pass: 'all', 'none', or a comma-separated list [default %default]"),
        make_option("--exclude-nodes", type = "character", default = "",
                    help = "Comma-separated nodes left out of the pass over other nodes"),
        make_option("--last-nodes", type = "character", default = "urn:node:PANGAEA",
                    help = "Comma-separated nodes searched last in the pass over other nodes [default %default]"),
        make_option("--others-every-days", type = "integer", default = 30,
                    help = "Days between passes over the other nodes [default %default]"),
        make_option("--lookback", type = "integer", default = 90,
                    help = "Days of lookback for the full-text search in later cycles [default %default]"),
        make_option("--full-text-every-cycles", type = "integer", default = 0,
                    help = "Every this many cycles, the OpenAlex full-text search searches all works; 0 for never [default %default]"),
        make_option("--text-sources", type = "character", default = "xdd,springer,scopus,plos",
                    help = "Comma-separated sources with full-text tracks [default %default]"),
        make_option("--track-minutes", type = "double", default = 50,
                    help = "The longest each full-text track runs per run [default %default]"),
        make_option("--alert-pass-cycles", type = "integer", default = 2,
                    help = "Alert when a track's pass has spanned this many cycles [default %default]"),
        make_option("--alert-paused-days", type = "double", default = 3,
                    help = "Alert when a track has been paused this many days [default %default]"),
        make_option("--metrics-check-hours", type = "double", default = 24,
                    help = "Hours between comparisons with DataONE's metrics service; 0 for never [default %default]"),
        make_option("--metrics-url", type = "character", default = NULL,
                    help = "The metrics service's address [default: the scythe.metrics_url option, or the package default]"),
        make_option("--approve", type = "character", default = Sys.getenv("SCYTHE_OPENALEX_APPROVE"),
                    help = "OpenAlex credits a run may spend, or 'unlimited'; also read from SCYTHE_OPENALEX_APPROVE [default: 500]"),
        make_option("--dir", type = "character", default = "/var/data/scythe",
                    help = "Directory for the search's state and results [default %default]"),
        make_option("--log-dir", type = "character", default = NULL,
                    help = "Directory for the OpenAlex review logs [default: <dir>/logs]")
    )
    opts <- parse_args(OptionParser(option_list = option_list))
    if (is.null(opts$`priority-nodes`)) stop("--priority-nodes is required")
    if (!is.null(opts$`metrics-url`)) options(scythe.metrics_url = opts$`metrics-url`)

    split <- function(x) {
        x <- trimws(strsplit(x %||% "", ",", fixed = TRUE)[[1]])
        x[nzchar(x)]
    }
    other_nodes <- switch(opts$`other-nodes`, all = "all", none = NULL, split(opts$`other-nodes`))
    max_credits <- if (identical(opts$approve, "unlimited")) {
        Inf
    } else if (nzchar(opts$approve)) {
        as.numeric(opts$approve)
    } else {
        500
    }

    scheduled_search(
        priority_nodes = split(opts$`priority-nodes`),
        dir = opts$dir,
        exclude_prefixes = parse_exclude_prefixes(opts$`exclude-prefixes`),
        cycle_days = as.integer(split(opts$`cycle-days`)),
        other_nodes = other_nodes,
        exclude_nodes = split(opts$`exclude-nodes`),
        last_nodes = split(opts$`last-nodes`),
        others_every_days = opts$`others-every-days`,
        lookback = opts$lookback,
        full_text_every_cycles = opts$`full-text-every-cycles`,
        text_sources = split(opts$`text-sources`),
        track_minutes = opts$`track-minutes`,
        max_credits = max_credits,
        alert_pass_cycles = opts$`alert-pass-cycles`,
        alert_paused_days = opts$`alert-paused-days`,
        metrics_check_hours = opts$`metrics-check-hours`,
        log_dir = opts$`log-dir` %||% file.path(opts$dir, "logs")
    )
    invisible()
}

# "urn:node:KNB=10.6073/AA/,10.6085/AA/;urn:node:X=10.1/" as a named list.
parse_exclude_prefixes <- function(text) {
    entries <- trimws(strsplit(text, ";", fixed = TRUE)[[1]])
    entries <- entries[nzchar(entries)]
    nodes <- sub("=.*$", "", entries)
    prefixes <- lapply(sub("^[^=]*=", "", entries), function(p) {
        p <- trimws(strsplit(p, ",", fixed = TRUE)[[1]])
        p[nzchar(p)]
    })
    stats::setNames(prefixes, nodes)
}

`%||%` <- function(x, y) if (is.null(x)) y else x

main()
