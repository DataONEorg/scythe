#!/usr/bin/env Rscript
# Live checks of the scheduled search against the real services, for the
# parts the offline tests can't cover (see tools/README.md). Run from the
# package's source directory, after devtools::install(), since the tracks'
# child processes load the installed package.
#
#   Rscript tools/live_check.R --tracks             # parallel tracks; no credits
#   Rscript tools/live_check.R --metrics            # the metrics comparison; no credits
#   Rscript tools/live_check.R --cycle --approve    # one scheduled run; ~12 OpenAlex credits
#
# Each check uses one small repository (--node, default urn:node:SDR) and a
# temporary directory, and prints what it found and whether it passed.

suppressPackageStartupMessages({
  library(optparse)
  library(scythe)
})

opts <- parse_args(OptionParser(option_list = list(
  make_option("--node", default = "urn:node:SDR", help = "Repository to check [default %default]"),
  make_option("--tracks", action = "store_true", default = FALSE,
              help = "Run the xDD and PLOS tracks in parallel child processes"),
  make_option("--metrics", action = "store_true", default = FALSE,
              help = "Check that the citations the metrics service has for --node are matched"),
  make_option("--cycle", action = "store_true", default = FALSE,
              help = "Run one scheduled run of --node (spends OpenAlex credits; needs --approve)"),
  make_option("--approve", action = "store_true", default = FALSE,
              help = "Approve the OpenAlex credits --cycle spends")
)))
if (!(opts$tracks || opts$metrics || opts$cycle)) stop("Choose at least one of --tracks, --metrics, --cycle")
`%||%` <- function(x, y) if (is.null(x)) y else x
ok <- TRUE
check <- function(label, pass) {
  cat(if (pass) "PASS" else "FAIL", label, "\n")
  if (!pass) ok <<- FALSE
}

if (opts$tracks) {
  cat("\n## Parallel tracks (xDD and PLOS) on", opts$node, "\n")
  dir <- file.path(tempdir(), "live-tracks")
  dois <- collect_dataone_dois(opts$node)
  scythe:::write_doi_list(dois, file.path(dir, "state", "dois", "2000-01-01-priority.csv"))
  scythe:::write_state(list(id = "2000-01-01", number = 1, started = "2000-01-01T00:00:00Z", phase = "done"),
                       file.path(dir, "state", "cycle.json"))
  h <- scythe:::start_tracks(c("xdd", "plos"), dir, opts$node, Sys.time() + 600, parallel = TRUE)
  check("two child processes started", length(h$procs) == 2)
  results <- scythe:::finish_tracks(h)
  for (s in names(results)) {
    t <- results[[s]]$track
    cat(sprintf("  %s: %s, %d of %d DOIs searched, %d citations\n", s, t$status,
                t$progress$searched, t$progress$total, nrow(results[[s]]$found %||% data.frame())))
    check(paste(s, "finished its pass"), identical(t$status, "done") && is.null(t$crashed))
  }
}

if (opts$metrics) {
  cat("\n## Metrics comparison for", opts$node, "\n")
  recorded <- get_metrics_citations(opts$node)
  dois <- collect_dataone_dois(opts$node)
  # The service's citations, as scythe would record them: each target as the
  # exact identifier in DataONE (found through its DOI).
  i <- match(scythe:::identifier_key(recorded$target_id), dois$doi)
  found <- data.frame(article_id = recorded$source_id, target_id = dois$identifier[i],
                      node = opts$node, stringsAsFactors = FALSE)[!is.na(i), , drop = FALSE]
  cat(sprintf("  %d recorded citations; %d have a DOI target in %s's DOI list\n",
              nrow(recorded), nrow(found), opts$node))
  status <- scythe:::metrics_status(found, recorded)
  print(table(status))
  check("every citation the service has is recognized as recorded", all(status == "yes"))
}

if (opts$cycle) {
  if (!opts$approve) stop("--cycle spends about 12 OpenAlex credits; add --approve to run it")
  cat("\n## One scheduled run of", opts$node, "\n")
  dir <- file.path(tempdir(), "live-cycle")
  status <- scheduled_search(opts$node, dir, other_nodes = NULL, text_sources = c("xdd", "plos"),
                             track_minutes = 5, max_credits = 100)
  check("the cycle's OpenAlex searches completed", identical(status$phase, "done"))
  check("results/citations.csv was written", file.exists(file.path(dir, "results", "citations.csv")))
  check("state/status.md was written", file.exists(file.path(dir, "state", "status.md")))
  cat("\n", readLines(file.path(dir, "state", "status.md")), sep = "\n")
}


cat("\n", if (ok) "All checks passed." else "Some checks FAILED.", "\n")
if (!ok) quit(status = 1)
