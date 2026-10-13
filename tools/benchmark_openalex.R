#!/usr/bin/env Rscript
# Measure the recall and precision of citation_search_openalex() against a gold
# set of known dataset citations.
#
# This is a developer procedure, not a test: a gold set covers thousands of
# DOIs, and searching them spends real OpenAlex credits. Run it only after the
# package tests pass, with an OpenAlex API key, from a host with a good
# connection, and only with explicit approval of the cost (see tools/README.md).
#
#   Rscript tools/benchmark_openalex.R --gold gold.csv --out benchmark \
#     --approve 20000
#
# The gold CSV needs `article_id` and `dataset_id` columns (DOIs, in any
# form), and may have a `source` column naming the source that found each
# citation.

suppressPackageStartupMessages({
  library(optparse)
  library(scythe)
})

opts <- parse_args(OptionParser(option_list = list(
  make_option("--gold", type = "character", help = "CSV of known citations (article_id, dataset_id[, source])"),
  make_option("--out", type = "character", default = "openalex-benchmark",
              help = "Output directory [default %default]"),
  make_option("--approve", type = "character", default = "",
              help = "Credits approved for this run, or 'unlimited' (required unless interactive)"),
  make_option("--methods", type = "character", default = "text,graph",
              help = "OpenAlex methods to benchmark [default %default]")
)))
if (is.null(opts$gold)) stop("--gold is required")

normalize_doi <- scythe:::normalize_doi
`%||%` <- function(x, y) if (is.null(x)) y else x
gold <- read.csv(opts$gold, stringsAsFactors = FALSE)
gold$article_doi <- normalize_doi(gold$article_id)
gold$dataset_doi <- normalize_doi(gold$dataset_id)
dataset_dois <- unique(stats::na.omit(gold$dataset_doi))
methods <- strsplit(opts$methods, ",", fixed = TRUE)[[1]]

# The cost must be approved explicitly: on the command line, or at a prompt.
estimate <- scythe:::estimate_openalex_cost(length(dataset_dois), methods)
message("Benchmark: ", nrow(gold), " gold citations of ", length(dataset_dois),
        " datasets; estimated cost about ", estimate, " OpenAlex credits.")
if (is.na(scythe_get_key("openalex"))) stop("Set an OpenAlex API key before running the benchmark.")
max_credits <- if (identical(opts$approve, "unlimited")) {
  Inf
} else if (nzchar(opts$approve)) {
  as.numeric(opts$approve)
} else if (interactive() &&
           tolower(readline(paste0("Spend up to ", estimate, " credits? [y/N] "))) == "y") {
  estimate
} else {
  stop("Not approved. Pass --approve with a number of credits, or 'unlimited'.")
}

dir.create(opts$out, showWarnings = FALSE, recursive = TRUE)
checkpoint_dir <- file.path(opts$out, "checkpoints")
state_file <- file.path(checkpoint_dir, "openalex-state.json")
found <- citation_search_openalex(
  dataset_dois, methods = methods,
  log_dir = file.path(opts$out, "logs"),
  checkpoint_dir = checkpoint_dir, state_file = state_file,
  max_credits = max_credits, preflight = FALSE
)
status <- attr(found, "openalex_status")
# The diagnostics below need more OpenAlex requests, so don't send them while
# the budget is rate limited.
if (identical(status$stopped, "rate_limited")) {
  stop("OpenAlex is rate limited until ", status$paused_until %||% "its reset time",
       ". Rerun the same command after then to continue from the checkpoints.", call. = FALSE)
}
if (!isTRUE(status$complete)) {
  warning("The OpenAlex search did not complete (", status$stopped %||% "failed batches",
          "); rerun the same command to continue from the checkpoints. ",
          "Recall below only covers the batches finished so far.")
}
# A search that found no articles writes no matches log.
matches_file <- file.path(opts$out, "logs", "openalex-matches.jsonl")
matches <- if (file.exists(matches_file)) {
  jsonlite::stream_in(file(matches_file), verbose = FALSE)
} else {
  data.frame(dataset_doi = character(), work_doi = character(), match_method = character())
}
matches <- matches[!duplicated(paste(matches$dataset_doi, matches$work_doi)), ]
found_key <- paste(matches$work_doi, matches$dataset_doi)
gold_key <- paste(gold$article_doi, gold$dataset_doi)

# Recall: which gold citations were found, and why the others were missed.
client <- scythe:::openalex_client(max_credits = max_credits - status$credits_used,
                                   state_file = state_file)
lookup <- function(dois, select) {
  dois <- unique(stats::na.omit(dois))
  batches <- scythe:::chunk(dois, 100)
  run <- scythe:::openalex_run(client, lapply(batches, function(b) {
    list(filter = paste0("doi:", paste(b, collapse = "|")), select = select, `per-page` = "200")
  }), paginate = FALSE)
  works <- unlist(run$results, recursive = FALSE)
  works <- stats::setNames(works, vapply(works, function(w) scythe:::openalex_doi(w$doi), ""))
  # The DOIs actually looked up: a batch that wasn't sent (the credits ran
  # out) tells us nothing about whether OpenAlex has them.
  attr(works, "checked") <- unlist(batches[run$status == "done"])
  works
}
gold$found <- gold_key %in% found_key
missed <- gold[!gold$found, ]
articles <- lookup(missed$article_doi, "id,doi,has_fulltext,referenced_works")
datasets <- lookup(missed$dataset_doi, "id,doi")
checked <- c(attr(articles, "checked"), attr(datasets, "checked"))
missed$reason <- vapply(seq_len(nrow(missed)), function(i) {
  if (is.na(missed$article_doi[i]) || is.na(missed$dataset_doi[i])) {
    return("identifier is not a DOI that scythe can normalize")
  }
  if (!all(c(missed$article_doi[i], missed$dataset_doi[i]) %in% checked)) {
    return("not checked (the lookups ran out of OpenAlex credits)")
  }
  a <- articles[[missed$article_doi[i]]]
  d <- datasets[[missed$dataset_doi[i]]]
  if (is.null(a)) {
    "article not in OpenAlex"
  } else if (!isTRUE(a$has_fulltext)) {
    "article has no full text in OpenAlex, and its references do not resolve the dataset"
  } else if (!is.null(d) && d$id %in% unlist(a$referenced_works)) {
    "article cites the dataset in OpenAlex but the search did not return it"
  } else {
    "article has full text but the exact DOI search did not match it"
  }
}, "")
unchecked <- sum(missed$reason == "not checked (the lookups ran out of OpenAlex credits)")
if (unchecked > 0) {
  warning(unchecked, " missed citations weren't checked, because the search used the approved ",
          "credits; rerun with a larger --approve to find out why they were missed.", call. = FALSE)
}
gold$reason <- NA_character_
gold$reason[!gold$found] <- missed$reason
gold$prefix <- sub("/.*$", "", gold$dataset_doi)
recall <- aggregate(found ~ prefix, gold, function(x) c(n = length(x), recall = mean(x)))
utils::write.csv(gold, file.path(opts$out, "gold-recall.csv"), row.names = FALSE)

# Precision: matches that are not in the gold set are checked against the
# citing work's Crossref reference list; the rest need a person to check them
# against the full text.
new <- matches[!found_key %in% gold_key, ]
corroborated <- vapply(seq_len(nrow(new)), function(i) {
  # DataCite DOIs are not in Crossref, so a 404 here just means "unknown".
  refs <- tryCatch(suppressWarnings(rcrossref::cr_works(dois = new$work_doi[i])$data$reference[[1]]),
                   error = function(e) NULL)
  if (is.null(refs)) return(NA)
  text <- tolower(paste(unlist(refs), collapse = " "))
  grepl(new$dataset_doi[i], text, fixed = TRUE)
}, NA)
new$crossref_corroborated <- corroborated
utils::write.csv(new[!(corroborated %in% TRUE), ],
                 file.path(opts$out, "needs-review.csv"), row.names = FALSE)
matches$status <- ifelse(found_key %in% gold_key, "in gold set",
                         ifelse(found_key %in% paste(new$work_doi, new$dataset_doi)[new$crossref_corroborated %in% TRUE],
                                "corroborated by Crossref", "unverified"))
precision <- as.data.frame.matrix(table(matches$match_method, matches$status))
utils::write.csv(precision, file.path(opts$out, "precision-by-method.csv"))

# Overlap with the other sources in the gold set.
if ("source" %in% names(gold)) {
  overlap <- as.data.frame.matrix(table(gold$source, ifelse(gold$found, "found by OpenAlex", "missed")))
  utils::write.csv(overlap, file.path(opts$out, "overlap-by-source.csv"))
}

summary_lines <- c(
  paste0("OpenAlex benchmark, ", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  paste0("Search complete: ", isTRUE(status$complete), "; credits used: ",
         status$credits_used + client$credits_used),
  paste0("Recall: ", sum(gold$found), " of ", nrow(gold), " gold citations (",
         round(100 * mean(gold$found), 1), "%)"),
  "Misses by reason:",
  paste0("  ", names(table(missed$reason)), ": ", table(missed$reason)),
  paste0("Matches not in the gold set: ", nrow(new), " (", sum(corroborated %in% TRUE),
         " corroborated by Crossref references; the rest are in needs-review.csv)"),
  "Matches by method and status:",
  utils::capture.output(print(precision)),
  "Recall by repository prefix:",
  utils::capture.output(print(recall))
)
writeLines(summary_lines, file.path(opts$out, "summary.txt"))
writeLines(summary_lines)
