# The scheduled citation search: every two weeks, a cycle searches the
# priority nodes' DOIs as completely as possible, and once that no longer
# needs OpenAlex credits, a monthly pass runs the OpenAlex citation-graph
# search over every other node. scheduled_search() is meant to be called
# hourly: each call reads the saved state, does whatever work is due and
# possible (stopping when OpenAlex rate-limits it), and returns.
#
# Everything is kept under `dir`:
#
#   state/cycle.json         the current priority cycle and its phase
#   state/others.json        the current pass over the other nodes
#   state/dois/              the DOI list saved for each cycle and pass
#   state/openalex/          OpenAlex checkpoints, one directory per phase
#   state/openalex-state.json  OpenAlex rate-limit pauses
#   results/citations.csv    every citation found so far, one row per pair
#   results/<cycle>/         each phase's citations, once the phase completes
#
# A cycle's phases are "priority_graph", "priority_text" and "done". A new
# cycle starts on a cycle day (or later, once the previous one is done), and
# the other nodes' pass only runs while the current cycle is done, so a new
# cycle pauses it until the priority OpenAlex phases are finished.

#' Run the scheduled citation search
#'
#' Does whatever work of the scheduled search is due and possible now, then
#' returns; it is meant to be run hourly (see `helm/`). Each priority cycle
#' collects the priority nodes' DOIs (see [collect_dataone_dois()]), runs the
#' OpenAlex citation-graph search over them, then the OpenAlex full-text
#' search. Once a cycle is done, a pass of the citation-graph search over the
#' other nodes runs, if one is due. A search stopped by OpenAlex's rate limit
#' or budget continues on a later call from its checkpoints.
#'
#' Citations are added to `results/citations.csv` in `dir` as they are found,
#' one row per pair of dataset DOI and citing article, recording which
#' searches found it and when it was first found. When a phase completes, its
#' citations are also written to `results/<cycle>/`: `priority-graph.csv`,
#' `priority-text.csv`, `priority-text-new.csv` (full-text citations the
#' graph search didn't find), and `others-graph.csv` for a pass over the
#' other nodes.
#'
#' @param priority_nodes member nodes searched every cycle, from their own
#'   indexes
#' @param dir directory holding the search's state and results
#' @param exclude_prefixes DOI prefixes to skip, per node (see
#'   [collect_dataone_dois()])
#' @param cycle_days days of the month on which a priority cycle may start
#' @param other_nodes the other nodes for the citation-graph pass: "all" (every
#'   node that isn't a priority node), a vector of nodes, or NULL for none
#' @param exclude_nodes nodes left out of the pass over other nodes
#' @param last_nodes nodes searched last in the pass over other nodes
#' @param others_every_days days between passes over the other nodes
#' @param lookback days before the start of the last cycle that completed its
#'   full-text search, from which a later cycle's full-text search looks for
#'   newly published works. A cycle with no earlier complete one searches all
#'   works. A longer lookback catches more works that OpenAlex indexed late,
#'   but costs more credits.
#' @param max_credits the most OpenAlex credits one call may spend
#' @param log_dir directory for the OpenAlex review logs
#' @param cn_url the DataONE coordinating node's base URL
#' @return a list describing the state after this call, invisibly
#' @export
scheduled_search <- function(priority_nodes, dir,
                             exclude_prefixes = list(),
                             cycle_days = c(1, 15),
                             other_nodes = "all",
                             exclude_nodes = character(),
                             last_nodes = "urn:node:PANGAEA",
                             others_every_days = 30,
                             lookback = 90,
                             max_credits = Inf,
                             log_dir = file.path(dir, "logs"),
                             cn_url = dataone_cn) {
  paths <- schedule_paths(dir)
  ran <- character()
  now <- http_now()
  today <- as.Date(now, tz = "UTC")
  phase_run <- function(dois, method, since, checkpoint_dir, cycle, phase) {
    ran <<- c(ran, phase)
    found <- citation_search_openalex(dois$doi, methods = method, since = since,
                                      log_dir = log_dir, checkpoint_dir = checkpoint_dir,
                                      state_file = paths$openalex_state,
                                      max_credits = max_credits, preflight = FALSE)
    found <- annotate_citations(found, dois)
    add_to_total(found, paths$total, source = paste0("openalex-", method), phase = phase,
                 cycle = cycle, found_at = format_utc(now))
    found
  }

  # Start a new priority cycle when one is due.
  cycle <- read_state(paths$cycle)
  if (cycle_due(cycle, today, cycle_days)) {
    id <- format(today)
    message("Starting priority cycle ", id, ".")
    dois <- tryCatch(collect_dataone_dois(priority_nodes, exclude_prefixes = exclude_prefixes,
                                          cn_url = cn_url),
                     error = function(e) e)
    if (inherits(dois, "error")) {
      message("Couldn't collect the priority nodes' DOIs (", conditionMessage(dois),
              "); trying again on the next run.")
      return(invisible(list(cycle = cycle, ran = ran)))
    }
    write_doi_list(dois, file.path(paths$dois, paste0(id, "-priority.csv")))
    cycle <- list(id = id, started = format_utc(now), phase = "priority_graph",
                  text_since_base = cycle$text_since_base)
    write_state(cycle, paths$cycle)
  }

  # The priority phases.
  if (!is.null(cycle) && cycle$phase != "done") {
    dois <- read_doi_list(file.path(paths$dois, paste0(cycle$id, "-priority.csv")))
    results_dir <- file.path(paths$results, cycle$id)
    if (cycle$phase == "priority_graph") {
      checkpoint <- file.path(paths$openalex, cycle$id, "priority-graph")
      found <- phase_run(dois, "graph", NULL, checkpoint, cycle$id, "priority_graph")
      if (!openalex_complete(found)) return(invisible(schedule_status(cycle, paths, ran, found)))
      write_citations(found, file.path(results_dir, "priority-graph.csv"))
      unlink(checkpoint, recursive = TRUE)
      cycle$phase <- "priority_text"
      write_state(cycle, paths$cycle)
    }
    if (cycle$phase == "priority_text") {
      since <- if (!is.null(cycle$text_since_base)) {
        format(as.Date(cycle$text_since_base) - lookback)
      }
      checkpoint <- file.path(paths$openalex, cycle$id, "priority-text")
      found <- phase_run(dois, "text", since, checkpoint, cycle$id, "priority_text")
      if (!openalex_complete(found)) return(invisible(schedule_status(cycle, paths, ran, found)))
      write_citations(found, file.path(results_dir, "priority-text.csv"))
      graph <- read_citations(file.path(results_dir, "priority-graph.csv"))
      write_citations(found[!pair_key(found) %in% pair_key(graph), , drop = FALSE],
                      file.path(results_dir, "priority-text-new.csv"))
      unlink(checkpoint, recursive = TRUE)
      cycle$phase <- "done"
      cycle$text_since_base <- format(as.Date(parse_utc(cycle$started)))
      cycle$completed <- format_utc(http_now())
      write_state(cycle, paths$cycle)
      message("Priority cycle ", cycle$id, " is complete.")
    }
  }

  # The pass over the other nodes, while no priority OpenAlex work is pending.
  others <- read_state(paths$others)
  if (length(other_nodes) && !is.null(cycle) && cycle$phase == "done" &&
      others_due(others, today, others_every_days)) {
    if (is.null(others) || others$status == "done") {
      id <- format(today)
      message("Starting a citation-graph pass over the other nodes (", id, ").")
      dois <- tryCatch(collect_other_dois(other_nodes, priority_nodes, exclude_nodes, last_nodes,
                                          cn_url),
                       error = function(e) e)
      if (inherits(dois, "error")) {
        message("Couldn't collect the other nodes' DOIs (", conditionMessage(dois),
                "); trying again on the next run.")
        return(invisible(schedule_status(cycle, paths, ran)))
      }
      write_doi_list(dois, file.path(paths$dois, paste0("others-", id, ".csv")))
      others <- list(id = id, started = format_utc(now), status = "running")
      write_state(others, paths$others)
    }
    dois <- read_doi_list(file.path(paths$dois, paste0("others-", others$id, ".csv")))
    checkpoint <- file.path(paths$openalex, paste0("others-", others$id))
    found <- phase_run(dois, "graph", NULL, checkpoint, paste0("others-", others$id), "others_graph")
    if (openalex_complete(found)) {
      write_citations(found, file.path(paths$results, paste0("others-", others$id), "others-graph.csv"))
      unlink(checkpoint, recursive = TRUE)
      others$status <- "done"
      others$completed <- format_utc(http_now())
      write_state(others, paths$others)
      message("The pass over the other nodes (", others$id, ") is complete.")
    }
    return(invisible(schedule_status(cycle, paths, ran, found)))
  }
  invisible(schedule_status(cycle, paths, ran))
}

schedule_paths <- function(dir) {
  list(
    cycle = file.path(dir, "state", "cycle.json"),
    others = file.path(dir, "state", "others.json"),
    dois = file.path(dir, "state", "dois"),
    openalex = file.path(dir, "state", "openalex"),
    openalex_state = file.path(dir, "state", "openalex-state.json"),
    results = file.path(dir, "results"),
    total = file.path(dir, "results", "citations.csv")
  )
}

read_state <- function(path) {
  if (!file.exists(path)) return(NULL)
  jsonlite::read_json(path, simplifyVector = TRUE)
}

write_state <- function(state, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp", Sys.getpid())
  jsonlite::write_json(Filter(Negate(is.null), state), tmp, auto_unbox = TRUE, pretty = TRUE)
  file.rename(tmp, path)
}

# The most recent cycle day on or before `today`. Days a month doesn't have
# (such as the 30th in February) are skipped, so up to three months are
# searched.
latest_cycle_day <- function(today, cycle_days) {
  months <- seq(as.Date(format(today, "%Y-%m-01")), by = "-1 month", length.out = 3)
  days <- as.Date(unlist(lapply(months, function(m) {
    last <- as.numeric(format(seq(m, by = "1 month", length.out = 2)[2] - 1, "%d"))
    format(m + (cycle_days[cycle_days <= last] - 1))
  })))
  max(days[days <= today])
}

# A new cycle is due when there is none yet, or the current one is done and
# a cycle day has passed since it started.
cycle_due <- function(cycle, today, cycle_days) {
  if (is.null(cycle)) return(TRUE)
  cycle$phase == "done" &&
    latest_cycle_day(today, cycle_days) > as.Date(parse_utc(cycle$started))
}

# A pass over the other nodes is due when one is in progress, or when the
# last one started at least `every_days` ago.
others_due <- function(others, today, every_days) {
  if (is.null(others) || others$status != "done") return(TRUE)
  as.numeric(today - as.Date(parse_utc(others$started))) >= every_days
}

collect_other_dois <- function(other_nodes, priority_nodes, exclude_nodes, last_nodes, cn_url) {
  dois <- if (identical(other_nodes, "all")) {
    collect_dataone_dois(other_nodes = "all", exclude_nodes = c(priority_nodes, exclude_nodes),
                         cn_url = cn_url)
  } else {
    collect_dataone_dois(other_nodes = setdiff(other_nodes, c(priority_nodes, exclude_nodes)),
                         cn_url = cn_url)
  }
  # Search the `last_nodes` last, so that they wait for every other node.
  dois[order(match(dois$node, last_nodes, nomatch = 0)), , drop = FALSE]
}

openalex_complete <- function(found) isTRUE(attr(found, "openalex_status")$complete)

# Add the node and dataset of each citation's dataset DOI.
annotate_citations <- function(found, dois) {
  status <- attr(found, "openalex_status")
  i <- match(normalize_doi(found$dataset_id), dois$doi)
  found$node <- dois$node[i]
  found$dataset <- dois$dataset[i]
  attr(found, "openalex_status") <- status
  found
}

pair_key <- function(citations) {
  paste(tolower(citations$article_id), normalize_doi(citations$dataset_id))
}

citation_columns <- c("article_id", "article_title", "dataset_id", "node", "dataset")

write_citations <- function(citations, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  for (col in setdiff(citation_columns, names(citations))) citations[[col]] <- character()
  utils::write.csv(citations[citation_columns], path, row.names = FALSE, na = "")
}

read_citations <- function(path) {
  if (!file.exists(path)) {
    return(as.data.frame(stats::setNames(rep(list(character()), length(citation_columns)),
                                         citation_columns), stringsAsFactors = FALSE))
  }
  utils::read.csv(path, colClasses = "character", na.strings = "")
}

# Add citations to the running total: new pairs are appended, recording the
# search, phase and cycle that first found them and when; a pair already in
# the total gets this search added to its `sources`.
add_to_total <- function(found, path, source, phase, cycle, found_at) {
  total <- if (file.exists(path)) {
    utils::read.csv(path, colClasses = "character", na.strings = "")
  } else {
    NULL
  }
  if (nrow(found) == 0 && !is.null(total)) return(invisible(total))
  found <- found[!duplicated(pair_key(found)), , drop = FALSE]
  if (!is.null(total) && nrow(total)) {
    known <- match(pair_key(found), pair_key(total))
    for (k in which(!is.na(known))) {
      sources <- strsplit(total$sources[known[k]], ";", fixed = TRUE)[[1]]
      total$sources[known[k]] <- paste(unique(c(sources, source)), collapse = ";")
    }
    found <- found[is.na(known), , drop = FALSE]
  }
  new <- data.frame(found[citation_columns], sources = rep(source, nrow(found)),
                    first_phase = rep(phase, nrow(found)), first_cycle = rep(cycle, nrow(found)),
                    first_found = rep(found_at, nrow(found)), stringsAsFactors = FALSE)
  total <- rbind(total, new)
  if (nrow(new)) message("Added ", nrow(new), " new citations to ", path)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp", Sys.getpid())
  utils::write.csv(total, tmp, row.names = FALSE, na = "")
  file.rename(tmp, path)
  invisible(total)
}

schedule_status <- function(cycle, paths, ran, found = NULL) {
  others <- read_state(paths$others)
  status <- list(cycle = cycle$id, phase = cycle$phase, others = others$id,
                 others_status = others$status, ran = ran,
                 openalex = attr(found, "openalex_status")[c("complete", "stopped", "paused_until",
                                                             "credits_used")])
  message("Scheduled search: cycle ", cycle$id %||% "none", " is at ", cycle$phase %||% "-",
          "; other nodes' pass ", others$id %||% "none", " is ", others$status %||% "not started",
          if (length(ran)) paste0("; ran ", paste(ran, collapse = ", ")), ".")
  status
}
