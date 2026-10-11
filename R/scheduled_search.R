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
#   state/tracks/            the full-text tracks' state (see tracks.R)
#   state/collection.json    problems collecting DOIs, for alerts
#   state/alerts.json, alerts.log, status.md   reporting (see schedule_report.R)
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
#' collects the priority nodes' DOIs (see [collect_dataone_dois()]) and runs
#' the OpenAlex citation-graph search over them. Then the full-text searches
#' work through them in node priority order: the OpenAlex full-text search,
#' and a track for each of xDD, Springer, Scopus and PLOS. The tracks run in
#' parallel, each for up to `track_minutes` per call, keeping to its own rate
#' limits; a track that hasn't finished when the next cycle starts finishes
#' its pass first. Once a cycle's OpenAlex searches are done, a pass of the
#' citation-graph search over the other nodes runs, if one is due. A search
#' stopped by a rate limit continues on a later call from its checkpoints.
#'
#' Citations are added to `results/citations.csv` in `dir` as they are found,
#' one row per pair of dataset DOI and citing article, recording which
#' searches found it and when it was first found. When a search completes,
#' its citations are also written to `results/<cycle>/`: `priority-graph.csv`,
#' `priority-text.csv` (OpenAlex full text), `priority-<source>.csv` for each
#' track, `priority-text-new.csv` (full-text citations the graph search didn't
#' find), and `others-graph.csv` for a pass over the other nodes.
#'
#' Each call logs a summary line for the OpenAlex searches and each track.
#' Problems that need an operator are logged once as `ACTION NEEDED:` lines,
#' and recorded in `state/alerts.log`, and `state/status.md` summarizes the
#' search's state and any open alerts.
#'
#' @param priority_nodes member nodes searched every cycle, from their own
#'   indexes, in priority order
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
#'   OpenAlex full-text search, from which a later cycle's full-text search
#'   looks for newly published works. A cycle with no earlier complete one
#'   searches all works. A longer lookback catches more works that OpenAlex
#'   indexed late, but costs more credits.
#' @param full_text_every_cycles if more than 0, every this many cycles the
#'   OpenAlex full-text search searches all works, not just recent ones, to
#'   catch works missed by the lookback
#' @param text_sources the sources with full-text tracks: any of "xdd",
#'   "springer", "scopus" and "plos"
#' @param track_minutes the longest each track runs per call
#' @param parallel whether the tracks run in parallel processes
#' @param max_credits the most OpenAlex credits one call may spend
#' @param alert_pass_cycles alert when a track's pass has spanned this many
#'   cycles
#' @param alert_paused_days alert when a track has been paused this many days
#' @param alert_projection_days alert when the OpenAlex full-text search's
#'   first full pass is projected to take longer than this many days
#' @param metrics_check_hours how often to compare the citations found with
#'   DataONE's metrics service (see [get_metrics_citations()]), marking each
#'   in `results/citations.csv` (`in_metrics`: "yes" if the service has the
#'   same citing work and target, "no" if not, or "unmapped" for a citation
#'   without a DataONE identifier, which is never submitted) and writing the
#'   ones to submit to `results/to-submit.csv`; 0 to never compare
#' @param lock_stale_hours a run lock older than this was left by a run that
#'   died, and is taken over; only one run at a time works in `dir`
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
                             full_text_every_cycles = 0,
                             text_sources = track_sources,
                             track_minutes = 50,
                             parallel = TRUE,
                             max_credits = Inf,
                             alert_pass_cycles = 2,
                             alert_paused_days = 3,
                             alert_projection_days = 30,
                             metrics_check_hours = 24,
                             lock_stale_hours = 24,
                             log_dir = file.path(dir, "logs"),
                             cn_url = dataone_cn) {
  paths <- schedule_paths(dir)
  if (!acquire_run_lock(dir, lock_stale_hours)) {
    return(invisible(list(locked = TRUE, ran = character())))
  }
  on.exit(release_run_lock(dir), add = TRUE)
  remove_leftover_temp_files(dir)
  ran <- character()
  openalex <- NULL
  now <- http_now()
  today <- as.Date(now, tz = "UTC")
  collection <- read_state(paths$collection) %||% list()
  phase_run <- function(dois, method, since, checkpoint_dir, cycle, phase) {
    ran <<- c(ran, phase)
    found <- citation_search_openalex(dois$doi, methods = method, since = since,
                                      log_dir = log_dir, checkpoint_dir = checkpoint_dir,
                                      state_file = paths$openalex_state,
                                      max_credits = max_credits, preflight = FALSE)
    openalex <<- attr(found, "openalex_status")
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
      collection$priority_error <- conditionMessage(dois)
      message("Couldn't collect the priority nodes' DOIs (", conditionMessage(dois),
              "); trying again on the next run.")
    } else {
      collection$priority_error <- NULL
      collection$drop <- if (!is.null(cycle$dois) && nrow(dois) < 0.5 * cycle$dois) {
        list(cycle = id, count = nrow(dois), previous = cycle$dois)
      }
      write_doi_list(dois, file.path(paths$dois, paste0(id, "-priority.csv")))
      if (!is.null(attr(dois, "doi_sources"))) {
        write_csv_atomic(attr(dois, "doi_sources"), file.path(paths$dois, paste0(id, "-sources.csv")))
      }
      cycle <- list(id = id, number = (cycle$number %||% 0) + 1, started = format_utc(now),
                    phase = "priority_graph", dois = nrow(dois),
                    text_since_base = cycle$text_since_base, previous_ready = cycle$id)
      write_state(cycle, paths$cycle)
    }
    write_state(collection, paths$collection)
  }

  # The full-text tracks run alongside the OpenAlex searches.
  tracks <- start_tracks(text_sources, dir, priority_nodes, now + track_minutes * 60, parallel)

  # The priority OpenAlex phases.
  text <- cycle[["text"]]
  if (!is.null(cycle) && cycle$phase != "done") {
    dois <- read_doi_list(file.path(paths$dois, paste0(cycle$id, "-priority.csv")))
    results_dir <- file.path(paths$results, cycle$id)
    if (cycle$phase == "priority_graph") {
      checkpoint <- file.path(paths$openalex, cycle$id, "priority-graph")
      found <- phase_run(dois, "graph", NULL, checkpoint, cycle$id, "priority_graph")
      if (openalex_complete(found)) {
        write_citations(found, file.path(results_dir, "priority-graph.csv"))
        unlink(checkpoint, recursive = TRUE)
        cycle$phase <- "priority_text"
        write_state(cycle, paths$cycle)
      }
    }
    if (cycle$phase == "priority_text") {
      full <- is.null(cycle$text_since_base) ||
        (full_text_every_cycles > 0 && cycle$number %% full_text_every_cycles == 0)
      since <- if (!full) format(as.Date(cycle$text_since_base) - lookback)
      checkpoint <- file.path(paths$openalex, cycle$id, "priority-text")
      cycle$text_started <- cycle$text_started %||% format_utc(now)
      found <- phase_run(dois, "text", since, checkpoint, cycle$id, "priority_text")
      text <- text_progress(dois, checkpoint, full, cycle$text_started, http_now())
      cycle[["text"]] <- Filter(Negate(is.null), text)
      if (openalex_complete(found)) {
        write_citations(found, file.path(results_dir, "priority-text.csv"))
        update_text_new(results_dir)
        unlink(checkpoint, recursive = TRUE)
        cycle$phase <- "done"
        cycle$text_since_base <- format(as.Date(parse_utc(cycle$started)))
        cycle$completed <- format_utc(http_now())
        message("Priority cycle ", cycle$id, "'s OpenAlex searches are complete.")
      }
      write_state(cycle, paths$cycle)
    }
  }

  # The pass over the other nodes, once no priority OpenAlex work is pending.
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
        collection$others_error <- conditionMessage(dois)
        message("Couldn't collect the other nodes' DOIs (", conditionMessage(dois),
                "); trying again on the next run.")
      } else {
        collection$others_error <- NULL
        write_doi_list(dois, file.path(paths$dois, paste0("others-", id, ".csv")))
        others <- list(id = id, started = format_utc(now), status = "running")
        write_state(others, paths$others)
      }
      write_state(collection, paths$collection)
    }
    if (!is.null(others) && others$status == "running") {
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
    }
  }

  # Collect the tracks' citations; only this process writes the running total.
  track_results <- finish_tracks(tracks)
  for (source in names(track_results)) {
    r <- track_results[[source]]
    if (is.null(r$found)) next
    add_to_total(r$found, paths$total, source = source, phase = "priority_text",
                 cycle = r$track$pass, found_at = format_utc(now))
    if (identical(r$track$status, "done") && identical(r$track$completed, r$track$last_run)) {
      update_text_new(file.path(paths$results, r$track$pass))
    }
  }
  # Re-add each track's last completed pass, so that citations a run found
  # but died before adding (after the track finished its pass) aren't lost.
  # Pairs already in the total are skipped, so this is usually a no-op.
  for (source in text_sources) {
    done <- read_state(track_paths(dir, source)$state)$last_completed
    file <- if (!is.null(done)) file.path(paths$results, done, paste0("priority-", source, ".csv"))
    if (!is.null(file) && file.exists(file)) {
      add_to_total(read_citations(file), paths$total, source = source, phase = "priority_text",
                   cycle = done, found_at = format_utc(now))
    }
  }

  # Compare with the metrics service, about once a day.
  metrics <- check_metrics(paths, http_now(), metrics_check_hours)

  # Report.
  track_states <- lapply(stats::setNames(text_sources, text_sources),
                         function(s) read_state(track_paths(dir, s)$state))
  message(openalex_line(cycle, text, openalex))
  for (s in text_sources) message(track_line(s, track_states[[s]]))
  alerts <- current_alerts(cycle, track_states, text, collection, http_now(), metrics = metrics,
                           pass_cycles = alert_pass_cycles, paused_days = alert_paused_days,
                           projection_days = alert_projection_days)
  open <- update_alerts(dir, alerts, http_now())
  others <- read_state(paths$others)
  write_status_report(dir, cycle, others, track_states, text, openalex, open, http_now(), metrics)
  invisible(list(cycle = cycle$id, phase = cycle$phase, others = others$id,
                 others_status = others$status, ran = ran, openalex = openalex,
                 tracks = track_states, alerts = names(open)))
}

# How far the OpenAlex full-text phase has got, in batches, and how long a
# full pass is projected to take once it has run for at least a day.
text_progress <- function(dois, checkpoint_dir, full, started, now) {
  searchable <- dois$doi[!openalex_unsearchable(dois$doi)]
  total <- length(chunk_for_search(unique(searchable), 100))
  done <- sum(startsWith(names(read_checkpoint(checkpoint_dir)), "text "))
  elapsed <- as.numeric(difftime(now, parse_utc(started), units = "days"))
  list(full = full, total = total, done = min(done, total), started = started,
       elapsed_days = elapsed,
       projected_days = if (done > 0 && elapsed >= 1) elapsed * total / done)
}

# Rewrite a cycle's priority-text-new.csv: the citations found by any
# full-text search so far that the citation-graph search didn't find.
update_text_new <- function(results_dir) {
  files <- c(file.path(results_dir, "priority-text.csv"),
             file.path(results_dir, paste0("priority-", track_sources, ".csv")))
  text <- do.call(rbind, lapply(files[file.exists(files)], read_citations))
  if (is.null(text)) return(invisible())
  graph <- read_citations(file.path(results_dir, "priority-graph.csv"))
  text <- text[!pair_key(text) %in% pair_key(graph) & !duplicated(pair_key(text)), , drop = FALSE]
  write_citations(text, file.path(results_dir, "priority-text-new.csv"))
}

schedule_paths <- function(dir) {
  list(
    cycle = file.path(dir, "state", "cycle.json"),
    others = file.path(dir, "state", "others.json"),
    dois = file.path(dir, "state", "dois"),
    openalex = file.path(dir, "state", "openalex"),
    openalex_state = file.path(dir, "state", "openalex-state.json"),
    collection = file.path(dir, "state", "collection.json"),
    metrics = file.path(dir, "state", "metrics.json"),
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

# Add each citation's target: the identifier literal in DataONE's system
# metadata that the searched DOI belongs to (`target_id`), whether it is a
# PID or a SID (`target_type`), and its node. The searches only report the
# DOIs they were given, so a citation without a target would be a bug; it is
# left with no target_id and never submitted.
annotate_citations <- function(found, dois) {
  keep <- attributes(found)[c("openalex_status", "search_status")]
  i <- match(normalize_doi(found$dataset_id), dois$doi)
  found$target_id <- dois$identifier[i]
  found$target_type <- ifelse(dois$field[i] == "seriesId", "sid", "pid")
  found$node <- dois$node[i]
  if (anyNA(i)) {
    message("Couldn't find the DataONE identifier of ", sum(is.na(i)), " cited DOIs (for example ",
            found$dataset_id[is.na(i)][1], "); they won't be submitted.")
  }
  for (a in names(keep)) if (!is.null(keep[[a]])) attr(found, a) <- keep[[a]]
  found
}

pair_key <- function(citations) {
  paste(tolower(citations$article_id), normalize_doi(citations$dataset_id))
}

citation_columns <- c("article_id", "article_title", "dataset_id", "target_id", "target_type", "node")

write_citations <- function(citations, path) {
  for (col in setdiff(citation_columns, names(citations))) {
    citations[[col]] <- rep(NA_character_, nrow(citations))
  }
  write_csv_atomic(citations[citation_columns], path)
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
  changed <- is.null(total)
  if (!is.null(total) && nrow(total)) {
    known <- match(pair_key(found), pair_key(total))
    for (k in which(!is.na(known))) {
      sources <- strsplit(total$sources[known[k]], ";", fixed = TRUE)[[1]]
      if (!source %in% sources) {
        total$sources[known[k]] <- paste(c(sources, source), collapse = ";")
        changed <- TRUE
      }
    }
    found <- found[is.na(known), , drop = FALSE]
  }
  new <- data.frame(found[citation_columns], sources = rep(source, nrow(found)),
                    first_phase = rep(phase, nrow(found)), first_cycle = rep(cycle, nrow(found)),
                    first_found = rep(found_at, nrow(found)), stringsAsFactors = FALSE)
  # Columns added to the total later (such as in_metrics) are blank for new rows.
  for (col in setdiff(names(total), names(new))) new[[col]] <- rep(NA_character_, nrow(new))
  if (!changed && nrow(new) == 0) return(invisible(total))
  total <- rbind(total, new[names(total %||% new)])
  if (nrow(new)) message("Added ", nrow(new), " new citations to ", path)
  write_csv_atomic(total, path)
  invisible(total)
}

# Write a CSV file through a temporary file and a rename, so that a reader
# (another process, or a later run after a crash) never sees part of one.
write_csv_atomic <- function(df, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp", Sys.getpid())
  utils::write.csv(df, tmp, row.names = FALSE, na = "")
  if (!file.rename(tmp, path)) stop("Couldn't replace ", path)
  invisible(path)
}
