# The full-text tracks of the scheduled search: one per source that is
# searched one DOI at a time (xDD, Springer, Scopus and PLOS). Each track
# works through a priority cycle's DOI list in node priority order, at its own
# pace, recording its progress so that each hourly run continues where the
# last one stopped. A track that hasn't finished its pass when a new cycle
# starts finishes it first, then starts on the newest cycle's list.
#
# A track's state is in state/tracks/<source>.json; its checkpoint (the DOIs
# searched so far in its pass) in state/tracks/<source>/<cycle>/; and its
# rate-limit state, kept across passes, in state/tracks/<source>-state.json.
# Each track writes only its own files, so tracks can run in parallel.

track_sources <- c("xdd", "springer", "scopus", "plos")

track_labels <- c(xdd = "xDD", springer = "Springer", scopus = "Scopus", plos = "PLOS",
                  openalex = "OpenAlex full text")

track_search <- function(source) {
  switch(source,
         xdd = citation_search_xdd, springer = citation_search_springer,
         scopus = citation_search_scopus, plos = citation_search_plos,
         stop("Unknown track source: ", source))
}

track_paths <- function(dir, source) {
  base <- file.path(dir, "state", "tracks")
  list(state = file.path(base, paste0(source, ".json")),
       limiter = file.path(base, paste0(source, "-state.json")),
       checkpoints = file.path(base, source))
}

# The newest cycle whose DOI list a track may search: one whose priority
# graph search is done, so that its graph results come first.
track_ready_cycle <- function(cycle) {
  if (is.null(cycle) || cycle$phase == "priority_graph") cycle$previous_ready else cycle$id
}

#' Run one full-text track for up to `deadline`
#'
#' @return a list with the track's state (`track`) and the citations found
#'   in its pass so far (`found`, annotated with node and dataset), or NULL
#'   `found` when the track had nothing to do
#' @noRd
run_track <- function(source, dir, priority_nodes, deadline = NULL) {
  paths <- track_paths(dir, source)
  cycle <- read_state(file.path(dir, "state", "cycle.json"))
  track <- read_state(paths$state) %||% list(source = source, status = "idle")
  now <- format_utc(http_now())
  label <- track_labels[[source]]

  # Start a pass on the newest ready cycle once the last pass is done.
  ready <- track_ready_cycle(cycle)
  if (track$status != "running") {
    if (is.null(ready) || identical(track[["pass"]], ready)) {
      return(list(track = track, found = NULL))
    }
    message(label, ": starting a pass over the DOIs of cycle ", ready)
    track <- list(source = source, status = "running", pass = ready,
                  pass_number = cycle_number(dir, ready), started = now,
                  last_completed = track$last_completed)
  }

  dois <- read_doi_list(file.path(dir, "state", "dois", paste0(track$pass, "-priority.csv")))
  dois <- dois[order(match(dois$node, priority_nodes)), , drop = FALSE]
  checkpoint_dir <- file.path(paths$checkpoints, track$pass)

  if (source %in% c("springer", "scopus") && is.na(scythe_get_key(source))) {
    track$stopped <- "no_key"
    track$last_error <- paste0("no API key is set for ", label)
    write_state(track, paths$state)
    return(list(track = track, found = NULL))
  }

  auth_error <- NULL
  found <- withCallingHandlers(
    track_search(source)(dois$doi, checkpoint_dir = checkpoint_dir,
                         state_file = paths$limiter, deadline = deadline),
    scythe_auth_error = function(w) auth_error <<- conditionMessage(w)
  )
  status <- attr(found, "search_status")
  found <- annotate_citations(found, dois)

  searched <- names(read_source_checkpoint(checkpoint_dir, source))
  track$progress <- track_progress(dois, searched)
  track$failed <- length(status$failed)
  track$citations <- nrow(found)
  track$stopped <- status$stopped
  track$last_error <- auth_error
  track$paused_until <- status$paused_until
  # Remember when a pause (other than the end of the time slice) began.
  paused <- isTRUE(status$stopped %in% c("rate_limited", "failures", "auth"))
  track$paused_since <- if (paused) track$paused_since %||% now
  track$last_run <- now
  track$crashed <- NULL

  if (status$pending == 0) {
    track$status <- "done"
    track$completed <- now
    track$last_completed <- track$pass
    write_citations(found, file.path(dir, "results", track$pass, paste0("priority-", source, ".csv")))
    unlink(checkpoint_dir, recursive = TRUE)
    message(label, ": finished the pass over cycle ", track$pass, "'s DOIs, finding ",
            nrow(found), " citations.")
  }
  write_state(track, paths$state)
  list(track = track, found = found)
}

# How far a track has got: DOIs searched overall, and in the node it's on.
track_progress <- function(dois, searched) {
  done <- dois$doi %in% searched
  remaining <- dois$node[!done]
  node <- if (length(remaining)) remaining[1] else dois$node[length(dois$node)]
  in_node <- dois$node %in% node
  list(searched = sum(done), total = nrow(dois), node = node %||% NA_character_,
       node_searched = sum(done & in_node), node_total = sum(in_node))
}

# A cycle's number (1 for the first cycle), from its saved DOI list's place
# among all the saved lists.
cycle_number <- function(dir, id) {
  lists <- sort(sub("-priority[.]csv$", "", list.files(file.path(dir, "state", "dois"),
                                                         pattern = "-priority[.]csv$")))
  match(id, lists)
}

#' Start the full-text tracks, in parallel when asked
#'
#' Each track runs in its own R process (with callr), given the API keys from
#' this process so that the children never need the keyring, and stops by
#' `deadline`. finish_tracks() waits for them and returns their results. When
#' `parallel` is FALSE, finish_tracks() runs the tracks one after another in
#' this process instead.
#' @noRd
start_tracks <- function(sources, dir, priority_nodes, deadline, parallel = TRUE) {
  h <- list(sources = sources, dir = dir, priority_nodes = priority_nodes,
            deadline = deadline, procs = NULL)
  if (!parallel || length(sources) == 0) return(h)
  # Only the keys of the tracks being started, so no other key is read.
  keyed <- intersect(sources, c("springer", "scopus"))
  keys <- vapply(stats::setNames(keyed, keyed), function(k) as.character(scythe_get_key(k)), "")
  env <- c(callr::rcmd_safe_env(), keys[!is.na(keys)])
  opts <- options()[grepl("^scythe[.]", names(options()))]
  h$procs <- lapply(sources, function(s) {
    callr::r_bg(
      function(source, dir, priority_nodes, deadline, opts) {
        options(opts)
        # The child process runs the installed package's run_track().
        run_track <- utils::getFromNamespace("run_track", "scythe")
        run_track(source, dir, priority_nodes, deadline)
      },
      args = list(source = s, dir = dir, priority_nodes = priority_nodes, deadline = deadline,
                  opts = opts),
      env = env, stdout = "|", stderr = "2>&1", supervise = TRUE
    )
  })
  h
}

#' Wait for the tracks and collect their results
#'
#' Each track stops itself at its deadline. One still running `grace`
#' seconds after that (hung on a connection, say) is stopped and reported as
#' failed, so that the run always finishes.
#' @return a list of run_track() results, named by source
#' @noRd
finish_tracks <- function(h, grace = 600) {
  if (is.null(h$procs)) {
    return(stats::setNames(lapply(h$sources, function(s) {
      tryCatch(run_track(s, h$dir, h$priority_nodes, h$deadline),
               error = function(e) track_crashed(s, h$dir, conditionMessage(e)))
    }), h$sources))
  }
  until <- Sys.time() + max(0, as.numeric(difftime(h$deadline, http_now(), units = "secs"))) + grace
  results <- lapply(seq_along(h$procs), function(k) {
    p <- h$procs[[k]]
    # processx's wait() takes its timeout in milliseconds.
    p$wait(timeout = max(1, as.numeric(difftime(until, Sys.time(), units = "secs"))) * 1000)
    timed_out <- p$is_alive()
    if (timed_out) p$kill()
    output <- tryCatch(p$read_all_output_lines(), error = function(e) character())
    if (length(output)) message(paste0("[", h$sources[k], "] ", output, collapse = "\n"))
    if (timed_out) {
      return(track_crashed(h$sources[k], h$dir, paste0(
        "it was still running ", round(grace / 60), " minutes after its time limit, so it was stopped")))
    }
    tryCatch(p$get_result(), error = function(e) track_crashed(h$sources[k], h$dir, conditionMessage(e)))
  })
  stats::setNames(results, h$sources)
}

# A track whose process failed: record the error so it is reported.
track_crashed <- function(source, dir, error) {
  paths <- track_paths(dir, source)
  track <- read_state(paths$state) %||% list(source = source, status = "idle")
  track$last_error <- paste("the track failed:", error)
  track$crashed <- format_utc(http_now())
  write_state(track, paths$state)
  message(track_labels[[source]], ": the track failed: ", error)
  list(track = track, found = NULL)
}
