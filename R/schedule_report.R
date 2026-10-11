# Reporting for the scheduled search: a summary line per track on every run,
# alerts for problems an operator needs to act on, and a status summary.
#
# Alerts are kept in state/alerts.json. A new alert is logged once, as an
# "ACTION NEEDED:" line in the job log and in state/alerts.log; while it
# stays open it is listed in state/status.md; when its condition clears, a
# "RESOLVED:" line is logged.

#' The alerts that apply now
#'
#' @param tracks the full-text tracks' states, named by source
#' @param text the OpenAlex full-text phase's progress (see text_progress())
#' @param collection problems collecting DOIs (state/collection.json)
#' @return a named list of alert messages, named by alert id
#' @noRd
current_alerts <- function(cycle, tracks, text, collection, now, metrics = NULL,
                           pass_cycles = 2, paused_days = 3, projection_days = 30,
                           metrics_stale_days = 3) {
  alerts <- list()
  number <- cycle$number %||% 0
  for (source in names(tracks)) {
    t <- tracks[[source]]
    label <- track_labels[[source]]
    if (identical(t$stopped, "no_key")) {
      alerts[[paste0("key:", source)]] <- paste0(
        "No API key is set for ", label, ", so the ", label, " track isn't running. Add the '",
        source, "' key to the api-keys Kubernetes secret (or set it with scythe_set_key()).")
    } else if (identical(t$stopped, "auth")) {
      alerts[[paste0("key:", source)]] <- paste0(
        label, " rejected the API key: ", t$last_error %||% "", " Fix the '", source,
        "' key in the api-keys Kubernetes secret; the ", label, " track is stopped until then.")
    }
    if (!is.null(t$crashed)) {
      alerts[[paste0("crash:", source)]] <- paste0(
        "The ", label, " track failed at ", t$crashed, ": ", t$last_error %||% "unknown error",
        ". Check the job log; the track is retried on each run.")
    }
    if (identical(t$status, "running") && !is.null(t$pass_number) &&
        number - t$pass_number >= pass_cycles) {
      p <- t$progress
      alerts[[paste0("slow:", source)]] <- paste0(
        label, " is still on its pass over cycle ", t$pass, "'s DOIs, ", number - t$pass_number,
        " cycles later (", fmt_count(p$searched), " of ", fmt_count(p$total), " searched). Its rate ",
        "limits are slower than the cycle: consider fewer priority nodes, less frequent cycles, ",
        "or leaving ", source, " out of the full-text sources.")
    }
    if (!is.null(t$paused_since) &&
        difftime(now, parse_utc(t$paused_since), units = "days") > paused_days) {
      alerts[[paste0("paused:", source)]] <- paste0(
        label, " has been paused since ", t$paused_since,
        if (!is.null(t$paused_until)) paste0(" (now until ", t$paused_until, ")"),
        " because it is ", if (identical(t$stopped, "failures")) "failing repeatedly" else "rate limited",
        ". Check the service's status, and the key's quota.")
    }
  }
  projected <- text[["projected_days"]]
  if (isTRUE(text[["full"]]) && is.numeric(projected) && length(projected) == 1 &&
      projected > projection_days) {
    alerts[["openalex-text-slow"]] <- paste0(
      "The OpenAlex full-text search's first full pass over cycle ", cycle$id, " is projected to take ",
      round(text$projected_days), " days (", text$done, " of ", text$total, " batches done in ",
      round(text$elapsed_days, 1), " days). Consider a larger OpenAlex budget or fewer priority nodes.")
  }
  if (!is.null(collection$priority_error)) {
    alerts[["dois:priority"]] <- paste0(
      "Collecting the priority nodes' DOIs failed (", collection$priority_error, "), so the new ",
      "cycle hasn't started. It is retried on each run; check the DataONE CN and member nodes.")
  }
  if (!is.null(collection$others_error)) {
    alerts[["dois:others"]] <- paste0(
      "Collecting the other nodes' DOIs from the CN failed (", collection$others_error,
      "). It is retried on each run; check the DataONE CN.")
  }
  if (!is.null(metrics$error) && !is.null(metrics$last_attempt)) {
    since <- metrics$checked
    # Measured from the last success, or from the first failure if none.
    from <- since %||% metrics$failing_since %||% metrics$last_attempt
    stale <- difftime(now, parse_utc(from), units = "days") > metrics_stale_days
    if (stale) {
      alerts[["metrics:stale"]] <- paste0(
        "The comparison with DataONE's metrics service hasn't succeeded ",
        if (is.null(since)) "yet" else paste0("since ", since), " (last error: ", metrics$error,
        "), so results/to-submit.csv is out of date. Check the service at ", metrics_url(),
        " (set with the scythe.metrics_url option).")
    }
  }
  if (!is.null(collection$drop)) {
    d <- collection$drop
    alerts[["dois:drop"]] <- paste0(
      "Cycle ", d$cycle, " collected ", fmt_count(d$count), " priority DOIs, far fewer than the ",
      fmt_count(d$previous), " of the previous cycle. Check the member nodes' indexes and the CN ",
      "(see state/dois/", d$cycle, "-sources.csv); the cycle continues with these DOIs.")
  }
  alerts
}

# Record which alerts are open: log the new ones and the resolved ones.
update_alerts <- function(dir, alerts, now) {
  path <- file.path(dir, "state", "alerts.json")
  log <- file.path(dir, "state", "alerts.log")
  open <- read_state(path) %||% list()
  stamp <- format_utc(now)
  lines <- character()
  for (id in setdiff(names(alerts), names(open))) {
    message("ACTION NEEDED: ", alerts[[id]])
    lines <- c(lines, paste0(stamp, " ACTION NEEDED [", id, "]: ", alerts[[id]]))
  }
  for (id in setdiff(names(open), names(alerts))) {
    message("RESOLVED: ", open[[id]]$message)
    lines <- c(lines, paste0(stamp, " RESOLVED [", id, "]: ", open[[id]]$message))
  }
  if (length(lines)) {
    dir.create(dirname(log), showWarnings = FALSE, recursive = TRUE)
    cat(lines, file = log, sep = "\n", append = TRUE)
  }
  updated <- lapply(stats::setNames(names(alerts), names(alerts)), function(id) {
    list(message = alerts[[id]], raised = open[[id]]$raised %||% stamp)
  })
  if (length(updated)) write_state(updated, path) else unlink(path)
  updated
}

fmt_count <- function(x) format(as.numeric(x %||% 0), big.mark = ",", scientific = FALSE, trim = TRUE)

short_node <- function(node) sub("^urn:node:", "", node %||% "")

# One line describing a full-text track.
track_line <- function(source, t) {
  label <- track_labels[[source]]
  if (is.null(t) || is.null(t[["pass"]])) return(paste0(label, ": not started"))
  p <- t$progress
  state <- switch(t$status %||% "idle",
    done = paste0("done with cycle ", t$pass, "'s DOIs"),
    running = paste0("on cycle ", t$pass, "'s DOIs"),
    t$status)
  progress <- if (!is.null(p) && identical(t$status, "running")) {
    paste0("; ", short_node(p$node), " ", fmt_count(p$node_searched), "/", fmt_count(p$node_total),
           ", overall ", fmt_count(p$searched), "/", fmt_count(p$total))
  } else ""
  stopped <- switch(t$stopped %||% "",
    rate_limited = paste0("; paused (rate limited) until ", t$paused_until %||% "?"),
    failures = paste0("; paused (repeated failures) until ", t$paused_until %||% "?"),
    auth = "; stopped: API key rejected",
    no_key = "; stopped: no API key",
    deadline = "; continues next run",
    "")
  paste0(label, ": ", state, progress, "; ", fmt_count(t$citations %||% 0), " citations",
         if (!is.null(t$failed) && t$failed > 0) paste0(", ", t$failed, " DOIs failed") else "",
         stopped)
}

openalex_line <- function(cycle, text, openalex) {
  phase <- switch(cycle$phase %||% "",
    priority_graph = "priority citation-graph search",
    priority_text = "priority full-text search",
    done = "priority phases done",
    "not started")
  progress <- if (identical(cycle$phase, "priority_text") && !is.null(text$total)) {
    paste0(" (", text$done, "/", text$total, " batches", if (isTRUE(text$full)) ", full pass" else "", ")")
  } else ""
  credits <- if (!is.null(openalex$credits_used)) paste0("; ", fmt_count(openalex$credits_used),
                                                        " credits this run") else ""
  stopped <- if (identical(openalex$stopped, "rate_limited")) {
    paste0("; paused (rate limited) until ", openalex$paused_until %||% "?")
  } else if (identical(openalex$stopped, "max_credits")) "; stopped at this run's credit limit" else ""
  paste0("OpenAlex: cycle ", cycle$id %||% "-", ", ", phase, progress, credits, stopped)
}

# Rewrite state/status.md.
write_status_report <- function(dir, cycle, others, tracks, text, openalex, alerts, now,
                                metrics = NULL, track_names = names(tracks)) {
  total <- read_citations_total(file.path(dir, "results", "citations.csv"))
  sources <- read_csv_if_exists(file.path(dir, "state", "dois", paste0(cycle$id %||% "", "-sources.csv")))
  lines <- c(
    "# Scythe scheduled search",
    "",
    paste0("Updated ", format_utc(now), "."),
    "",
    "## Alerts",
    "",
    if (length(alerts)) {
      vapply(names(alerts), function(id) paste0("- **ACTION NEEDED** (since ", alerts[[id]]$raised,
                                               "): ", alerts[[id]]$message), "")
    } else "None.",
    "",
    "## Priority cycle",
    "",
    if (is.null(cycle)) "No cycle has started yet." else c(
      paste0("Cycle ", cycle$id, " (number ", cycle$number %||% "?", "), started ", cycle$started,
             if (!is.null(cycle$completed)) paste0(", OpenAlex phases completed ", cycle$completed), "."),
      "",
      paste0("- ", openalex_line(cycle, text, openalex)),
      vapply(track_names, function(s) paste0("- ", track_line(s, tracks[[s]])), "")
    ),
    "",
    if (nrow(sources)) c(
      "| Node | DOIs | From | Excluded | CN records |",
      "|---|---|---|---|---|",
      sprintf("| %s | %s | %s | %s | %s |", short_node(sources$node), fmt_count(sources$dois),
              ifelse(sources$index == "member", "own index", "CN (fallback)"),
              fmt_count(as.numeric(sources$excluded) + as.numeric(sources$test_dois %||% 0)),
              fmt_count(sources$cn_count)),
      ""
    ),
    "## Other nodes",
    "",
    if (is.null(others)) "No pass over the other nodes has started yet." else paste0(
      "Pass ", others$id, ": ", others$status, ", started ", others$started,
      if (!is.null(others$completed)) paste0(", completed ", others$completed), "."),
    "",
    "## Metrics service",
    "",
    if (is.null(metrics$checked) && is.null(metrics$last_attempt)) {
      "Not compared yet."
    } else c(
      if (!is.null(metrics$checked)) paste0(
        "Compared ", metrics$checked, ": ", fmt_count(metrics$yes), " already recorded, ",
        fmt_count(metrics$no), " to submit (results/to-submit.csv)",
        if (isTRUE(metrics$unmapped > 0)) paste0(", ", fmt_count(metrics$unmapped),
                                                " without a DataONE identifier"), "."),
      if (!is.null(metrics$error)) paste0("The last attempt (", metrics$last_attempt, ") failed: ",
                                          metrics$error, ".")
    ),
    "",
    "## Citations found",
    "",
    paste0(fmt_count(nrow(total)), " in total (results/citations.csv)",
           if (nrow(total)) paste0(": ", paste(names(source_counts(total)), fmt_count(source_counts(total)),
                                              sep = " ", collapse = ", ")) else "", ".")
  )
  path <- file.path(dir, "state", "status.md")
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp", Sys.getpid())
  writeLines(lines, tmp)
  file.rename(tmp, path)
}

read_csv_if_exists <- function(path) {
  if (file.exists(path)) utils::read.csv(path, colClasses = "character", na.strings = "") else data.frame()
}

read_citations_total <- function(path) {
  x <- read_csv_if_exists(path)
  if (nrow(x) == 0) data.frame(sources = character()) else x
}

# Citations per source (a citation found by several sources counts for each).
source_counts <- function(total) {
  sort(table(unlist(strsplit(total$sources, ";", fixed = TRUE))), decreasing = TRUE)
}
