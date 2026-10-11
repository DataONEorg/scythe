# A lock that lets only one scheduled search run at a time in a directory.
# The CronJob already doesn't overlap its own runs, but a run started by hand,
# or by a second release sharing the volume, could otherwise rewrite the same
# results and state files at the same time.
#
# The lock is a directory, state/run.lock, because creating a directory is
# atomic: only one run can succeed. It holds info.json recording the host,
# process and start time. A lock older than `stale_hours` was left by a run
# that died, and is taken over.

run_lock_path <- function(dir) file.path(dir, "state", "run.lock")

acquire_run_lock <- function(dir, stale_hours = 24) {
  lock <- run_lock_path(dir)
  dir.create(dirname(lock), showWarnings = FALSE, recursive = TRUE)
  info <- list(host = Sys.info()[["nodename"]], pid = Sys.getpid(), started = format_utc(http_now()))
  take <- function() {
    if (!dir.create(lock, showWarnings = FALSE)) return(FALSE)
    write_state(info, file.path(lock, "info.json"))
    TRUE
  }
  if (take()) return(TRUE)

  held <- read_state(file.path(lock, "info.json"))
  age <- if (!is.null(held$started)) {
    as.numeric(difftime(http_now(), parse_utc(held$started), units = "hours"))
  } else {
    # Just created by another run that hasn't written its info yet.
    as.numeric(difftime(Sys.time(), file.info(lock)$mtime, units = "hours"))
  }
  holder <- paste0(held$host %||% "an unknown host", " process ", held$pid %||% "?",
                   ", started ", held$started %||% "at an unknown time")
  if (is.na(age) || age < stale_hours) {
    message("Another run (", holder, ") is in progress in ", dir, "; exiting.")
    return(FALSE)
  }
  # Move the stale lock aside (only one run can), check it is the one we
  # judged stale, then take the lock.
  aside <- paste0(lock, ".stale", Sys.getpid())
  if (!file.rename(lock, aside)) {
    message("Another run is taking over the lock in ", dir, "; exiting.")
    return(FALSE)
  }
  moved <- read_state(file.path(aside, "info.json"))
  if (!identical(moved$started, held$started) || !identical(moved$pid, held$pid)) {
    file.rename(aside, lock)
    message("Another run took over the lock in ", dir, "; exiting.")
    return(FALSE)
  }
  unlink(aside, recursive = TRUE)
  message("Taking over the lock left by a run that didn't finish (", holder, ").")
  take()
}

release_run_lock <- function(dir) {
  lock <- run_lock_path(dir)
  held <- read_state(file.path(lock, "info.json"))
  # Only release our own lock (a stale-lock takeover may have replaced it).
  if (identical(as.integer(held$pid), Sys.getpid()) && identical(held$host, Sys.info()[["nodename"]])) {
    unlink(lock, recursive = TRUE)
  }
}

# Remove temporary files left by runs that died mid-write. Only called while
# holding the lock, before any track starts, so none are in use.
remove_leftover_temp_files <- function(dir) {
  tmp <- list.files(dir, pattern = "[.]tmp[0-9]+$", recursive = TRUE, full.names = TRUE, all.files = TRUE)
  stale <- list.files(file.path(dir, "state"), pattern = "^run[.]lock[.]stale", full.names = TRUE)
  if (length(tmp) + length(stale)) {
    message("Removing ", length(tmp) + length(stale), " temporary files left by an earlier run.")
    unlink(c(tmp, stale), recursive = TRUE)
  }
}
