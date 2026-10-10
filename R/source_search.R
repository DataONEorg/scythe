# Searching the sources that take one request per identifier (Scopus,
# Springer, xDD and PLOS) through the shared rate limiter, with progress saved
# so that an interrupted search continues where it stopped.

empty_citations <- function() {
  data.frame(article_id = character(), article_title = character(),
             dataset_id = character(), source = character(), stringsAsFactors = FALSE)
}

# A human-readable message from an error response body, if it has one.
response_message <- function(res) {
  body <- tryCatch(jsonlite::fromJSON(res$body %||% "", simplifyVector = FALSE),
                   error = function(e) NULL)
  msg <- body$message %||% body$error$message %||%
    body$`service-error`$status$statusText %||% body$error
  if (is.character(msg) && length(msg) == 1) msg else NULL
}

# How to handle a response: "ok", "rate_limited", "auth" (the API key was
# rejected), "retry" (a server or network error) or "failed" (any other error,
# which only affects this identifier).
classify_response <- function(res) {
  code <- res$status
  if (is.na(code) || code >= 500) return("retry")
  if (code == 200) return("ok")
  if (code == 429) return("rate_limited")
  if (code %in% c(401, 403)) {
    # Some services answer an exhausted quota with 403.
    msg <- response_message(res) %||% ""
    return(if (grepl("rate|quota|limit|throttl", msg, ignore.case = TRUE)) "rate_limited" else "auth")
  }
  "failed"
}

describe_response <- function(res) {
  if (is.na(res$status)) return(paste("network error:", res$error %||% "no response"))
  msg <- response_message(res)
  paste0("HTTP ", res$status, if (!is.null(msg)) paste0(": ", msg))
}

# Completed identifiers are recorded in a JSON Lines checkpoint, one line per
# identifier with the citations found, so that a rerun with the same
# checkpoint directory skips them.
source_checkpoint_path <- function(checkpoint_dir, source) {
  file.path(checkpoint_dir, paste0(source, "-checkpoint.jsonl"))
}

read_source_checkpoint <- function(checkpoint_dir, source) {
  if (is.null(checkpoint_dir)) return(list())
  path <- source_checkpoint_path(checkpoint_dir, source)
  if (!file.exists(path)) return(list())
  records <- lapply(readLines(path, encoding = "UTF-8", warn = FALSE), function(line) {
    tryCatch(jsonlite::fromJSON(line, simplifyVector = TRUE), error = function(e) NULL)
  })
  records <- Filter(function(r) !is.null(r$identifier), records)
  stats::setNames(lapply(records, function(r) {
    if (length(r$citations) == 0) {
      data.frame(article_id = character(), article_title = character(), stringsAsFactors = FALSE)
    } else {
      as.data.frame(r$citations, stringsAsFactors = FALSE)
    }
  }), vapply(records, `[[`, "", "identifier"))
}

write_source_checkpoint <- function(checkpoint_dir, source, identifier, citations) {
  if (is.null(checkpoint_dir)) return(invisible())
  dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
  line <- jsonlite::toJSON(list(identifier = identifier, citations = citations),
                           auto_unbox = TRUE, na = "null", dataframe = "rows")
  cat(line, "\n", file = source_checkpoint_path(checkpoint_dir, source), sep = "", append = TRUE)
}

#' Search a source one identifier at a time
#'
#' Sends one request per identifier through `limiter`, retrying server and
#' network errors up to 3 times. A rate limit pauses the search (see
#' limiter_ready()); a rejected API key stops it; any other error skips that
#' identifier, and a run of them is treated as an unreported rate limit.
#'
#' @param source the source's name, recorded in the `source` column
#' @param identifiers the identifiers to search for
#' @param request a function of an identifier returning a list with the `url`
#'   and request `headers`
#' @param parse a function of a parsed JSON response body returning a
#'   data.frame with `article_id` and `article_title` columns
#' @param limiter the source's rate limiter
#' @param quota optionally, a function of the response headers returning the
#'   `remaining` quota and the seconds until it `reset`s
#' @param no_results optionally, a function of an error response returning
#'   TRUE when it just means nothing was found (Springer answers a search
#'   without results with a 404)
#' @param checkpoint_dir directory recording completed identifiers, or NULL
#' @return a data.frame of citations, with a `search_status` attribute
#' @noRd
search_by_identifier <- function(source, identifiers, request, parse, limiter,
                                 quota = NULL, no_results = NULL, checkpoint_dir = NULL) {
  on.exit(limiter_save(limiter), add = TRUE)
  label <- limiter$label
  found <- read_source_checkpoint(checkpoint_dir, source)
  todo <- setdiff(unique(identifiers), names(found))
  cached <- length(unique(identifiers)) - length(todo)
  if (cached > 0) {
    message(label, ": ", cached, " identifiers were already searched (in ", checkpoint_dir,
            "); searching the other ", length(todo), ".")
  }
  failed <- character()
  errors <- character()
  i <- 1
  attempts <- 0
  while (i <= length(todo)) {
    if (!limiter_ready(limiter)) break
    id <- todo[i]
    req <- request(id)
    limiter_record_requests(limiter)
    res <- http_get(req$url, req$headers %||% list())[[1]]
    q <- if (!is.null(quota)) quota(res$headers) else list(remaining = NA, reset = NA)
    kind <- classify_response(res)
    if (kind != "ok" && !is.null(no_results) && isTRUE(no_results(res))) {
      res$body <- "{}"
      kind <- "ok"
    }
    if (kind == "ok") {
      citations <- tryCatch({
        rows <- parse(jsonlite::fromJSON(res$body, simplifyVector = FALSE))
        rows[c("article_id", "article_title")]
      }, error = function(e) e)
      if (inherits(citations, "error")) {
        failed <- c(failed, id)
        errors <- c(errors, paste("unreadable response:", conditionMessage(citations)))
        limiter_failure(limiter)
        i <- i + 1
        attempts <- 0
        next
      } else {
        limiter_success(limiter)
        found[[id]] <- citations
        write_source_checkpoint(checkpoint_dir, source, id, citations)
        limiter_note_quota(limiter, q$remaining, q$reset)
        i <- i + 1
        attempts <- 0
        next
      }
    }
    if (kind == "rate_limited") {
      wait <- header_number(res$headers, "retry-after")
      if (is.na(wait)) wait <- q$reset
      limiter_rate_limited(limiter, wait, paste0("rate limited (", describe_response(res), ")"))
    } else if (kind == "auth") {
      limiter_auth_failed(limiter, describe_response(res))
      break
    } else if (kind == "retry" && attempts < 3) {
      attempts <- attempts + 1
      wait <- header_number(res$headers, "retry-after")
      http_sleep(if (is.na(wait)) min(60, 2^attempts) else wait)
    } else {
      failed <- c(failed, id)
      errors <- c(errors, describe_response(res))
      limiter_failure(limiter)
      i <- i + 1
      attempts <- 0
    }
  }

  ids <- intersect(unique(identifiers), names(found))
  rows <- lapply(ids, function(id) {
    df <- found[[id]]
    if (nrow(df) == 0) return(NULL)
    data.frame(article_id = as.character(df$article_id),
               article_title = as.character(df$article_title),
               dataset_id = id, source = source, stringsAsFactors = FALSE)
  })
  results <- do.call(rbind, c(list(empty_citations()), rows))

  pending <- setdiff(todo, c(names(found), failed))
  if (length(failed)) {
    scythe_warning("scythe_request_failed", service = source, identifiers = failed,
                   paste0(label, ": ", length(failed), " of ", length(todo),
                          " identifiers could not be searched and were skipped (first: '",
                          failed[1], "', ", errors[1], ").",
                          if (!is.null(checkpoint_dir)) " They are retried on the next run."))
  }
  attr(results, "search_status") <- list(
    source = source,
    complete = length(pending) == 0 && length(failed) == 0,
    stopped = limiter$stopped,
    paused_until = if (!is.null(limiter$paused_until)) format_utc(limiter$paused_until),
    searched = length(todo) - length(pending) - length(failed),
    cached = cached,
    failed = failed,
    pending = length(pending),
    requests = limiter$requests
  )
  results
}

# A source's limiter, with its settings overridable by options named
# scythe.<source>.<setting> (for example scythe.plos.max_wait).
source_limiter <- function(source, label, state_file, checkpoint_dir, min_interval,
                           windows = NULL) {
  opt <- function(name, default) getOption(paste0("scythe.", source, ".", name), default)
  if (is.null(state_file) && !is.null(checkpoint_dir)) {
    state_file <- file.path(checkpoint_dir, paste0(source, "-state.json"))
  }
  rate_limiter(source, label = label, state_file = state_file,
               min_interval = opt("min_interval", min_interval),
               windows = opt("windows", windows),
               max_wait = opt("max_wait", getOption("scythe.max_wait", 300)),
               backoff = opt("backoff", 900),
               max_backoff = opt("max_backoff", 86400),
               failure_breaker = opt("failure_breaker", 10))
}
