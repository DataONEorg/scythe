# Submitting citations to DataONE's metrics service (DataONEorg/scythe#49).
#
# The service's POST /citations endpoint (metrics-service c16bf23) shapes how
# citations are sent:
#
# - A request with more than one citation fails with HTTP 500, and only the
#   first of a citation's related_identifiers is recorded, so each pair is
#   sent in a request of its own. citation_request() still builds the arrays,
#   ready for when the service accepts batches.
# - HTTP 202 means accepted, not stored: the service answers 202 even when its
#   insert fails. The ledger records what was sent, and a pair is only counted
#   as done once the metrics comparison reads it back (see scheduled_search()).
# - The service doesn't check for duplicates, and its primary key includes
#   link_publication_date, which defaults to the day it receives the request.
#   A fixed date (when scythe first found the pair) means a pair sent again is
#   rejected by the primary key instead of being stored twice.
# - The service looks up the citing work's metadata itself, so only the pair,
#   its relation type and the date are sent.

# The service's production address; see metrics_url_default in metrics.R for
# why it isn't metrics.dataone.org yet.
citations_url_default <- "https://logproc-stage-ucsb-1.test.dataone.org/citations"

default_citations_url <- function() getOption("scythe.citations_url", citations_url_default)

relation_types <- c("isCitedBy", "isReferencedBy")

ledger_columns <- c("source_id", "target_id", "relation_type", "link_publication_date", "status",
                    "http_status", "message", "attempts", "first_attempt", "last_attempt",
                    "confirmed_at")

#' Submit citations to DataONE's metrics service
#'
#' Sends each citation (a citing work and the DataONE identifier it cites) to
#' the metrics service, one request per citation, and reports how the
#' service answered.
#'
#' The service's address is the `scythe.citations_url` option, which
#' defaults to DataONE's production metrics service. To submit to a
#' development service, set `options(scythe.citations_url = ...)`, and set
#' `scythe.metrics_url` to the same service so that submissions can be
#' confirmed with [get_metrics_citations()]; a warning is given when the two
#' options name different hosts.
#'
#' The DataONE authentication token (a JWT) is read with
#' [scythe_get_key()]: from the `dataone_token` environment variable, or else
#' the `dataone_token` key in the scythe keyring, set with
#' `scythe_set_key("dataone_token", ...)`. It can't be passed as an argument,
#' so that it never appears in code or a command history. Before anything is
#' sent, the call stops with a `scythe_auth_error` if no token can be found,
#' or if the token has expired. A dry run needs no token.
#'
#' An accepted citation (`registered` or `queued`) isn't necessarily stored:
#' the service accepts some citations it then fails to store. Confirm them
#' later with [get_metrics_citations()].
#'
#' Requests are sent one after another with no delay. After 5 failures in a
#' row (the option `scythe.metrics_submit.failure_breaker`), or when the
#' service rate-limits the requests for longer than `scythe.max_wait`
#' seconds, the call stops sending, and the remaining citations are reported
#' as `not_sent`.
#'
#' @param citations a data frame, or the path of a CSV file such as the
#'   scheduled search's `results/to-submit.csv`, with the columns
#'   `source_id` (or `article_id`), the citing work's DOI in any form, and
#'   `target_id`, the dataset's exact DataONE identifier (PID or SID).
#'   Optional columns: `relation_type`, overriding the argument for that row;
#'   and `link_publication_date` (`YYYY-MM-DD`), which defaults to the date
#'   of `first_found` if there is one, or else today.
#' @param relation_type the relation type for rows without their own:
#'   "isCitedBy" (the work cites the dataset) or "isReferencedBy" (the work
#'   uses the dataset, but only refers to it indirectly)
#' @param submitter the DataONE subject submitting the citations; by default
#'   the token's subject (its `sub` claim)
#' @param dry_run if TRUE, build the requests without sending them; they're
#'   returned in the result's `requests` attribute
#' @param max the most requests to send
#' @param ledger the path of a CSV file recording every citation submitted
#'   and how the service answered. Citations already in it are skipped,
#'   unless their submission failed. NULL for none.
#' @param log_dir a directory to which each request and the service's answer
#'   are appended, as JSON Lines files named by date; NULL for none
#' @return `citations`, with these columns added: `submit_status`
#'   ("registered", "queued", "rejected", "failed", "skipped", "not_sent" or
#'   "dry_run"), `http_status`, `message` (the service's message, or why the
#'   citation was skipped) and `submitted_at`. Its `submit_status` attribute
#'   summarizes the counts, and whether and why sending stopped early.
#' @export
#' @examples
#' \dontrun{
#' scythe_set_key("dataone_token", "eyJ...")
#' submit_citations("results/to-submit.csv", dry_run = TRUE)
#' submit_citations(data.frame(source_id = "10.1016/j.marenvres.2009.12.006",
#'                             target_id = "doi:10.5063/F1JS9NCJ"))
#' }
submit_citations <- function(citations, relation_type = "isCitedBy", submitter = NULL,
                             dry_run = FALSE, max = Inf, ledger = NULL, log_dir = NULL) {
  citations <- read_submission_input(citations)
  if (!(is.character(relation_type) && length(relation_type) == 1 && relation_type %in% relation_types)) {
    stop("relation_type must be one of ", paste0("\"", relation_types, "\"", collapse = ", "),
         call. = FALSE)
  }
  rows <- prepare_submissions(citations, relation_type)
  now <- http_now()

  # Skip what can't or needn't be sent.
  status <- rep(NA_character_, nrow(rows))
  msg <- rep(NA_character_, nrow(rows))
  skip <- function(which, why) {
    which <- which & is.na(status)
    status[which] <<- "skipped"
    msg[which] <<- why
  }
  skip(is.na(rows$target_id), "no DataONE identifier for the cited dataset")
  skip(is.na(rows$source_key), "the citing work's identifier isn't a DOI")
  skip(is.na(rows$link_publication_date), "link_publication_date isn't a date (YYYY-MM-DD)")
  known <- read_ledger(ledger)
  in_ledger <- match(rows$key, ledger_key(known))
  done <- !is.na(in_ledger) & known$status[in_ledger] != "failed"
  for (i in which(done & is.na(status))) {
    status[i] <- "skipped"
    msg[i] <- paste0("already submitted (", known$status[in_ledger[i]], ")")
  }
  skip(duplicated(rows$key), "the same citation as an earlier row")
  todo <- which(is.na(status))

  url <- default_citations_url()
  warn_if_hosts_differ(url, default_metrics_url())
  token <- scythe_get_key("dataone_token")
  claims <- token_claims(token)
  submitter <- submitter %||% claims$sub
  if (length(todo) && !dry_run) {
    if (!is.character(token) || length(token) != 1 || is.na(token) || !nzchar(token)) {
      scythe_stop("scythe_auth_error", paste0(
        "No DataONE token was found, so citations can't be submitted. Set the dataone_token ",
        "environment variable, or store it with scythe_set_key(\"dataone_token\", ...)."))
    }
    expires <- token_expires(claims)
    if (!is.null(expires) && expires <= now) {
      scythe_stop("scythe_auth_error", paste0("The DataONE token expired at ", format_utc(expires),
                                              "; set a new one with scythe_set_key(\"dataone_token\", ...)."))
    }
    if (is.null(submitter) || is.na(submitter)) {
      stop("The submitter isn't known: the token has no subject (sub claim); pass `submitter`.",
           call. = FALSE)
    }
  }
  submitter <- submitter %||% NA_character_
  headers <- if (!dry_run) list(Authorization = paste("Bearer", token))

  limiter <- rate_limiter("metrics_submit", label = "The metrics service", min_interval = 0,
                          adaptive = FALSE, max_wait = getOption("scythe.max_wait", 300),
                          failure_breaker = getOption("scythe.metrics_submit.failure_breaker", 5))
  http_status <- rep(NA_integer_, nrow(rows))
  submitted_at <- rep(NA_character_, nrow(rows))
  requests <- character()
  log <- list()
  sent <- 0
  stopped <- NULL
  for (i in todo) {
    body <- request_json(citation_request(rows[i, , drop = FALSE], submitter))
    if (dry_run) {
      status[i] <- "dry_run"
      requests <- c(requests, body)
      log[[length(log) + 1]] <- list(time = format_utc(http_now()), dry_run = TRUE,
                                     request = jsonlite::fromJSON(body, simplifyVector = FALSE))
      next
    }
    if (sent >= max) {
      stopped <- "max"
      break
    }
    if (!limiter_ready(limiter)) {
      stopped <- limiter$stopped
      break
    }
    limiter_record_requests(limiter)
    res <- http_post(url, body, headers)
    sent <- sent + 1
    kind <- classify_submission(res)
    status[i] <- switch(kind, auth = , rate_limited = "failed", kind)
    http_status[i] <- as.integer(res$status)
    msg[i] <- response_message(res) %||% if (kind %in% c("failed", "auth", "rate_limited")) {
      describe_response(res)
    } else {
      NA_character_
    }
    submitted_at[i] <- format_utc(http_now())
    log[[length(log) + 1]] <- list(time = submitted_at[i],
                                   request = jsonlite::fromJSON(body, simplifyVector = FALSE),
                                   http_status = res$status, submit_status = status[i],
                                   response = res$body %||% res$error)
    known <- ledger_update(known, rows[i, , drop = FALSE], status[i], http_status[i], msg[i],
                           submitted_at[i])
    switch(kind,
      registered = , queued = , rejected = limiter_success(limiter),
      failed = limiter_failure(limiter),
      rate_limited = limiter_rate_limited(limiter, header_number(res$headers, "retry-after"),
                                          paste0("rate limited (", describe_response(res), ")")),
      auth = {
        limiter$stopped <- "auth"
        scythe_warning("scythe_auth_error", service = "metrics_submit", paste0(
          "The metrics service rejected the DataONE token (", describe_response(res), "). Check ",
          "the token set with scythe_set_key(\"dataone_token\", ...); no more citations will be ",
          "sent in this call."))
      }
    )
    if (identical(limiter$stopped, "auth")) {
      stopped <- "auth"
      break
    }
  }
  left <- which(is.na(status))
  status[left] <- "not_sent"
  msg[left] <- switch(stopped %||% "none",
    max = paste0("not sent: the limit of ", max, " requests was reached"),
    auth = "not sent: the service rejected the token",
    "not sent: the service was rate limited or failing; try again later")

  if (!is.null(ledger) && !dry_run && sent > 0) write_ledger(known, ledger)
  if (!is.null(log_dir) && length(log)) append_submission_log(log, log_dir, now, dry_run)

  citations$submit_status <- status
  citations$http_status <- http_status
  citations$message <- msg
  citations$submitted_at <- submitted_at
  counts <- table(factor(status, levels = c("registered", "queued", "rejected", "failed",
                                            "skipped", "not_sent", "dry_run")))
  attr(citations, "submit_status") <- c(
    as.list(stats::setNames(as.integer(counts), names(counts))),
    list(requests = sent, stopped = stopped, url = url,
         paused_until = if (!is.null(limiter$paused_until)) format_utc(limiter$paused_until))
  )
  if (dry_run) attr(citations, "requests") <- requests
  if (counts[["failed"]] > 0) {
    first <- which(status == "failed")[1]
    scythe_warning("scythe_request_failed", service = "metrics_submit", paste0(
      counts[["failed"]], " of ", sent, " citations couldn't be submitted (first: ", msg[first],
      "). They can be sent again later."))
  }
  message(if (dry_run) {
    paste0("Dry run: built ", counts[["dry_run"]], " requests for ", url, " without sending them")
  } else {
    paste0("Submitted ", sent, " citations to ", url, ": ", counts[["registered"]], " registered, ",
           counts[["queued"]], " queued, ", counts[["rejected"]], " rejected, ",
           counts[["failed"]], " failed")
  }, if (counts[["skipped"]] > 0) paste0("; ", counts[["skipped"]], " skipped"),
  if (counts[["not_sent"]] > 0) paste0("; ", counts[["not_sent"]], " not sent"), ".")
  citations
}

# The citations to submit, from a data frame or a CSV file.
read_submission_input <- function(citations) {
  if (is.character(citations) && length(citations) == 1) {
    if (!file.exists(citations)) stop("There is no file ", citations, call. = FALSE)
    citations <- utils::read.csv(citations, colClasses = "character", na.strings = "")
  }
  if (!is.data.frame(citations)) {
    stop("citations must be a data frame or the path of a CSV file", call. = FALSE)
  }
  if (!any(c("source_id", "article_id") %in% names(citations)) ||
      !"target_id" %in% names(citations)) {
    stop("citations must have a source_id (or article_id) column and a target_id column",
         call. = FALSE)
  }
  citations
}

# One row per citation with what is sent: `source_key`, the citing work's DOI
# (lowercase, without a prefix, NA if it isn't a DOI); `target_id`;
# `relation_type`; `link_publication_date`; and `key`, the pair as compared
# with the ledger and the metrics service.
prepare_submissions <- function(citations, relation_type) {
  blank <- function(x) is.na(x) | !nzchar(trimws(x))
  col <- function(name) if (name %in% names(citations)) as.character(citations[[name]])
  source <- col("source_id") %||% col("article_id")
  target <- col("target_id")
  target[blank(target)] <- NA_character_
  relation <- col("relation_type") %||% rep(NA_character_, nrow(citations))
  relation[blank(relation)] <- relation_type
  bad <- unique(relation[!relation %in% relation_types])
  if (length(bad)) {
    stop("Unknown relation_type ", paste0("\"", bad, "\"", collapse = ", "), "; use ",
         paste0("\"", relation_types, "\"", collapse = " or "), ".", call. = FALSE)
  }
  date <- col("link_publication_date") %||% rep(NA_character_, nrow(citations))
  first_found <- col("first_found")
  if (!is.null(first_found)) first_found <- substr(first_found, 1, 10)
  if (!is.null(first_found)) date[blank(date)] <- first_found[blank(date)]
  date[blank(date)] <- format(as.Date(http_now(), tz = "UTC"))
  date[!grepl("^\\d{4}-\\d{2}-\\d{2}$", date) | is.na(as.Date(date, optional = TRUE))] <- NA_character_
  source_key <- normalize_doi(source)
  data.frame(source_key = source_key, target_id = target, relation_type = relation,
             link_publication_date = date,
             key = paste(source_key, identifier_key(target)), stringsAsFactors = FALSE)
}

#' The request body for one or more citations
#'
#' Builds the request in the format the service documents: one entry in
#' `citations` per row, each with its target in `related_identifiers`. Rows
#' are sent one at a time for now, because the service rejects requests with
#' more than one citation and records only the first related identifier.
#' @noRd
citation_request <- function(rows, submitter) {
  list(
    request_type = "dataset",
    submitter = submitter,
    citations = lapply(seq_len(nrow(rows)), function(i) {
      list(source_id = rows$source_key[i],
           related_identifiers = list(list(identifier = rows$target_id[i],
                                           relation_type = rows$relation_type[i])),
           link_publication_date = rows$link_publication_date[i])
    })
  )
}

request_json <- function(request) {
  as.character(jsonlite::toJSON(request, auto_unbox = TRUE, null = "null", na = "null"))
}

# How the service answered a submission: "registered" or "queued" (accepted),
# "rejected" (the service refused the citation, so sending it again won't
# help), "failed" (a server or network error, worth trying again later),
# "rate_limited" or "auth" (the token was rejected).
classify_submission <- function(res) {
  code <- res$status
  if (is.na(code)) return("failed")
  if (code >= 200 && code < 300) {
    return(if (identical(response_message(res), "Successful")) "queued" else "registered")
  }
  if (code %in% c(401, 403)) return("auth")
  if (code %in% c(429, 503)) return("rate_limited")
  # The service reports invalid citations as HTTP 500 with a JSON message;
  # any other server error is treated as temporary.
  if (code == 500 && !is.null(response_message(res))) return("rejected")
  if (code >= 500) return("failed")
  "rejected"
}

# The claims in a JWT's payload, or NULL if `token` isn't a readable JWT. The
# signature isn't checked: the service does that.
token_claims <- function(token) {
  if (!is.character(token) || length(token) != 1 || is.na(token) || !nzchar(token)) return(NULL)
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) < 2) return(NULL)
  claims <- tryCatch(jsonlite::fromJSON(rawToChar(jsonlite::base64url_dec(parts[2]))),
                     error = function(e) NULL)
  if (is.list(claims)) claims else NULL
}

# When a token expires (its exp claim), or NULL if it doesn't say.
token_expires <- function(claims) {
  exp <- suppressWarnings(as.numeric(claims$exp %||% NA))
  if (is.na(exp)) return(NULL)
  as.POSIXct(exp, origin = "1970-01-01", tz = "UTC")
}

# The host name in a URL, in lower case.
url_host <- function(url) tolower(sub("^[A-Za-z][A-Za-z0-9+.-]*://([^/:?#]+).*$", "\\1", url))

warn_if_hosts_differ <- function(citations_url, metrics_url) {
  if (!identical(url_host(citations_url), url_host(metrics_url))) {
    warning("Citations are submitted to ", url_host(citations_url), " (scythe.citations_url), but ",
            "compared with ", url_host(metrics_url), " (scythe.metrics_url), so submissions can't ",
            "be confirmed. Set both options to the same service.", call. = FALSE)
  }
}

# Signal an error of a scythe condition class, such as scythe_auth_error.
scythe_stop <- function(class, message) {
  stop(structure(class = c(class, "scythe_error", "error", "condition"),
                 list(message = message, call = NULL)))
}

# The ledger: one row per citation submitted, with the service's last answer.
read_ledger <- function(path) {
  empty <- as.data.frame(stats::setNames(rep(list(character()), length(ledger_columns)),
                                         ledger_columns), stringsAsFactors = FALSE)
  if (is.null(path) || !file.exists(path)) return(empty)
  known <- utils::read.csv(path, colClasses = "character", na.strings = "")
  for (col in setdiff(ledger_columns, names(known))) known[[col]] <- rep(NA_character_, nrow(known))
  known
}

write_ledger <- function(known, path) write_csv_atomic(known, path)

ledger_key <- function(known) paste(normalize_doi(known$source_id), identifier_key(known$target_id))

# Record one submission in the ledger, adding the pair or updating its row.
ledger_update <- function(known, row, status, http_status, message, time) {
  i <- match(row$key, ledger_key(known))
  if (is.na(i)) {
    entry <- as.data.frame(stats::setNames(rep(list(NA_character_), length(ledger_columns)),
                                           ledger_columns), stringsAsFactors = FALSE)
    entry$source_id <- row$source_key
    entry$target_id <- row$target_id
    entry$first_attempt <- time
    entry$attempts <- "0"
    known <- rbind(known[ledger_columns], entry)
    i <- nrow(known)
  }
  known$relation_type[i] <- row$relation_type
  known$link_publication_date[i] <- row$link_publication_date
  known$status[i] <- status
  known$http_status[i] <- as.character(http_status)
  known$message[i] <- message
  attempts <- suppressWarnings(as.integer(known$attempts[i]))
  known$attempts[i] <- as.character(if (is.na(attempts)) 1L else attempts + 1L)
  known$last_attempt[i] <- time
  known
}

append_submission_log <- function(log, log_dir, now, dry_run) {
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  name <- paste0(if (dry_run) "dry-run-", format(as.Date(now, tz = "UTC")), ".jsonl")
  lines <- vapply(log, function(entry) {
    as.character(jsonlite::toJSON(entry, auto_unbox = TRUE, null = "null", na = "null"))
  }, "")
  con <- file(file.path(log_dir, name), open = "a", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(lines, con)
}
