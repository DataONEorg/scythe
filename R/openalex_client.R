# HTTP client for the OpenAlex API (https://openalex.org).
#
# OpenAlex charges credits per request (1 for a list or filter query, 10 for a
# search query) against a daily budget, and returns the remaining budget in
# X-RateLimit-* headers. The client sends requests concurrently, stops before
# it would spend more than `max_credits`, and keeps a circuit breaker so that
# once OpenAlex rate-limits it, no further requests are sent until the limit
# resets. Pauses longer than `max_wait` are written to a shared state file so
# that other processes using the same key (later cron runs, other shards) also
# hold off until the reset.

openalex_api <- "https://api.openalex.org"

# Wrappers around the clock, sleeping and the network so tests can replace them.
openalex_now <- function() Sys.time()
openalex_sleep <- function(seconds) Sys.sleep(seconds)

# Send GET requests for `urls` concurrently and return, for each, a list with
# the HTTP `status` (NA on a network error), the response `headers` as a named
# list with lowercase names, and the `body` as text.
openalex_http <- function(urls) {
  pool <- curl::new_pool(total_con = length(urls), host_con = length(urls))
  out <- vector("list", length(urls))
  for (i in seq_along(urls)) {
    local({
      j <- i
      handle <- curl::new_handle(
        url = urls[j],
        useragent = "scythe (https://github.com/DataONEorg/scythe)",
        timeout = 120
      )
      curl::multi_add(
        handle,
        pool = pool,
        done = function(res) {
          out[[j]] <<- list(
            status = res$status_code,
            headers = curl::parse_headers_list(res$headers),
            body = rawToChar(res$content)
          )
        },
        fail = function(msg) {
          out[[j]] <<- list(status = NA_integer_, headers = list(), body = NULL, error = msg)
        }
      )
    })
  }
  curl::multi_run(pool = pool)
  out
}

# Credits OpenAlex charges for one request: 10 for any search, 1 otherwise.
openalex_request_cost <- function(query) {
  if ("search" %in% names(query) || grepl("\\.search", query$filter %||% "")) 10 else 1
}

openalex_url <- function(client, path, query) {
  query$include_xpac <- "true"
  if (!is.na(client$key)) query$api_key <- client$key
  if (!is.na(client$mailto)) query$mailto <- client$mailto
  values <- vapply(query, function(v) utils::URLencode(as.character(v), reserved = TRUE), "")
  paste0(openalex_api, path, "?", paste(names(query), values, sep = "=", collapse = "&"))
}

# Read and write the JSON state file shared by every process that uses the
# same OpenAlex key. Writes go through a temporary file and a rename so that a
# reader never sees a partly written file. Updates aren't locked, so a writer
# never shortens a pause another process has recorded.
read_openalex_state <- function(path) {
  if (is.null(path) || !file.exists(path)) return(list())
  tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) list())
}

update_openalex_state <- function(path, ...) {
  if (is.null(path)) return(invisible())
  state <- read_openalex_state(path)
  fields <- list(...)
  if (!is.null(fields$paused_until) && !is.null(state$paused_until) &&
      isTRUE(parse_utc(state$paused_until) > parse_utc(fields$paused_until))) {
    fields$paused_until <- state$paused_until
  }
  state[names(fields)] <- fields
  state <- Filter(Negate(is.null), state)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp", Sys.getpid())
  jsonlite::write_json(state, tmp, auto_unbox = TRUE, pretty = TRUE)
  file.rename(tmp, path)
  invisible(state)
}

format_utc <- function(time) format(time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
parse_utc <- function(text) as.POSIXct(text, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# The end of a pause recorded in the shared state file, or NULL if none is
# still in force.
shared_pause <- function(path) {
  paused_until <- read_openalex_state(path)$paused_until
  if (is.null(paused_until)) return(NULL)
  paused_until <- parse_utc(paused_until)
  if (is.na(paused_until) || paused_until <= openalex_now()) return(NULL)
  paused_until
}

#' Create an OpenAlex client
#'
#' @param max_credits the most credits this client may spend
#' @param state_file path of the shared state file recording rate-limit
#'   pauses, or NULL to keep pauses within this process only
#' @return an environment holding the client's settings and running totals
#' @noRd
openalex_client <- function(max_credits = Inf,
                            state_file = NULL,
                            concurrency = getOption("scythe.openalex.concurrency", 4),
                            max_wait = getOption("scythe.openalex.max_wait", 300),
                            reserve = getOption("scythe.openalex.reserve", 0.05)) {
  client <- new.env(parent = emptyenv())
  client$key <- scythe_get_key("openalex")
  client$mailto <- scythe_get_key("openalex_mailto")
  client$max_credits <- max_credits
  client$state_file <- state_file
  client$concurrency <- concurrency
  client$active_concurrency <- concurrency
  client$max_wait <- max_wait
  client$reserve <- reserve
  client$credits_used <- 0
  client$requests <- 0
  client$remaining <- NA_real_
  client$limit <- NA_real_
  client$reset <- NA_real_
  client$paused_until <- NULL
  client$consecutive_429 <- 0
  client$last_pause <- 0
  client$successes <- 0
  # Why the client stopped sending requests, or NULL while it is still active.
  client$stopped <- NULL

  # Honor a pause recorded by an earlier run or by another process.
  paused_until <- shared_pause(state_file)
  if (!is.null(paused_until)) {
    client$paused_until <- paused_until
    client$stopped <- "rate_limited"
    message("OpenAlex is rate limited until ", format_utc(paused_until),
            " (recorded in ", state_file, "); not sending any requests.")
  }
  client
}

# Record the rate-limit headers from a response, and open the breaker when the
# daily budget has run out.
openalex_note_headers <- function(client, headers) {
  num <- function(name) suppressWarnings(as.numeric(headers[[name]] %||% NA))
  if (!is.na(num("x-ratelimit-limit"))) client$limit <- num("x-ratelimit-limit")
  if (!is.na(num("x-ratelimit-reset"))) client$reset <- num("x-ratelimit-reset")
  if (!is.na(num("x-ratelimit-remaining"))) {
    client$remaining <- num("x-ratelimit-remaining")
    if (client$remaining <= 0) {
      openalex_pause(client, client$reset %||% NA, "daily budget used up")
    }
  }
}

# Open the circuit breaker for `seconds`. A pause longer than `max_wait` is
# written to the shared state file straight away so other processes see it.
openalex_pause <- function(client, seconds, reason) {
  if (is.na(seconds)) seconds <- 60 * 2^max(0, client$consecutive_429 - 1)
  if (client$consecutive_429 >= 3) seconds <- max(seconds, 2 * client$last_pause)
  client$last_pause <- seconds
  until <- openalex_now() + seconds
  if (is.null(client$paused_until) || until > client$paused_until) {
    client$paused_until <- until
  }
  message("OpenAlex: ", reason, "; pausing requests until ", format_utc(client$paused_until))
  if (seconds > client$max_wait) {
    update_openalex_state(client$state_file, paused_until = format_utc(client$paused_until))
  }
}

# Return TRUE when the client may send requests. A short pause is waited out
# here, after which requests resume one at a time; a long pause stops the
# client for the rest of this run. Pauses that other processes record in the
# shared state file while this client runs are honoured too.
openalex_ready <- function(client) {
  if (!is.null(client$stopped)) return(FALSE)
  shared <- shared_pause(client$state_file)
  if (!is.null(shared) && (is.null(client$paused_until) || shared > client$paused_until)) {
    client$paused_until <- shared
  }
  if (is.null(client$paused_until)) return(TRUE)
  wait <- as.numeric(difftime(client$paused_until, openalex_now(), units = "secs"))
  if (wait > client$max_wait) {
    client$stopped <- "rate_limited"
    update_openalex_state(client$state_file, paused_until = format_utc(client$paused_until))
    warning(call. = FALSE,
            "OpenAlex is rate limited until ", format_utc(client$paused_until),
            "; stopping with partial results. Run again after that time to continue.")
    return(FALSE)
  }
  if (wait > 0) openalex_sleep(wait)
  client$paused_until <- NULL
  client$active_concurrency <- 1
  client$successes <- 0
  TRUE
}

#' Run OpenAlex queries
#'
#' Sends each query in `queries` (a list of named lists of query parameters
#' for `path`), following cursor pagination for the queries where `paginate`
#' is TRUE, and collects the results.
#'
#' @return a list with `results` (the collected works for each query), `count`
#'   (`meta$count` for each query) and `status` ("done", "failed", or
#'   "pending" when the client stopped before the query finished)
#' @noRd
openalex_run <- function(client, queries, paginate = TRUE, path = "/works") {
  n <- length(queries)
  paginate <- rep_len(paginate, n)
  results <- vector("list", n)
  count <- rep(NA_real_, n)
  status <- rep("pending", n)
  cursor <- ifelse(paginate, "*", NA_character_)
  attempts <- rep(0L, n)
  errors <- rep(NA_character_, n)

  repeat {
    todo <- which(status == "pending")
    if (length(todo) == 0 || !openalex_ready(client)) break

    wave <- utils::head(todo, client$active_concurrency)
    cost <- vapply(queries[wave], openalex_request_cost, 1)
    within_cap <- client$credits_used + cumsum(cost) <= client$max_credits
    if (!within_cap[1]) {
      client$stopped <- "max_credits"
      warning(call. = FALSE, "Stopped after spending ", client$credits_used,
              " OpenAlex credits (max_credits = ", client$max_credits, ").")
      break
    }
    wave <- wave[within_cap]
    cost <- cost[within_cap]
    if (!is.na(client$remaining)) {
      reserve <- if (is.na(client$limit)) 0 else client$reserve * client$limit
      affordable <- client$remaining - cumsum(cost) >= reserve
      if (!affordable[1]) {
        openalex_pause(client, client$reset, "daily budget is down to its reserve")
        next
      }
      wave <- wave[affordable]
    }

    urls <- vapply(wave, function(i) {
      q <- queries[[i]]
      if (paginate[i]) q$cursor <- cursor[i]
      openalex_url(client, path, q)
    }, "")
    responses <- openalex_http(urls)
    client$requests <- client$requests + length(wave)

    for (k in seq_along(wave)) {
      i <- wave[k]
      res <- responses[[k]]
      openalex_note_headers(client, res$headers)
      code <- res$status
      if (!is.na(code) && code == 200) {
        # OpenAlex charges for the request whether or not its body parses.
        used <- suppressWarnings(as.numeric(res$headers[["x-ratelimit-credits-used"]] %||% NA))
        client$credits_used <- client$credits_used + if (is.na(used)) cost[k] else used
        body <- tryCatch(jsonlite::fromJSON(res$body, simplifyVector = FALSE),
                         error = function(e) NULL)
        if (is.null(body)) {
          status[i] <- "failed"
          errors[i] <- "unparseable response"
          next
        }
        client$consecutive_429 <- 0
        client$successes <- client$successes + 1
        results[[i]] <- c(results[[i]], body$results)
        count[i] <- body$meta$count %||% length(body$results)
        next_cursor <- body$meta$next_cursor
        if (paginate[i] && !is.null(next_cursor) && length(body$results) > 0) {
          cursor[i] <- next_cursor
        } else {
          status[i] <- "done"
        }
      } else if (!is.na(code) && code == 429) {
        # Not retried directly: the breaker decides when requests resume.
        client$consecutive_429 <- client$consecutive_429 + 1
        wait <- suppressWarnings(as.numeric(
          res$headers[["retry-after"]] %||% res$headers[["x-ratelimit-reset"]] %||% NA
        ))
        openalex_pause(client, wait, "rate limited (HTTP 429)")
      } else if (is.na(code) || code >= 500) {
        attempts[i] <- attempts[i] + 1L
        errors[i] <- res$error %||% paste("HTTP", code)
        if (attempts[i] >= 4) {
          status[i] <- "failed"
        } else {
          wait <- suppressWarnings(as.numeric(res$headers[["retry-after"]] %||% NA))
          openalex_sleep(if (is.na(wait)) min(60, 2^attempts[i]) else wait)
        }
      } else {
        status[i] <- "failed"
        msg <- tryCatch(jsonlite::fromJSON(res$body)$message, error = function(e) NULL)
        errors[i] <- paste0("HTTP ", code, if (!is.null(msg)) paste0(": ", msg))
      }
    }

    if (client$active_concurrency < client$concurrency && client$successes >= 10) {
      client$active_concurrency <- client$concurrency
    }
  }

  for (i in which(status == "failed")) {
    message("OpenAlex query failed: ", errors[i])
  }
  list(results = results, count = count, status = status, error = errors)
}

#' Estimate the credits a search will use
#'
#' Text screening costs 10 credits per batch; attributing hits to single DOIs
#' costs about 2 searches per level of bisection for each cited DOI (assuming
#' `hit_rate` of the DOIs are cited); the citation graph costs about 3 list
#' requests per batch (resolving DOIs, then one or more pages of citing works).
#' @noRd
estimate_openalex_cost <- function(n, methods = c("text", "graph"), batch_size = 100,
                                   hit_rate = 0.05) {
  if (n == 0) return(0)
  batches <- ceiling(n / batch_size)
  cost <- 0
  if ("text" %in% methods) {
    hits <- max(1, ceiling(hit_rate * n))
    depth <- ceiling(log2(min(batch_size, n)))
    cost <- cost + 10 * batches + 10 * 2 * hits * depth
  }
  if ("graph" %in% methods) cost <- cost + 3 * batches
  ceiling(cost)
}
