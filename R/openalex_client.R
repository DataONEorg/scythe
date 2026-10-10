# HTTP client for the OpenAlex API (https://openalex.org).
#
# OpenAlex charges credits per request (1 for a list or filter query, 10 for a
# search query) against a daily budget, and returns the remaining budget in
# X-RateLimit-* headers. The client sends requests concurrently and stops
# before it would spend more than `max_credits`. Its circuit breaker and shared
# state file come from the shared rate limiter in http_client.R: once OpenAlex
# rate-limits it, no further requests are sent until the limit resets, and
# pauses longer than `max_wait` are recorded so that other processes using the
# same key (later cron runs, other shards) also hold off until the reset.

openalex_api <- "https://api.openalex.org"

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

#' Create an OpenAlex client
#'
#' The client is a rate limiter (see rate_limiter()) with OpenAlex's credit
#' accounting added.
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
  # OpenAlex reports its budget in headers, so requests aren't spaced out;
  # the client stops at the reserve, and on a 429 waits for the reset.
  client <- rate_limiter("openalex", label = "OpenAlex", state_file = state_file,
                         adaptive = FALSE, max_wait = max_wait, backoff = 60,
                         max_backoff = Inf, failure_breaker = Inf,
                         concurrency = concurrency)
  client$key <- scythe_get_key("openalex")
  client$mailto <- scythe_get_key("openalex_mailto")
  client$max_credits <- max_credits
  client$reserve <- reserve
  client$credits_used <- 0
  client$remaining <- NA_real_
  client$limit <- NA_real_
  client$reset <- NA_real_
  client
}

# Record the rate-limit headers from a response, and open the breaker when the
# daily budget has run out.
openalex_note_headers <- function(client, headers) {
  num <- function(name) header_number(headers, name)
  if (!is.na(num("x-ratelimit-limit"))) client$limit <- num("x-ratelimit-limit")
  if (!is.na(num("x-ratelimit-reset"))) client$reset <- num("x-ratelimit-reset")
  if (!is.na(num("x-ratelimit-remaining"))) {
    client$remaining <- num("x-ratelimit-remaining")
    limiter_note_quota(client, client$remaining, client$reset, "daily budget used up")
  }
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
    if (length(todo) == 0 || !limiter_ready(client)) break

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
        limiter_pause(client, client$reset, "daily budget is down to its reserve")
        next
      }
      wave <- wave[affordable]
    }

    urls <- vapply(wave, function(i) {
      q <- queries[[i]]
      if (paginate[i]) q$cursor <- cursor[i]
      openalex_url(client, path, q)
    }, "")
    limiter_record_requests(client, length(wave))
    responses <- http_get(urls)

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
        limiter_success(client)
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
        wait <- header_number(res$headers, "retry-after")
        if (is.na(wait)) wait <- header_number(res$headers, "x-ratelimit-reset")
        limiter_rate_limited(client, wait, "rate limited (HTTP 429)")
      } else if (is.na(code) || code >= 500) {
        attempts[i] <- attempts[i] + 1L
        errors[i] <- res$error %||% paste("HTTP", code)
        if (attempts[i] >= 4) {
          status[i] <- "failed"
        } else {
          wait <- header_number(res$headers, "retry-after")
          http_sleep(if (is.na(wait)) min(60, 2^attempts[i]) else wait)
        }
      } else {
        status[i] <- "failed"
        msg <- tryCatch(jsonlite::fromJSON(res$body)$message, error = function(e) NULL)
        errors[i] <- paste0("HTTP ", code, if (!is.null(msg)) paste0(": ", msg))
      }
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
