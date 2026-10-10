# Live OpenAlex tests spend credits, so they never run on CRAN or CI, and run
# locally only when a developer opts in with SCYTHE_TEST_OPENALEX=true. An API
# key alone isn't enough: without one, OpenAlex charges the per-IP budget.
skip_unless_openalex_live <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_not(
    isTRUE(as.logical(Sys.getenv("SCYTHE_TEST_OPENALEX", "false"))),
    "live OpenAlex tests spend credits; set SCYTHE_TEST_OPENALEX=true to run them"
  )
  testthat::skip_if_offline("api.openalex.org")
}

# Skip a live test whose search stopped early because the OpenAlex budget is
# rate limited, rather than failing on its partial results.
skip_if_openalex_rate_limited <- function(res) {
  status <- attr(res, "openalex_status")
  testthat::skip_if(identical(status$stopped, "rate_limited"),
                    paste("OpenAlex is rate limited until", status$paused_until %||% "later"))
}

# Read a JSON Lines review log, or an empty data.frame if nothing was logged.
read_log <- function(res, name) {
  path <- file.path(attr(res, "openalex_status")$log_dir, name)
  if (!file.exists(path)) return(data.frame())
  jsonlite::stream_in(file(path), verbose = FALSE)
}

# A fake OpenAlex API for testing the OpenAlex adapter offline.
#
# `works` is a list of work records; each may have a `text` field (the DOIs
# its full text mentions) and `referenced_works`. The fake answers the
# fulltext.search.exact, doi and cites filters the adapter uses, records every
# URL it is sent, and returns the responses queued in `responses` (one list of
# status/headers per request) before falling back to answering normally.
fake_openalex <- function(works = list(), responses = list(), headers = list()) {
  server <- new.env()
  server$works <- works
  server$responses <- responses
  server$headers <- headers
  server$urls <- character()
  server$http <- function(urls) {
    lapply(urls, function(url) {
      server$urls <- c(server$urls, url)
      if (length(server$responses)) {
        res <- server$responses[[1]]
        server$responses <- server$responses[-1]
        if (!is.null(res$status) && res$status != 200) {
          return(list(status = res$status, headers = res$headers %||% list(), body = "{}"))
        }
        extra <- res$headers %||% list()
      } else {
        extra <- list()
      }
      body <- fake_openalex_answer(server$works, url)
      list(status = 200, headers = c(extra, server$headers), body = body)
    })
  }
  server
}

fake_openalex_answer <- function(works, url) {
  query <- strsplit(sub("^[^?]*\\?", "", url), "&")[[1]]
  params <- stats::setNames(
    lapply(query, function(q) utils::URLdecode(sub("^[^=]*=", "", q))),
    sub("=.*$", "", query)
  )
  filter <- params$filter
  value <- sub("^[^:]*:", "", sub(",from_publication_date:.*$", "", filter))
  terms <- strsplit(value, "|", fixed = TRUE)[[1]]
  if (startsWith(filter, "fulltext.search.exact:")) {
    terms <- gsub("\"", "", terms)
    hits <- Filter(function(w) any(terms %in% w$text), works)
  } else if (startsWith(filter, "doi:")) {
    hits <- Filter(function(w) sub("^https://doi.org/", "", w$doi %||% "") %in% terms, works)
  } else if (startsWith(filter, "cites:")) {
    hits <- Filter(function(w) {
      any(paste0("https://openalex.org/", terms) %in% unlist(w$referenced_works))
    }, works)
  } else {
    stop("fake OpenAlex does not support filter ", filter)
  }
  per_page <- as.integer(params[["per-page"]] %||% 25)
  start <- if (identical(params$cursor, "*") || is.null(params$cursor)) 0 else as.integer(params$cursor)
  page <- utils::head(utils::tail(hits, max(0, length(hits) - start)), per_page)
  next_cursor <- if (!is.null(params$cursor) && start + per_page < length(hits)) {
    as.character(start + per_page)
  }
  page <- lapply(page, function(w) { w$text <- NULL; w })
  jsonlite::toJSON(list(meta = list(count = length(hits), next_cursor = next_cursor),
                        results = page),
                   auto_unbox = TRUE, null = "null")
}

fake_work <- function(id, doi = NULL, type = "article", text = character(),
                      cites = character(), title = paste("Work", id)) {
  list(id = paste0("https://openalex.org/", id),
       doi = if (!is.null(doi)) paste0("https://doi.org/", doi),
       title = title, type = type, publication_year = 2020,
       primary_location = list(landing_page_url = paste0("https://example.org/", id),
                               source = list(display_name = "Journal")),
       referenced_works = as.list(paste0("https://openalex.org/", cites)),
       text = as.list(text))
}

# Run `code` against a fake OpenAlex server with a fake clock, without any
# API key and with logs in a temporary directory.
with_fake_openalex <- function(server, code, env = parent.frame()) {
  clock <- new.env()
  clock$now <- as.POSIXct("2026-10-01 12:00:00", tz = "UTC")
  clock$sleeps <- numeric()
  testthat::local_mocked_bindings(
    openalex_http = server$http,
    openalex_now = function() clock$now,
    openalex_sleep = function(s) {
      clock$sleeps <- c(clock$sleeps, s)
      clock$now <- clock$now + s
    },
    scythe_get_key = function(source) NA,
    datacite_related = function(doi) NULL,
    .package = "scythe",
    .env = env
  )
  withr::local_options(scythe.log_dir = withr::local_tempdir(.local_envir = env), .local_envir = env)
  server$clock <- clock
  suppressMessages(force(code))
}
