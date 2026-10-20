# Helpers for the tests of submitting citations to the metrics service.

# An unsigned JWT with the given claims, like a DataONE token. The fake
# clock in with_fake_http() starts at 2026-10-01 12:00 UTC, so the default
# expiry is in the future and `expired_token()` is in the past.
test_token <- function(sub = "http://orcid.org/0000-0000-0000-0001",
                       exp = as.POSIXct("2026-12-01", tz = "UTC")) {
  part <- function(x) jsonlite::base64url_enc(as.character(jsonlite::toJSON(x, auto_unbox = TRUE)))
  claims <- Filter(Negate(is.null), list(sub = sub, exp = if (!is.null(exp)) as.numeric(exp)))
  paste(part(list(alg = "none", typ = "JWT")), part(claims), "signature", sep = ".")
}

expired_token <- function() test_token(exp = as.POSIXct("2026-09-01", tz = "UTC"))

# A fake metrics service that answers the n-th POST with `responses[[n]]`
# (recycling the last), or "Registered" if none are given.
fake_citations_service <- function(responses = list(respond(202, list(message = "Registered",
                                                                        status_code = "202")))) {
  fake_http(post = function(url, body, headers, n) responses[[min(n, length(responses))]])
}

# The body of each POST the fake received, parsed.
posted_bodies <- function(server) {
  lapply(server$posts(), function(r) jsonlite::fromJSON(r$body, simplifyVector = FALSE))
}

# Run `code`, returning its value and the classes of the warnings it gave.
collect_warnings <- function(code) {
  classes <- list()
  value <- withCallingHandlers(code, warning = function(w) {
    classes[[length(classes) + 1]] <<- class(w)
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = vapply(classes, `[[`, "", 1))
}

submission_pairs <- function(...) {
  data.frame(source_id = c("doi:10.1016/J.MARENVRES.2009.12.006", "https://doi.org/10.5194/ESSD-14-955-2022"),
             target_id = c("doi:10.5063/F1JS9NCJ", "ess-dive-abc-123"),
             first_found = c("2026-09-15T03:00:00Z", "2026-09-20T03:00:00Z"),
             stringsAsFactors = FALSE, ...)
}

# Integration tests against a development metrics service. They post real
# citations, so they never run on CRAN or CI, and run locally only when a
# developer names a development service in SCYTHE_TEST_CITATIONS_URL (its
# /citations endpoint). Citations are read back from SCYTHE_TEST_METRICS_URL,
# which defaults to the same service's /metrics endpoint. They refuse to post
# to DataONE's production service.
production_metrics_hosts <- function() {
  unique(c(url_host(citations_url_default), url_host(metrics_url_default), "metrics.dataone.org"))
}

check_test_citations_url <- function(url) {
  if (url_host(url) %in% production_metrics_hosts()) {
    stop("SCYTHE_TEST_CITATIONS_URL (", url, ") names DataONE's production metrics service. ",
         "The integration tests post citations, so they only run against a development service.",
         call. = FALSE)
  }
  url
}

skip_unless_citations_server <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  url <- Sys.getenv("SCYTHE_TEST_CITATIONS_URL", "")
  testthat::skip_if(!nzchar(url), paste(
    "the integration tests need a development metrics service; set SCYTHE_TEST_CITATIONS_URL",
    "to its /citations endpoint"))
  check_test_citations_url(url)
  testthat::skip_if_offline(url_host(url))
  url
}

# Point scythe at the development service for the rest of the calling test,
# and return the DataONE token that submit_citations() will find (from the
# dataone_token environment variable or keyring), for the tests that build
# their own requests.
local_test_metrics_service <- function(env = parent.frame()) {
  url <- skip_unless_citations_server()
  metrics <- Sys.getenv("SCYTHE_TEST_METRICS_URL", sub("/citations/?$", "/metrics", url))
  check_test_citations_url(metrics)
  withr::local_options(scythe.citations_url = url, scythe.metrics_url = metrics, .local_envir = env)
  token <- scythe_get_key("dataone_token")
  testthat::skip_if(is.na(token), "no DataONE token set (scythe_set_key(\"dataone_token\", ...))")
  token
}

# The citations the development service has recorded for a dataset, read
# from its /metrics endpoint's dataset filter (which reads the citations
# table directly, unlike the repository filter), as `source_id` and
# `target_id`.
read_back_citations <- function(target) {
  query <- list(
    metricsPage = list(total = 0, start = 0, count = 0),
    metrics = list("citations"),
    filterBy = list(
      list(filterType = "dataset", values = list(target), interpretAs = "list"),
      list(filterType = "month", values = list("01/01/2000", format(Sys.Date(), "%m/%d/%Y")),
           interpretAs = "range")
    ),
    groupBy = list("months")
  )
  body <- metrics_request(default_metrics_url(), jsonlite::toJSON(query, auto_unbox = TRUE))
  rows <- lapply(body$resultDetails$citations, function(c) {
    data.frame(source_id = as.character(c$source_id %||% NA)[1],
               target_id = as.character(c$target_id %||% NA)[1], stringsAsFactors = FALSE)
  })
  do.call(rbind, c(list(data.frame(source_id = character(), target_id = character())), rows))
}

# How many times the service has recorded the pair.
times_recorded <- function(source, target) {
  recorded <- read_back_citations(target)
  sum(identifier_key(recorded$source_id) == identifier_key(source) &
        identifier_key(recorded$target_id) == identifier_key(target))
}

# A target identifier that can't exist in DataONE, unique to this run.
synthetic_target <- function(label) {
  paste0("urn:uuid:scythe-test-", label, "-", format(Sys.time(), "%Y%m%d%H%M%S"), "-",
         paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
}
