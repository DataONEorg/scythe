# Shared fixtures for the scheduled search's tests: fixed DOI lists in place
# of the DataONE indexes, a fake OpenAlex citation graph, and fakes for the
# full-text sources.

doi_rows <- function(dois, node, group) {
  data.frame(doi = dois, identifier = paste0("doi:", dois), field = "id", node = node,
             dataset = paste0("doi:", dois), format_type = "METADATA",
             date_uploaded = "2020-01-01T00:00:00Z", index = "member", group = group,
             stringsAsFactors = FALSE)
}
priority_dois <- rbind(doi_rows("10.18739/p1", "urn:node:ARCTIC", "priority"),
                       doi_rows("10.5063/p2", "urn:node:KNB", "priority"))
other_dois <- rbind(doi_rows("10.1594/pangaea.1", "urn:node:PANGAEA", "other"),
                    doi_rows("10.6085/o1", "urn:node:PISCO", "other"))

# A1 cites p1 (graph only), A2 mentions p2 (text only), A3 cites and
# mentions p1 (both), B1 and B2 cite the other nodes' datasets.
works <- list(
  fake_work("D1", "10.18739/p1", type = "dataset"),
  fake_work("D2", "10.5063/p2", type = "dataset"),
  fake_work("O1", "10.6085/o1", type = "dataset"),
  fake_work("P1", "10.1594/pangaea.1", type = "dataset"),
  fake_work("A1", "10.1/a1", cites = "D1"),
  fake_work("A2", "10.1/a2", text = "10.5063/p2"),
  fake_work("A3", "10.1/a3", cites = "D1", text = "10.18739/p1"),
  fake_work("B1", "10.1/b1", cites = "O1"),
  fake_work("B2", "10.1/b2", cites = "P1")
)

# Run `code` with collect_dataone_dois() returning the fixed DOI lists, and
# count its calls in `collected`.
with_fake_dataone <- function(code, env = parent.frame()) {
  collected <- new.env()
  collected$priority <- 0
  collected$others <- 0
  assign("collected", collected, envir = env)
  testthat::local_mocked_bindings(
    collect_dataone_dois = function(priority_nodes = character(), other_nodes = NULL,
                                    exclude_nodes = character(), ...) {
      if (length(priority_nodes)) {
        collected$priority <- collected$priority + 1
        return(priority_dois)
      }
      collected$others <- collected$others + 1
      other_dois[!other_dois$node %in% exclude_nodes, , drop = FALSE]
    },
    .package = "scythe", .env = env
  )
  force(code)
}


total <- function(dir) read.csv(file.path(dir, "results", "citations.csv"), colClasses = "character")

pairs <- function(path) {
  x <- read.csv(path, colClasses = "character")
  sort(paste(x$article_id, x$dataset_id))
}

# Make the fake answer the first request whose URL matches `pattern` with a
# 429 that resets after `seconds`.
rate_limit_once <- function(server, pattern, seconds = 7200) {
  http <- server$http
  limited <- FALSE
  server$http <- function(urls) {
    lapply(urls, function(url) {
      if (!limited && grepl(pattern, url, fixed = TRUE)) {
        limited <<- TRUE
        server$urls <- c(server$urls, url)
        return(list(status = 429, headers = list(`retry-after` = as.character(seconds)), body = "{}"))
      }
      http(url)[[1]]
    })
  }
}

# A fake answering OpenAlex from `works`, and each full-text source from its
# handler in `sources` (a function of the requested DOI and the request
# number for that source), defaulting to no citations.
fake_services <- function(works, sources = list()) {
  counts <- new.env()
  fake_http(function(url, headers, n) {
    if (startsWith(url, "https://api.openalex.org")) {
      return(respond(body = fake_openalex_answer(works, url)))
    }
    source <- if (grepl("xdd.wisc.edu", url, fixed = TRUE)) "xdd"
      else if (grepl("springernature", url, fixed = TRUE)) "springer"
      else if (grepl("elsevier", url, fixed = TRUE)) "scopus"
      else if (grepl("api.plos.org", url, fixed = TRUE)) "plos"
      else stop("No fake for ", url)
    counts[[source]] <- (counts[[source]] %||% 0) + 1
    handler <- sources[[source]] %||% function(doi, k) found_by(source, character())
    handler(requested_id(url), counts[[source]])
  })
}

# A response from `source` listing `articles` as citing works.
found_by <- function(source, articles) {
  switch(source,
    xdd = respond(body = list(success = list(data = lapply(articles, function(a) list(doi = a, title = a))))),
    springer = if (length(articles)) {
      respond(body = list(records = lapply(articles, function(a) list(identifier = paste0("doi:", a), title = a))))
    } else {
      respond(404, body = list(message = "No data was found for the given query."))
    },
    scopus = respond(body = list(`search-results` = list(entry = if (length(articles)) {
      lapply(articles, function(a) list(`prism:doi` = a, `dc:title` = a))
    } else list(list(error = "Result set was empty"))))),
    plos = respond(body = list(response = list(numFound = length(articles),
                                               docs = lapply(articles, function(a) list(id = a, title = a)))))
  )
}

# The DOIs a fake service was asked about, for one source.
requested <- function(server, pattern) {
  urls <- server$urls()
  requested_id(urls[grepl(pattern, urls, fixed = TRUE)])
}

# Make a fake_http() service answer the first request whose URL matches
# `pattern` with a 429 that resets after `seconds`.
rate_limit_first <- function(server, pattern, seconds = 7200) {
  handler <- server$handler
  limited <- FALSE
  server$handler <- function(url, headers, n) {
    if (!limited && grepl(pattern, url, fixed = TRUE)) {
      limited <<- TRUE
      return(respond(429, headers = list(`retry-after` = as.character(seconds))))
    }
    handler(url, headers, n)
  }
}
