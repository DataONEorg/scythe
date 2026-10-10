# Routing of citing works into bins, classification of data-to-data links,
# and the JSONL review logs written alongside the OpenAlex adapter's results.

# DOI prefixes of DataONE member repositories. A work under one of these
# prefixes that cites a dataset is treated as a data-to-data link, since
# OpenAlex's `type` field is unreliable for repository records (EDI and
# ESS-DIVE datasets are often typed "article"). Override with
# options(scythe.repository_prefixes = ...).
repository_prefixes <- function() {
  getOption("scythe.repository_prefixes", c(
    "10.18739", # Arctic Data Center
    "10.5063",  # KNB
    "10.6073",  # Environmental Data Initiative (PASTA)
    "10.15485", # ESS-DIVE
    "10.5061"   # Dryad
  ))
}

doi_prefix <- function(doi) sub("/.*$", "", doi)

#' Sort citing works into bins
#'
#' Returns "no_doi" for works without a DOI (which `write_citation_pairs()`
#' cannot look up), "data_relation" for works that are datasets or come from
#' a data repository, and "article" for everything else.
#' @noRd
citer_bin <- function(pairs) {
  no_doi <- is.na(pairs$work_doi) | pairs$work_doi == ""
  prefix <- doi_prefix(pairs$work_doi)
  is_data <- pairs$type %in% "dataset" |
    prefix %in% repository_prefixes() |
    prefix == doi_prefix(pairs$dataset_doi)
  ifelse(no_doi, "no_doi", ifelse(is_data, "data_relation", "article"))
}

# How DataCite relationTypes map onto the kinds of data-to-data relationship
# that the review log distinguishes.
relation_classes <- c(
  IsVersionOf = "version", HasVersion = "version",
  IsNewVersionOf = "version", IsPreviousVersionOf = "version",
  Obsoletes = "version", IsObsoletedBy = "version",
  IsDerivedFrom = "derivation", IsSourceOf = "derivation",
  IsVariantFormOf = "derivation", IsOriginalFormOf = "derivation",
  HasPart = "composition", IsPartOf = "composition",
  Compiles = "composition", IsCompiledBy = "composition",
  Cites = "citation", IsCitedBy = "citation",
  References = "citation", IsReferencedBy = "citation",
  IsSupplementTo = "citation", IsSupplementedBy = "citation"
)

# When a pair has several relations, the first of these classes wins.
relation_class_order <- c("version", "derivation", "composition", "citation")

# Fetch a URL from DataCite, returning curl's response (status_code NA on a
# network error). A wrapper so that tests can replace the network.
datacite_fetch <- function(url) {
  handle <- curl::new_handle(useragent = "scythe (https://github.com/DataONEorg/scythe)",
                             timeout = 60)
  tryCatch(curl::curl_fetch_memory(url, handle = handle),
           error = function(e) list(status_code = NA_integer_, headers = raw(0),
                                    content = raw(0)))
}

# Fetch a DOI's related identifiers from DataCite, as a data.frame with the
# related DOI (normalized) and the relationType. Returns NULL for DOIs that
# DataCite does not register (such as Crossref DOIs) or that have no related
# identifiers, and NA when the lookup failed. Lookups are sent one at a time,
# well under DataCite's rate limit; a 429, a server error or a network error
# is retried with backoff before giving up.
datacite_related <- function(doi, attempts = 4) {
  url <- paste0("https://api.datacite.org/dois/", utils::URLencode(doi, reserved = TRUE))
  for (attempt in seq_len(attempts)) {
    res <- datacite_fetch(url)
    code <- res$status_code
    if (!is.na(code) && code == 404) return(NULL)
    if (!is.na(code) && code == 200) {
      record <- tryCatch(jsonlite::fromJSON(rawToChar(res$content), simplifyVector = FALSE),
                         error = function(e) NULL)
      if (is.null(record)) return(NA)
      related <- record$data$attributes$relatedIdentifiers
      if (is.null(related) || length(related) == 0) return(NULL)
      return(data.frame(
        doi = normalize_doi(vapply(related, function(r) r$relatedIdentifier %||% NA_character_, "")),
        relation_type = vapply(related, function(r) r$relationType %||% NA_character_, ""),
        stringsAsFactors = FALSE
      ))
    }
    # Other client errors won't succeed on a retry.
    if (!is.na(code) && code != 429 && code < 500) return(NA)
    if (attempt < attempts) {
      headers <- if (length(res$headers)) curl::parse_headers_list(res$headers) else list()
      wait <- suppressWarnings(as.numeric(headers[["retry-after"]] %||% NA))
      http_sleep(if (is.na(wait)) min(60, 2^attempt) else wait)
    }
  }
  NA
}

#' Classify the data-to-data links found by an OpenAlex search
#'
#' [citation_search_openalex()] returns only articles that cite datasets. Works
#' that are datasets themselves, or that come from a data repository, are
#' logged separately to `openalex-data-relations.jsonl`, since OpenAlex only
#' records that one cites or mentions the other. Many of these links are
#' housekeeping rather than reuse: a new version of a dataset that mentions
#' its predecessor, or a data package that lists its members.
#'
#' This function looks up each link in DataCite, which often records how two
#' datasets are related, and labels it with a `relation_class` of "version",
#' "derivation", "composition" or "citation", so that links that reflect
#' reuse of data ("derivation" and "citation") can be told apart from the
#' rest. It is a separate step, run after a search is complete, because it
#' sends up to two DataCite requests for each dataset in the log.
#'
#' **It is not needed to get citation counts.** It changes neither the results
#' of [citation_search_openalex()] nor its other logs, and nothing in scythe
#' uses its labels yet. It is here for future versions that count or report
#' data-to-data citations, and for reviewing what the data relations log holds.
#'
#' Links with no DataCite relation between the two DOIs, in either direction,
#' are labelled "unknown". If that is because a DataCite lookup failed (after
#' retrying rate limits and server errors), `evidence` also records
#' `datacite:lookup_failed:<doi>`, so that the link can be checked again later.
#'
#' @param log_dir the directory holding the review logs of
#'   [citation_search_openalex()]; defaults to the `scythe.log_dir` option, or
#'   the temporary directory a search in this session used
#' @return data.frame of the data-to-data links, one per pair of DOIs, with
#'   `relation_type` (the DataCite relationTypes found, or "openalex:cites" or
#'   "openalex:mentions" if none), `relation_class` and `evidence`. It is also
#'   written to `openalex-data-relations-classified.jsonl` in `log_dir`,
#'   replacing any earlier copy.
#' @export
#' @examples
#' \dontrun{
#' citation_search_openalex(identifiers, log_dir = "logs")
#' relations <- classify_data_relations("logs")
#' table(relations$relation_class)
#' }
classify_data_relations <- function(log_dir = NULL) {
  log_dir <- openalex_log_dir(log_dir)
  path <- file.path(log_dir, "openalex-data-relations.jsonl")
  relations <- if (file.exists(path)) {
    jsonlite::stream_in(file(path), verbose = FALSE)
  } else {
    data.frame(dataset_doi = character(), work_doi = character(), match_method = character())
  }
  # The log is appended to by every run; a pair logged again later (found by
  # a second method, say) replaces the earlier record.
  relations <- relations[!duplicated(paste(relations$dataset_doi, relations$work_doi),
                                     fromLast = TRUE), , drop = FALSE]
  relations <- classify_relations(relations)
  out <- file.path(log_dir, "openalex-data-relations-classified.jsonl")
  unlink(out)
  write_jsonl(relations, out)
  relations
}

# Label each data-to-data link in `relations` (with dataset_doi, work_doi and
# match_method columns) from DataCite's relatedIdentifiers.
classify_relations <- function(relations) {
  if (nrow(relations) == 0) {
    return(cbind(relations, relation_type = character(), relation_class = character(),
                 evidence = character()))
  }
  relations$relation_type <- ifelse(relations$match_method == "text",
                                    "openalex:mentions", "openalex:cites")
  relations$relation_class <- rep("unknown", nrow(relations))
  relations$evidence <- paste0("openalex:", relations$match_method)

  cache <- new.env(parent = emptyenv())
  lookup <- function(doi) {
    if (!exists(doi, envir = cache, inherits = FALSE)) {
      assign(doi, datacite_related(doi), envir = cache)
    }
    get(doi, envir = cache, inherits = FALSE)
  }
  for (i in seq_len(nrow(relations))) {
    from <- relations$dataset_doi[i]
    to <- relations$work_doi[i]
    types <- character()
    evidence <- character()
    failed <- character()
    for (side in list(c(from, to), c(to, from))) {
      related <- lookup(side[1])
      if (identical(related, NA)) {
        failed <- c(failed, side[1])
        next
      }
      hit <- unique(related$relation_type[!is.na(related$doi) & related$doi == side[2]])
      if (length(hit)) {
        types <- c(types, hit)
        evidence <- c(evidence, paste0("datacite:", side[1]))
      }
    }
    if (length(types)) {
      # A pair can have several relations (a new version that also references
      # its predecessor); keep them all and classify by the most specific.
      classes <- unname(relation_classes[types])
      classes <- classes[!is.na(classes)]
      relations$relation_type[i] <- paste(unique(types), collapse = ";")
      relations$relation_class[i] <- if (length(classes)) {
        relation_class_order[min(match(classes, relation_class_order))]
      } else {
        "unknown"
      }
      relations$evidence[i] <- paste(evidence, collapse = ";")
    } else if (length(failed)) {
      relations$evidence[i] <- paste(c(relations$evidence[i],
                                       paste0("datacite:lookup_failed:", failed)), collapse = ";")
    }
  }
  n_failed <- sum(vapply(ls(cache), function(d) identical(get(d, envir = cache), NA), TRUE))
  if (n_failed) {
    message("DataCite lookups failed for ", n_failed, " DOIs; their relations are ",
            "logged as unknown with datacite:lookup_failed evidence.")
  }
  relations
}

# Directory for the review logs: `log_dir`, else the scythe.log_dir option,
# else a temporary directory.
openalex_log_dir <- function(log_dir = NULL) {
  log_dir <- log_dir %||% getOption("scythe.log_dir") %||% file.path(tempdir(), "scythe-logs")
  dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
  log_dir
}

# Build a data.frame of log records from columns whose first argument may be
# empty, recycling the remaining (scalar) columns; NULL when there is nothing
# to log.
log_rows <- function(...) {
  cols <- list(...)
  n <- length(cols[[1]])
  if (n == 0) return(NULL)
  as.data.frame(lapply(cols, rep_len, n), stringsAsFactors = FALSE)
}

# Append the rows of a data.frame to a JSON Lines file, one object per row.
write_jsonl <- function(df, path) {
  if (is.null(df) || nrow(df) == 0) return(invisible())
  con <- file(path, open = "a", encoding = "UTF-8")
  on.exit(close(con))
  jsonlite::stream_out(df, con, verbose = FALSE, na = "null", auto_unbox = TRUE)
}

# Completed batches are recorded in a JSON Lines checkpoint so that a rerun
# with the same checkpoint directory skips them. Each line holds the batch key
# (method, date filter and DOIs) and the pairs that batch found.
read_checkpoint <- function(checkpoint_dir) {
  path <- file.path(checkpoint_dir %||% "", "openalex-checkpoint.jsonl")
  if (is.null(checkpoint_dir) || !file.exists(path)) return(list())
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  records <- lapply(lines, function(l) tryCatch(jsonlite::fromJSON(l), error = function(e) NULL))
  records <- Filter(Negate(is.null), records)
  stats::setNames(lapply(records, function(r) {
    if (length(r$pairs) == 0) empty_pairs() else as.data.frame(r$pairs, stringsAsFactors = FALSE)
  }), vapply(records, `[[`, "", "key"))
}

write_checkpoint <- function(checkpoint_dir, key, pairs) {
  if (is.null(checkpoint_dir)) return(invisible())
  dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
  line <- jsonlite::toJSON(list(key = key, pairs = pairs), auto_unbox = TRUE,
                           na = "null", dataframe = "rows")
  cat(line, "\n", file = file.path(checkpoint_dir, "openalex-checkpoint.jsonl"),
      sep = "", append = TRUE)
}
