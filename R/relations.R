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

# Fetch a DOI's related identifiers from DataCite, as a data.frame with the
# related DOI (normalized) and the relationType. Returns NULL for DOIs that
# DataCite does not register (such as Crossref DOIs) or on any error.
datacite_related <- function(doi) {
  url <- paste0("https://api.datacite.org/dois/", utils::URLencode(doi, reserved = TRUE))
  record <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE), error = function(e) NULL)
  related <- record$data$attributes$relatedIdentifiers
  if (is.null(related) || length(related) == 0) return(NULL)
  data.frame(
    doi = normalize_doi(vapply(related, function(r) r$relatedIdentifier %||% NA_character_, "")),
    relation_type = vapply(related, function(r) r$relationType %||% NA_character_, ""),
    stringsAsFactors = FALSE
  )
}

#' Classify data-to-data links using DataCite
#'
#' OpenAlex only records that one work cites or mentions another. For each
#' link, look for a DataCite relatedIdentifier between the two DOIs, from
#' either side, and map its relationTypes to a `relation_class` of version,
#' derivation, composition or citation. Links with no DataCite relation keep
#' `relation_class = "unknown"` and the OpenAlex evidence.
#' @noRd
classify_relations <- function(relations) {
  if (nrow(relations) == 0) {
    return(cbind(relations, relation_type = character(), relation_class = character(),
                 evidence = character()))
  }
  relations$relation_type <- ifelse(relations$match_method == "text",
                                    "openalex:mentions", "openalex:cites")
  relations$relation_class <- rep("unknown", nrow(relations))
  relations$evidence <- paste0("openalex:", relations$match_method)
  if (!isTRUE(getOption("scythe.classify_relations", TRUE))) {
    return(relations)
  }

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
    for (side in list(c(from, to), c(to, from))) {
      related <- lookup(side[1])
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
    }
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
