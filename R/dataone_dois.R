# Collecting the DOIs of DataONE datasets to search for citations.
#
# Each priority node's DOIs come from the node's own Solr index, which is
# current, while the coordinating node (CN) can lag and has gaps. Only the
# records a node is authoritative for (authoritativeMN) are kept, which drops
# the replicas it holds for other nodes and counts a record whose authority
# was transferred under its new node. A node whose index refuses queries, or
# returns fewer authoritative records than the CN (an index problem, such as
# GOA's quoted authoritativeMN values), falls back to the CN. All other nodes'
# DOIs come from the CN.
#
# A dataset can have several DOIs: each version's identifier and the series
# identifier can all be DOIs. Every one is collected and grouped under the
# dataset (its series identifier, or else the first version in its chain).

dataone_cn <- "https://cn.dataone.org/cn"

doi_record_query <- "(id:doi* OR seriesId:doi*)"
doi_record_fields <- c("id", "seriesId", "obsoletes", "obsoletedBy", "authoritativeMN",
                       "formatType", "dateUploaded")

# Send a Solr query to a node's (or the CN's) query endpoint and return the
# parsed response, retrying server and network errors. Any other error stops
# with a condition of class `scythe_solr_error`.
solr_query <- function(base_url, params) {
  values <- vapply(params, function(v) utils::URLencode(as.character(v), reserved = TRUE), "")
  url <- paste0(sub("/+$", "", base_url), "/v2/query/solr/?",
                paste(names(params), values, sep = "=", collapse = "&"))
  for (attempt in 1:4) {
    res <- http_get(url)[[1]]
    if (isTRUE(res$status == 200)) {
      return(jsonlite::fromJSON(res$body, simplifyVector = FALSE))
    }
    retry <- is.na(res$status) || res$status >= 500
    if (!retry || attempt == 4) break
    http_sleep(2^attempt)
  }
  stop(structure(class = c("scythe_solr_error", "error", "condition"),
                 list(message = paste0("Solr query to ", base_url, " failed (",
                                       describe_response(res), ")"),
                      call = NULL, status = res$status)))
}

authoritative_filter <- function(nodes) {
  paste0("authoritativeMN:(", paste0("\"", nodes, "\"", collapse = " OR "), ")")
}

# The number of DOI records matching the filter `fq`.
solr_count <- function(base_url, fq) {
  solr_query(base_url, list(q = doi_record_query, fq = fq, rows = 0, wt = "json"))$response$numFound
}

# All DOI records matching the filter `fq`, read a page at a time with a
# cursor, as a data.frame with one character column per field.
solr_doi_records <- function(base_url, fq, page_size = 1000, label = base_url) {
  cursor <- "*"
  pages <- list()
  repeat {
    res <- solr_query(base_url, list(q = doi_record_query, fq = fq, rows = page_size,
                                     sort = "id asc", cursorMark = cursor, wt = "json",
                                     fl = paste(doi_record_fields, collapse = ",")))
    docs <- res$response$docs
    if (length(docs)) pages[[length(pages) + 1]] <- docs_to_df(docs)
    if (length(pages) %% 50 == 0 && length(docs)) {
      message(label, ": read ", length(pages) * page_size, " of ", res$response$numFound, " records")
    }
    nxt <- res$nextCursorMark
    if (is.null(nxt) || identical(nxt, cursor) || length(docs) == 0) break
    cursor <- nxt
  }
  do.call(rbind, c(list(docs_to_df(list())), pages))
}

docs_to_df <- function(docs) {
  cols <- lapply(doi_record_fields, function(f) {
    vapply(docs, function(d) as.character(unlist(d[[f]]))[1] %||% NA_character_, "")
  })
  as.data.frame(stats::setNames(cols, doi_record_fields), stringsAsFactors = FALSE)
}

# The DataONE member nodes, with their identifiers and base URLs, from the
# CN's node list.
dataone_nodes <- function(cn_url = dataone_cn) {
  res <- http_get(paste0(cn_url, "/v2/node"))[[1]]
  if (!isTRUE(res$status == 200)) stop("Couldn't read the DataONE node list (", describe_response(res), ")")
  blocks <- regmatches(res$body, gregexpr("<node[ >].*?</node>", res$body))[[1]]
  field <- function(block, tag) {
    m <- regmatches(block, regexpr(paste0("<", tag, ">[^<]*</", tag, ">"), block))
    if (length(m)) sub(paste0("^<", tag, ">(.*)</", tag, ">$"), "\\1", m) else NA_character_
  }
  attr_of <- function(block, name) {
    m <- regmatches(block, regexpr(paste0(name, "=\"[^\"]*\""), block))
    if (length(m)) sub(".*=\"(.*)\"", "\\1", m) else NA_character_
  }
  nodes <- data.frame(
    node = vapply(blocks, field, "", tag = "identifier", USE.NAMES = FALSE),
    base_url = vapply(blocks, field, "", tag = "baseURL", USE.NAMES = FALSE),
    type = vapply(blocks, attr_of, "", name = "type", USE.NAMES = FALSE),
    state = vapply(blocks, attr_of, "", name = "state", USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  nodes[nodes$type %in% "mn", , drop = FALSE]
}

# The DOI records a priority node is authoritative for, from its own index or,
# when that index refuses queries or comes up short, from the CN.
priority_node_records <- function(node, nodes, cn_url) {
  fq <- authoritative_filter(node)
  cn_count <- solr_count(cn_url, fq)
  base_url <- nodes$base_url[match(node, nodes$node)]
  problem <- NULL
  if (is.na(base_url)) {
    problem <- "it isn't in the DataONE node list"
  } else {
    own_count <- tryCatch(solr_count(base_url, fq), scythe_solr_error = function(e) e)
    if (inherits(own_count, "error")) {
      problem <- paste0("its index refused the query: ", conditionMessage(own_count))
    } else if (own_count < cn_count) {
      problem <- paste0("its index has ", own_count, " authoritative DOI records, but the CN has ",
                        cn_count)
    }
  }
  if (!is.null(problem)) {
    scythe_warning("scythe_index_fallback", node = node,
                   paste0(node, ": using the CN's index instead of the node's own, because ",
                          problem, "."))
    records <- solr_doi_records(cn_url, fq, label = node)
    index <- "cn"
  } else {
    records <- tryCatch(solr_doi_records(base_url, fq, label = node),
                        scythe_solr_error = function(e) e)
    if (inherits(records, "error")) {
      scythe_warning("scythe_index_fallback", node = node,
                     paste0(node, ": using the CN's index instead of the node's own, because ",
                            "reading it failed: ", conditionMessage(records), "."))
      records <- solr_doi_records(cn_url, fq, label = node)
      index <- "cn"
    } else {
      index <- "member"
    }
  }
  message(node, ": ", nrow(records), " DOI records from ",
          if (index == "member") "its own index" else "the CN", " (the CN has ", cn_count, ")")
  records$index <- rep(index, nrow(records))
  records$cn_count <- rep(cn_count, nrow(records))
  attr(records, "source") <- list(index = index, cn_count = cn_count)
  records
}

# Label each record with its dataset: the series identifier if any version
# has one, or else the first-uploaded version of its version chain. Versions
# are linked through obsoletes and obsoletedBy, including versions that
# aren't themselves in `records` (such as a version with a UUID identifier
# between two DOI versions).
dataset_keys <- function(records) {
  n <- nrow(records)
  if (n == 0) return(character())
  ids <- records$id
  sids <- ifelse(is.na(records$seriesId), NA_character_, paste0("sid:", records$seriesId))
  vertices <- unique(c(ids, stats::na.omit(records$obsoletes), stats::na.omit(records$obsoletedBy),
                       stats::na.omit(sids)))
  v <- function(x) match(x, vertices)
  edges <- rbind(
    cbind(v(ids), v(records$obsoletes)),
    cbind(v(ids), v(records$obsoletedBy)),
    cbind(v(ids), v(sids))
  )
  edges <- edges[stats::complete.cases(edges), , drop = FALSE]
  label <- seq_along(vertices)
  # Propagate the smallest label along the edges until every connected
  # version shares one.
  repeat {
    low <- pmin(label[edges[, 1]], label[edges[, 2]])
    candidate <- c(label, low, low)
    vertex <- c(seq_along(label), edges[, 1], edges[, 2])
    o <- order(vertex, candidate)
    new <- candidate[o][!duplicated(vertex[o])]
    new <- new[new]
    if (identical(new, label)) break
    label <- new
  }
  component <- label[v(ids)]
  first_sid <- tapply(records$seriesId, component, function(s) sort(stats::na.omit(s))[1])
  first_version <- tapply(seq_len(n), component, function(i) {
    i[order(records$dateUploaded[i], records$id[i])][1]
  })
  key <- first_sid[as.character(component)]
  ifelse(is.na(key), ids[first_version[as.character(component)]], key)
}

# One row per DOI found in each record's identifier and series identifier,
# before duplicates are removed (see unique_dois()).
records_to_dois <- function(records) {
  records$dataset <- dataset_keys(records)
  rows <- lapply(c("id", "seriesId"), function(field) {
    doi <- normalize_doi(records[[field]])
    keep <- !is.na(doi)
    data.frame(doi = doi[keep], identifier = records[[field]][keep], field = rep(field, sum(keep)),
               node = records$authoritativeMN[keep], dataset = records$dataset[keep],
               format_type = records$formatType[keep], date_uploaded = records$dateUploaded[keep],
               index = records$index[keep], group = records$group[keep], stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Keep one row per DOI (a series identifier appears on every version),
# preferring a priority node's record, then an identifier over a series
# identifier, then the earliest upload.
unique_dois <- function(dois) {
  dois <- dois[order(dois$group != "priority", dois$field != "id", dois$date_uploaded), , drop = FALSE]
  dois[!duplicated(dois$doi), , drop = FALSE]
}

#' Collect the DOIs of DataONE datasets
#'
#' Collects every DOI of the datasets held by DataONE member nodes, ready to
#' search for citations. Each priority node's DOIs come from the node's own
#' search index, which is up to date, while the coordinating node (CN) can lag
#' behind it. Only the records a node is authoritative for are kept, so the
#' copies it holds of other nodes' records are left out. A node whose index
#' refuses queries, or has fewer authoritative records than the CN, falls back
#' to the CN, with a warning of class `scythe_index_fallback`. The other
#' nodes' DOIs come from the CN.
#'
#' Every DOI of a dataset is collected: each version's identifier and the
#' series identifier, including versions replaced by newer ones. The
#' `dataset` column groups them: the series identifier if the dataset has one,
#' or else the identifier of its first version.
#'
#' @param priority_nodes member node identifiers (such as "urn:node:ARCTIC")
#'   whose DOIs are read from their own indexes
#' @param other_nodes more member nodes whose DOIs are read from the CN, or
#'   "all" for every member node that isn't a priority node
#' @param exclude_nodes nodes to leave out when `other_nodes` is "all"
#' @param exclude_prefixes DOI prefixes to skip, as a named list mapping a
#'   node to its prefixes, for example
#'   `list("urn:node:KNB" = c("10.6073/AA/", "10.6085/AA/"))`
#' @param cn_url the coordinating node's base URL
#' @return data.frame with one row per DOI: `doi` (normalized), `identifier`
#'   (as registered in DataONE), `field` ("id" or "seriesId"), `node`,
#'   `dataset`, `format_type`, `date_uploaded`, `index` ("member" or "cn")
#'   and `group` ("priority" or "other"). A `doi_sources` attribute summarizes
#'   each node: where its DOIs came from, the CN's count, and how many were
#'   excluded.
#' @export
#' @examples
#' \dontrun{
#' dois <- collect_dataone_dois(
#'   c("urn:node:ARCTIC", "urn:node:KNB"),
#'   exclude_prefixes = list("urn:node:KNB" = c("10.6073/AA/", "10.6085/AA/"))
#' )
#' attr(dois, "doi_sources")
#' }
collect_dataone_dois <- function(priority_nodes = character(), other_nodes = NULL,
                                 exclude_nodes = character(), exclude_prefixes = list(),
                                 cn_url = dataone_cn) {
  nodes <- dataone_nodes(cn_url)
  records <- lapply(priority_nodes, function(node) {
    r <- priority_node_records(node, nodes, cn_url)
    r$group <- rep("priority", nrow(r))
    r
  })
  sources <- lapply(records, attr, "source")
  if (length(other_nodes)) {
    fq <- if (identical(other_nodes, "all")) {
      skip <- c(priority_nodes, exclude_nodes)
      if (length(skip)) paste0("-", authoritative_filter(skip)) else "*:*"
    } else {
      authoritative_filter(setdiff(other_nodes, priority_nodes))
    }
    others <- solr_doi_records(cn_url, fq, label = "Other nodes (CN)")
    message("Other nodes: ", nrow(others), " DOI records from the CN")
    others$index <- rep("cn", nrow(others))
    others$cn_count <- rep(NA_real_, nrow(others))
    others$group <- rep("other", nrow(others))
    records <- c(records, list(others))
  }
  records <- do.call(rbind, c(list(cbind(docs_to_df(list()), index = character(),
                                         cn_count = numeric(), group = character())), records))
  dois <- records_to_dois(records)

  # Skip the excluded prefixes, node by node, before removing duplicates, so
  # that a DOI a priority node's index mislabels as its own (such as KNB's
  # copies of PISCO records) is still collected under its real node.
  excluded <- stats::setNames(integer(length(exclude_prefixes)), names(exclude_prefixes))
  for (node in names(exclude_prefixes)) {
    prefixes <- normalize_doi_prefix(exclude_prefixes[[node]])
    skip <- dois$node == node & Reduce(`|`, lapply(prefixes, function(p) startsWith(dois$doi, p)),
                                        FALSE)
    excluded[[node]] <- length(unique(dois$doi[skip]))
    if (any(skip)) {
      message(node, ": skipped ", excluded[[node]], " DOIs with the excluded prefixes ",
              paste(exclude_prefixes[[node]], collapse = ", "))
    }
    dois <- dois[!skip, , drop = FALSE]
  }
  dois <- unique_dois(dois)

  # One summary row per priority node (even one with no DOIs), then one per
  # other node found.
  other <- setdiff(stats::na.omit(unique(records$authoritativeMN[records$group == "other"])),
                   priority_nodes)
  summary <- data.frame(node = c(priority_nodes, other),
                        group = rep(c("priority", "other"), c(length(priority_nodes), length(other))),
                        stringsAsFactors = FALSE)
  summary$index <- c(vapply(sources, `[[`, "", "index"), rep("cn", length(other)))
  summary$cn_count <- c(vapply(sources, function(x) as.numeric(x$cn_count), 1),
                        rep(NA_real_, length(other)))
  summary$records <- as.vector(table(factor(records$authoritativeMN, levels = summary$node)))
  summary$dois <- as.vector(table(factor(dois$node, levels = summary$node)))
  summary$excluded <- unname(ifelse(summary$node %in% names(excluded), excluded[summary$node], 0))
  rownames(dois) <- NULL
  attr(dois, "doi_sources") <- summary
  dois
}

# DOI prefixes in the normalized (lowercase, bare) form of normalize_doi().
normalize_doi_prefix <- function(prefixes) {
  tolower(sub("^(doi:|https?://(dx\\.)?doi\\.org/)", "", trimws(prefixes), ignore.case = TRUE))
}

#' Save and read a collected DOI list
#'
#' A DOI list is saved as CSV, so that a search can use the same DOIs (and so
#' the same batches) until it completes.
#' @param dois a data.frame from collect_dataone_dois()
#' @param path the CSV file
#' @noRd
write_doi_list <- function(dois, path) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(dois, path, row.names = FALSE, na = "")
  invisible(path)
}

read_doi_list <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = "")
}
