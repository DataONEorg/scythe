#' Search for citations in PLOS
#'
#' This function searches for citations in PLOS. Requests are throttled
#' at one identifier every 6 seconds so as to not overload the PLOS
#' API. This function uses modified source code from the `rplos` package, 
#' which is no longer maintained.
#'
#' @param identifiers a vector of identifiers to be searched for
#'
#' @return tibble of matching dataset and publication identifiers
#' @export
#' @examples
#' \dontrun{
#' identifiers <- c("10.18739/A22274", "10.18739/A2D08X", "10.5063/F1T151VR")
#' result <- citation_search_plos(identifiers)
#' }
citation_search_plos <- function(identifiers) {
  if (length(identifiers) > 1) {
    message(
      paste0(
        "Your result will take ~",
        length(identifiers) * 6 ,
        " seconds to return,
                   since this function is rate limited to one call every 6 seconds."
      )
    )
  }
  
  identifiers <- check_identifiers(identifiers)
  
  # encode colons to not break PLOS API
  identifiers <- gsub(":", "%3A", identifiers)
  
  # search for identifier
  results <- lapply(identifiers, function(x) {
    Sys.sleep(6)
    v <- searchplos(q = x,
                           fl = c("id", "title"),
                           limit = 1000)
    return(v)
    
  })
  
  plos_results <- list()
  # assign dataset identifier to each result
  for (i in 1:length(results)) {
    if (results[[i]]$meta$numFound == 0 | is.null(results[[i]])) {
      plos_results[[i]] <- data.frame(
        id = NA,
        dataset_id = identifiers[i],
        title = NA,
        source = "plos"
      )
    }
    else if (results[[i]]$meta$numFound > 0) {
      plos_results[[i]] <- results[[i]]$data
      plos_results[[i]]$dataset_id <- identifiers[i]
      plos_results[[i]]$source <- "plos"
    }
    
  }
  
  # bind resulting tibbles
  plos_results <- do.call(rbind, plos_results)
  names(plos_results)[which(names(plos_results) == "id")] <-
    "article_id"
  names(plos_results)[which(names(plos_results) == "title")] <-
    "article_title"
  plos_results <-
    plos_results[stats::complete.cases(plos_results),] # remove incomplete cases (NAs)
  
  return(plos_results)
}

#' A Modified Version of rplos::searchplos
#'
#' This function is adapted from the searchplos in the `rplos` package, which is no longer maintained.
#'
#' @param q Search terms, eg: field:query 
#' @param fl Fields to return
#' @param fq Fields to filter query on
#' @param sort Sort results according to field
#' @param start Record to start at for pagination
#' @param limit Number of results to return for pagination
#' @param sleep Seconds to wait between requests
#' @param errors One of simple or complete
#' @param proxy List of args for proxy connection
#' @param callopts Optional curl options
#' @param progress Optional logic for progress bar
#' @param ... Addtl Solr arguments
searchplos <- function(q = NULL, fl = 'id', fq = NULL, sort = NULL, start = 0,
                       limit = 10, sleep = 6, errors = "simple", proxy = NULL, callopts = list(), 
                       progress = NULL, ...) {
    
    # Make sure limit is a numeric or integer
    limit <- tryCatch(as.numeric(as.character(limit)), warning=function(e) e)
    if("warning" %in% class(limit)){
        stop("limit should be a numeric or integer class value", call. = FALSE)
    }
    if(!inherits(limit, "numeric") | is.na(limit))
        stop("limit should be a numeric or integer class value", call. = FALSE)
    
    if (is.null(limit)) limit <- 999
    if (limit == 0) fl <- NULL
    fl <- paste(fl, collapse = ",")
    
    args <- list()
    if (!is.null(fq[[1]])) {
        if (length(fq) == 1) {
            args$fq <- fq
        } else {
            args <- fq
            names(args) <- rep("fq",length(args))
        }
    }
    args <- c(args, ploscompact(list(q = q, fl = fl, start = as.integer(start),
                                     rows = as.integer(limit), sort = sort, wt = 'json')))
    
    conn_plos <- solrium::SolrClient$new(host = "api.plos.org", path = "search", port = NULL)
    
    getnum_tmp <- suppressMessages(
        conn_plos$search(params = list(q = q, fl = fl, rows = 0, wt = "json"))
    )
    getnumrecords <- attr(getnum_tmp, "numFound")
    
    if (getnumrecords > limit) {
        getnumrecords <- limit
    } else {
        getnumrecords <- getnumrecords
    }
    
    if (min(getnumrecords, limit) < 1000) {
        if (!is.null(limit)) args$rows <- limit
        if (length(args) == 0) args <- NULL
        jsonout <- suppressMessages(
            conn_plos$search(params = args, callopts = callopts,
                             minOptimizedRows = FALSE, progress = progress, ...)
        )
        meta <- dplyr::tibble(
            numFound = attr(jsonout, "numFound"),
            start = attr(jsonout, "start")
        )
        return(list(meta = meta, data = jsonout))
    } else {
        byby <- 500
        getvecs <- seq(from = 0, to = getnumrecords - 1, by = byby)
        lastnum <- as.numeric(strextract(getnumrecords, "[0-9]{3}$"))
        if (lastnum == 0)
            lastnum <- byby
        if (lastnum > byby) {
            lastnum <- getnumrecords - getvecs[length(getvecs)]
        } else {
            lastnum <- lastnum
        }
        getrows <- c(rep(byby, length(getvecs) - 1), lastnum)
        out <- list()
        for (i in seq_along(getvecs)) {
            args$start <- as.integer(getvecs[i])
            args$rows <- as.integer(getrows[i])
            if (length(args) == 0) args <- NULL
            jsonout <- suppressMessages(conn_plos$search(
                params = ploscompact(list(q = args$q, fl = args$fl, 
                                          fq = args[names(args) == "fq"],
                                          sort = args$sort,
                                          rows = as.integer(args$rows), start = as.integer(args$start),
                                          wt = "json")), minOptimizedRows = FALSE, callopts = callopts,
                progress = progress, ...
            ))
            out[[i]] <- jsonout
        }
        resdf  <- dplyr::bind_rows(out)
        meta <- dplyr::tibble(
            numFound = attr(jsonout, "numFound"),
            start = attr(jsonout, "start")
        )
        return(list(meta = meta, data = resdf))
    }
}
#' This function is from the `rplos` package, which is no longer maintained.
#' @param l a list
ploscompact <- function(l) Filter(Negate(is.null), l)

#' This function is from the `rplos` package, which is no longer maintained.
#'
#' @param str A string
#' @param pattern A regex pattern
strextract <- function(str, pattern) {
    regmatches(str, regexpr(pattern, str))
}
