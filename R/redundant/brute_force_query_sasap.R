# SASAP only dois

library(jsonlite)
library(dplyr)
library(xml2)

## bash SCOPUS queries
# Need to use our API key in links below (saved separately)
#
# # multi-page ADC
# for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query=ALL:10.18739\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.18739-2009-2019-pg${pg}.json; done
# 
# # multi-page KNB
# for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:10.5063\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.5063-2009-2019-pg${pg}.json; done

# query for ADC DOIs
cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")

result <- query(mn, list(q = "projectText:'State of Alaska's Salmon and People' AND (*:* NOT obsoletedBy:*)",
                                fl = "identifier,rightsHolder,formatId",
                                start ="0",
                                rows = "15000"),
                                as="data.frame")

dois <- grep("doi", result$identifier, value = T) %>% 
  gsub("doi:", "", .)

# brute force query SCOPUS for each DOI
t <- list()
for (i in 1:length(dois)){
  t[[i]] <- fromJSON(curl(paste0("https://api.elsevier.com/content/search/scopus?query=ALL:",dois[i],"&APIKey=ae55f95a9d2f56c21147d3f9f6c4eef0")))
}

# find the number of results per DOI
res <- lapply(t, function(x){x$`search-results`$`opensearch:totalResults`})

t_working <- t[which(res != 0)]

t_results <- lapply(t_working, function(x){
  x$`search-results`$entry$search <- x$`search-results`$`opensearch:Query`$`@searchTerms`
  return(x$`search-results`$entry)
  })

results <- do.call(bind_rows, t_results)
View(results)





