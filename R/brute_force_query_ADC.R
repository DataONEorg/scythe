library(jsonlite)
library(dplyr)
library(xml2)
library(curl)

## bash SCOPUS queries
# APIKEY=985f23cdfde84bdcb15229504fb644b4
# 
# # multi-page ADC
# for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query=ALL:10.18739\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.18739-2009-2019-pg${pg}.json; done
# 
# # multi-page KNB
# for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:10.5063\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.5063-2009-2019-pg${pg}.json; done

# query for ADC DOIs
cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:ARCTIC")

result <- query(mn, list(q = "formatType:METADATA AND (*:* NOT obsoletedBy:*)",
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

# filter out DOIs with no results
t_working <- t[which(res != 0)]

# pull out information for datasets with results
t_results <- lapply(t_working, function(x){
  x$`search-results`$entry$search <- x$`search-results`$`opensearch:Query`$`@searchTerms`
  return(x$`search-results`$entry)
  })


results <- do.call(bind_rows, t_results)

# trim out unnecessary information
results_slim <- results %>% 
  dplyr::select(`prism:doi`, search) %>% 
  mutate(search = gsub("ALL:", "", search)) %>% 
  rename(journal = `prism:doi`,
         adc_dataset = search)

# left col - citer, right col - citee
write.csv(results_slim, "~/dataone-citations/results/brute_force_results.csv", row.names = F)



