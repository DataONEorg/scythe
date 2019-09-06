# parse SCOPUS results

library(dplyr)
library(jsonlite)

files <- list.files("~/dataone-citations/results/", full.names = T) %>%
  grep("10.18739", . ,value = T)

results_doi <- list()
for (i in 1:length(files)){
  res <- fromJSON(files[i])
  results_doi <- bind_rows(results_doi, res$`search-results`$entry)
}

res_cdb <- fromJSON("~/dataone-citations/results/ADC_citations_db.json")
res_cdb <- res_cdb$resultDetails$citations

files_loose <- list.files("~/dataone-citations/results/", full.names = T)
files_loose <- files_loose[4:7]

results_loose <- list()
for (i in 1:length(files_loose)){
  res <- fromJSON(files_loose[i])
  results_loose <- bind_rows(results_loose, res$`search-results`$entry)
}

results_loose <- unique(results_loose) %>%
  select(-link, -affiliation)

overlap <- inner_join(results_doi, results_loose)
