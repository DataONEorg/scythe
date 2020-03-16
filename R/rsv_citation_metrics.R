# get citation metrics for reverse site visit

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

non_overlap <- anti_join(results_doi, results_loose)


############

down <- fromJSON("results/downloads_by_country.json")
d <- down$aggregations$pid_list$buckets$key
d$doc_count <- down$aggregations$pid_list$buckets$doc_count

codes <- read.csv("https://pkgstore.datahub.io/core/country-list/data_csv/data/d7c9d7cfb42cb69f4422dec222dbbaa8/data_csv.csv") %>% rename(country = Code)

d <- left_join(d, codes)

d_data <- filter(d, format == "DATA")
d_metadata <- filter(d, format == "METADATA")
t <- joinCountryData2Map(d_data, joinCode = "ISO2", nameJoinColumn = "country")

mapCountryData(mapToPlot = t, nameColumnToPlot = "doc_count", numCats = 10, catMethod = "quantiles")
