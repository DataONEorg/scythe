library(rcrossref)
library(jsonlite)
library(bib2df)


# manually generated citations list
cit <- read.csv("DBO_citations/citationlist.csv", stringsAsFactors = F)

# write list of citations to bib format
bib <- cr_cn(dois = cit$publicationDOI, format = "bibtex")
writeLines(unlist(bib), "DBO_citations/all_citations.bib" )

# import as a dataframe
df <- bib2df("~/dataone-citations/DBO_citations/all_citations.bib")
df$datasetID <- cit$datasetID

# rename for database ingest
cit_full <- df %>% 
  rename(target_id = datasetID,
         source_id = DOI,
         source_url = URL,
         origin = AUTHOR,
         title = TITLE,
         publisher = PUBLISHER,
         journal = JOURNAL,
         volume = VOLUME,
         page = PAGES,
         year_of_publishing = YEAR) %>% 
  select(target_id, source_id, source_url, origin, title, publisher, journal, volume, page, year_of_publishing) %>% 
  mutate(id = NA, report = NA, metadata = NA, link_publication_date = NA) %>% 
  mutate(publisher = ifelse(publisher == "Elsevier {BV", "Elsevier", "Copernicus"))

write_json(cit_full, "~/dataone-citations/DBO_citations/citations_export.json")

