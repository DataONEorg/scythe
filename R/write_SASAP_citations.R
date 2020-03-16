library(rcrossref)
library(jsonlite)
library(bib2df)
library(tibble)


# manually generated citations list
cit <- tribble(~publicationDOI, ~datasetID,
              "10.1016/j.marpol.2018.07.010", "10.5063/F1G73BVP",
              "10.1080/08941920.2019.1665763", "10.5063/F1NG4NW6",
              "10.1080/08941920.2019.1665763", "10.5063/F1QN652R",
              "10.1080/08941920.2019.1665763", "10.5063/F1K935SB",
              "10.1080/08941920.2019.1665763","10.5063/F1S75DKG")

# write list of citations to bib format
bib <- cr_cn(dois = cit$publicationDOI, format = "bibtex")
writeLines(unlist(bib), "~/dataone-citations/SASAP_citations/all_citations.bib" )

# import as a dataframe
df <- bib2df("~/dataone-citations/SASAP_citations/all_citations.bib")
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


write_json(cit_full, "~/dataone-citations/SASAP_citations/citations_export.json")

