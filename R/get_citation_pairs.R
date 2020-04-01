library(jsonlite)
library(dplyr)
library(xml2)

# this will only find citations to 100% open access journals

## bash SCOPUS queries
# APIKEY=
# 
# # multi-page ADC
# for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query=ALL:10.18739\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.18739-2009-2019-pg${pg}.json; done
# 
# # multi-page KNB
# for pg in 0 26; do curl https://api.elsevier.com/content/search/scopus?query==ALL:10.5063\&date=2009-2019\&APIKey=${APIKEY}\&start=${pg} -o results/scopus-10.5063-2009-2019-pg${pg}.json; done

## read in json files from SCOPUS hits as a dataframe

files <- list.files("~/dataone-citations/results/", full.names = T) %>% 
  grep("10.5063|10.18739", . ,value = T)


results <- list()
for (i in 1:length(files)){
  res <- fromJSON(files[i])
  results <- bind_rows(results, res$`search-results`$entry)
}

## find results that have an elsevier PII so we can try and get their reference list
## for some reason docuements without a PII give me a "resource not found" error
elsevier <- results %>% 
  filter(!is.na(pii))


## save content of the elsevier article query to an xml file
dsets <- list()
for (i in 1:nrow(elsevier)){
  system(paste0("curl https://api.elsevier.com/content/article/pii/", elsevier$pii[i],"?APIKey=\\&view=META_ABS_REF -o ~/dataone-citations/temp_content/", elsevier$pii[i],".xml;"))
}

## look for KNB/ADC shoulders within the xml file for each document, and retrieve the document DOI
shoulders <- "10.5063|10.18739"
refs <- c()
document <- c()

for (i in 1:nrow(elsevier)){

  dsets <- read_xml(paste0("~/dataone-citations/temp_content/",elsevier$pii[i],".xml"))
  
  ## it seems like most of the documents are not formatting ADC references in the same way as journals
  ## this grabs the inter-ref element (whatever that is, can't find a description)
  ref1 <- xml_find_all(dsets, "//ce:inter-ref") %>% 
    xml_attr(., "xlink:href", xml_ns(dsets)) %>% 
    grep(shoulders, . ,value = T)
  
  ## this grabs all of the doi elements
  ref2 <- xml_find_all(dsets, "//ce:doi") %>% 
    grep(shoulders, ., value = T) %>% 
    gsub("<ce:doi>|</ce:doi>", "", .)
  

  refs[i] <- paste0(ref1, ref2, collapse = ";")

  ## get the document identifier
  document[i] <- xml_find_all(dsets, "//dc:identifier") %>% 
    xml_text()

}

## save pairs of references to a data frame
reference_pairs <- data.frame(target_id = unlist(document), source_id = unlist(refs), stringsAsFactors = F)





