library(jsonlite)
library(dplyr)
library(EML)
library(arcticdatautils)
library(rcrossref)
library(stringr)
library(stringi)


###### find DBO publications via DBO keyword
files <- list.files("~/dataone-citations/results/", full.names = T) %>% 
  grep("DBO", . ,value = T)


adc <- list()
for (i in 1:length(files)){
  res <- fromJSON(files[i])
  adc <- bind_rows(adc, res$`search-results`$entry)
}

# ^ throws: Error in if (is.character(txt) && length(txt) == 1 && nchar(txt, type = "bytes") <  : 
#           missing value where TRUE/FALSE needed


dbo <- read_eml("~/ArcticSupport/DBO_Page/DBOpage.xml")

dbo_researchers <- eml_get_simple(dbo$filterGroup$choiceFilter[[2]]$choice, "label")
dbo_researchers <- stringi::stri_split_fixed(dbo_researchers, pattern = " ", simplify = T, omit_empty = F) %>% 
  as.data.frame(., stringsAsFactors = F) %>% 
  mutate(V3 = ifelse(V3 == "", V2, V3))

dbo_researchers <- dbo_researchers$V3

adc$`dc:creator` <- gsub(" [A-Z].", "", adc$`dc:creator`)

citations_DBO <- adc %>% 
  filter(`dc:creator` %in% dbo_researchers)

DBO_example <- citations_DBO[7, ]

bib <- cr_cn(dois = adc$`prism:doi`, format = "bibtex")
test <- cr_cn(dois = adc$`prism:doi`, format = "text")

test <- test[str_detect(test, paste0(dbo_researchers, collapse = "|"))]

writeLines(unlist(test), "~/ArcticSupport/DBO_Page/dbo_bib_text.txt")


####### find publications citing ADC associated with DBO authors

files <- list.files("~/dataone-citations/results/", full.names = T) %>% 
  grep("ADC|10.18739", . ,value = T)


adc <- list()
for (i in 1:length(files)){
  res <- fromJSON(files[i])
  adc <- bind_rows(adc, res$`search-results`$entry)
}

adc <- adc[!duplicated(adc$`dc:title`), ]

test <- cr_cn(dois = adc$`prism:doi`, format = "text")
test <- test[str_detect(test, paste0(dbo_researchers, collapse = "|"))]
