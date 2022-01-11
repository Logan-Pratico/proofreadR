# library(here)
# library(dplyr)
# library(textreadr)
# library(reshape2)
# library(tokenizers)


### FUNCTIONS###
source(here::here("R", "checkAcronyms.R"))
source(here::here("R", "checkDash.R"))
source(here::here("R", "getTotalWordCountFrequency.R"))
source(here::here("R", "sentenceWordCount.R"))

doc <- textreadr::read_document(here::here("data-raw", "<doc>.docx"))
#values <- check.acronyms(paste0(doc, collapse=" "))
values <- checkDash(doc, T)

#View(df)