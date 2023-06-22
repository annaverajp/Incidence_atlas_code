library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

##############################################################################
# The following script calculates the total number of cases and per          #
# educational level for all diseases studied.                                #
##############################################################################

## Adgang til Incidence af alt
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")

## Get the data needed
diags_all_level3 <- dbReadTable(conn = conn_inci, name = "diags_all_level3")

Population_2012_2021 <- dbReadTable(conn = conn_inci, name = "Population_2012_2021")

## Calculate frequencies
N <- left_join(Population_2012_2021, diags_all_level3, by = "PERSON_ID") %>% 
  filter(!is.na(DIAG)) %>% 
  mutate(prior = as.Date(D_INDDTO) >= as.Date(STUDY_ENTRANCE),
         check = as.Date(D_INDDTO) <= as.Date(STUDY_EXIT)) %>% 
  filter(prior == TRUE & check == TRUE) %>% 
  group_by(DIAG) %>% 
  summarise(N = n()) %>% 
  filter(N >= 100) %>% 
  mutate(prop = N/sum(N))

## save data
dbWriteTable(conn_inci, "N_all", N, overwrite = TRUE)

