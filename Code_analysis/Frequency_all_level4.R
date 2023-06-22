library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

##############################################################################
# The following script calculates the total number of cases and per          #
# educational level for all diseases studied - at the fouth level.           #
##############################################################################

# Data needed
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
diags_all_level4 <- DBI::dbReadTable(conn = conn_inci, name = "diags_all_level4")
Population_2012_2021 <- dbReadTable(conn = conn_inci, name = "Population_2012_2021")


## Calculate frequencies
N_level4 <- left_join(Population_2012_2021, diags_all_level4, by = "PERSON_ID") %>% 
  filter(!is.na(DIAG)) %>% 
  mutate(prior = as.Date(D_INDDTO) >= as.Date(STUDY_ENTRANCE),
         check = as.Date(D_INDDTO) <= as.Date(STUDY_EXIT)) %>% 
  filter(prior == TRUE & check == TRUE) %>% 
  group_by(DIAG4) %>% 
  summarise(N = n()) %>% 
  mutate(prop = N/sum(N),
         DIAG = substr(str_trim(DIAG4), 1, 4)) 



## save data
dbWriteTable(conn_inci, "N_level4", N_level4, overwrite = TRUE)


  
