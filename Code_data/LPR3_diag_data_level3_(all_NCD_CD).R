library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)

###############################################################################
# The following script creates a dataset with all first time diagnoses on the #
# third level (three characters) from LPR 3.                                  # 
###############################################################################

########## Get LPR 3 data ###############
conn_PSD <- OraGenvej("DB_PSD", "X30")

LPR3KONTAKTER <- DBI::dbGetQuery(conn_PSD, 'SELECT PERSON_ID, DW_EK_KONTAKT, DATO_START FROM D282171."PSD_LPR3_KONTAKTER"')

LPR3DIAGNOSER <- DBI::dbGetQuery(conn_PSD, 'SELECT DW_EK_KONTAKT, DIAGNOSEKODE, SENERE_AFKRAEFTET FROM D282171."PSD_LPR3_DIAGNOSER"')


# create own dataset
diags_LPR3 <- left_join(LPR3KONTAKTER, LPR3DIAGNOSER, by = "DW_EK_KONTAKT")

diags_LPR3 <- diags_LPR3 %>% 
  filter(SENERE_AFKRAEFTET == "Nej") %>% 
  select(PERSON_ID, DIAGNOSEKODE, DATO_START) %>% 
  mutate(DIAG = substr(str_trim(DIAGNOSEKODE), 1, 4)) %>% 
  group_by(PERSON_ID, DIAG) %>% 
  filter(DATO_START == min(DATO_START)) %>% 
  filter(DIAG <= "DT98",
         DIAG >= "DA00") %>% 
  mutate(D_INDDTO = as.Date(DATO_START),
         LPR_VERSION = "LPR3") %>% 
  select(PERSON_ID, DIAG, D_INDDTO, LPR_VERSION) %>% 
  unique()


# Select only people from our study population
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
Population <- DBI::dbReadTable(conn = conn_inci, name = "Population_2013_2022")


pop_id <- Population %>% 
  select(PERSON_ID) %>% 
  unique()


diags_LPR3_level3 <- left_join(pop_id, diags_LPR3, by = "PERSON_ID") %>% 
  filter(!is.na(DIAG))


### Save data ###
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
DBI::dbWriteTable(conn_inci, 'diags_LPR3_level3', diags_LPR3_level3, overwrite = TRUE)






