library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)

###############################################################################
# The following script creates a dataset with all first time diagnoses on the #
# fourth level (four characters) from LPR 3.                                  # 
###############################################################################

########## Get LPR 3 data ###############
conn_LPR <- OraGenvej(dbname = "STATPROD", dbuser = "[D222008]")
dbListTables(conn_LPR)

LPR3KONTAKTER <- dbReadTable(conn = conn_LPR, name = "LPR3_TMP_KONTAKTER")
LPR3DIAGNOSER <- dbReadTable(conn = conn_LPR, name = "LPR3_TMP_DIAGNOSER")

# create own dataset
diags_LPR3 <- left_join(select(LPR3KONTAKTER, PERSON_ID, DW_EK_KONTAKT, DATO_START), select(LPR3DIAGNOSER, DW_EK_KONTAKT, DIAGNOSEKODE, SENERE_AFKRAEFTET), by = "DW_EK_KONTAKT")

diags_LPR3 <- diags_LPR3 %>% 
  filter(SENERE_AFKRAEFTET == "Nej") %>% 
  select(PERSON_ID, DIAGNOSEKODE, DATO_START) %>% 
  mutate(DIAG = substr(str_trim(DIAGNOSEKODE), 1, 5)) %>% 
  group_by(PERSON_ID, DIAG) %>% 
  filter(DATO_START == min(DATO_START)) %>% 
  filter(DIAG <= "DT983",
         DIAG >= "DA000") %>% 
  mutate(D_INDDTO = as.Date(DATO_START),
         LPR_VERSION = "LPR3") %>% 
  select(PERSON_ID, DIAG, D_INDDTO, LPR_VERSION) %>% 
  unique()


# Select only people from Lifelines
conn_LL <- OraGenvej(dbname = "STATPROD", dbuser = "[D220005]")

pop_id <- DBI::dbReadTable(conn = conn_LL, name = "MERGED_LINES") %>% 
  select(PERSON_ID) %>% 
  unique()

diags_LPR3_level4 <- left_join(pop_id, diags_LPR3, by = "PERSON_ID") %>% 
  filter(!is.na(DIAG))


### Save data ###
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
DBI::dbWriteTable(conn_inci, 'diags_LPR3_level4', diags_LPR3_level4, overwrite = TRUE)







