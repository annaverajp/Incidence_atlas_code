library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)

###############################################################################
# The following script creates a dataset with all first time diagnoses on the #
# fourth level (four characters). Using both LPR2 and LPR3                    #
# Diagnoses that are given before age 30 years at least 25% of the time are   #
# excluded.                                                                   # 
###############################################################################

########## Get data ###############
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
diags_LPR2_level4 <- DBI::dbReadTable(conn = conn_inci, name = "diags_LPR2_level4")
diags_LPR3_level4 <- DBI::dbReadTable(conn = conn_inci, name = "diags_LPR3_level4")


########## Merging the data ###############

diags_LPR2_level4 <- diags_LPR2_level4 %>% 
  select(PERSON_ID, DIAG, D_INDDTO) %>% 
  mutate(LPR_VERSION = "LPR2")

diags_LPR3_level4 <- diags_LPR3_level4 %>% 
  select(PERSON_ID, DIAG, D_INDDTO) %>% 
  mutate(LPR_VERSION = "LPR3")

diags_all_level4 <- rbind(diags_LPR2_level4, diags_LPR3_level4) %>% 
  group_by(PERSON_ID, DIAG) %>% 
  filter(D_INDDTO == min(D_INDDTO))


######### Select diags that primarily occur after age 30 years ###############
# The selection was already done for diags_all_level3. We therefore use this
# dataset to select diags for diags_all_level4

diags_all_level4 <- diags_all_level4 %>% 
  mutate(DIAG4 = DIAG,
         DIAG = substr(DIAG, 1, 4))

diags_all_level3 <- DBI::dbReadTable(conn = conn_inci, name = "diags_all_level3")

diags_all_level3 <- diags_all_level3 %>% 
  mutate(DIAG = substr(DIAG, 1, 4)) %>% 
  select(DIAG) %>% 
  unique()

diags_all_level4 <- left_join(diags_all_level3, diags_all_level4, by = "DIAG")


########## Save data ###############

dbWriteTable(conn_inci, "diags_all_level4", diags_all_level4, overwrite = TRUE)
