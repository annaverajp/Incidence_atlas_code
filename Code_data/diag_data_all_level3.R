library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)

###############################################################################
# The following script creates a dataset with all first time diagnoses on the #
# third level (three characters). Using both LPR2 and LPR3                    #
# Diagnoses that are given before age 30 years at least 25% of the time are   #
# excluded.                                                                   # 
###############################################################################

########## Get data ###############
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
diags_LPR2_level3 <- DBI::dbReadTable(conn = conn_inci, name = "diags_LPR2_level3")
diags_LPR3_level3 <- DBI::dbReadTable(conn = conn_inci, name = "diags_LPR3_level3")


########## Merging the data ###############

diags_LPR2_level3 <- diags_LPR2_level3 %>% 
  select(PERSON_ID, DIAG, D_INDDTO) %>% 
  mutate(LPR_VERSION = "LPR2")

diags_LPR3_level3 <- diags_LPR3_level3 %>% 
  select(PERSON_ID, DIAG, D_INDDTO) %>% 
  mutate(LPR_VERSION = "LPR3")

diags_all_level3 <- rbind(diags_LPR2_level3, diags_LPR3_level3) %>% 
  group_by(PERSON_ID, DIAG) %>% 
  filter(D_INDDTO == min(D_INDDTO))


########## Select diags that primarily occur after age 30 years ###############

Population_2012_2021 <- DBI::dbReadTable(conn = conn_inci, name = "Population_2012_2021")

# Create data set for calculating age distribution
DIAG_select <- left_join(select(Population_2012_2021, PERSON_ID, BIRTHDAY), diags_all_level3, by = "PERSON_ID")
head(DIAG_select)

# Calculate age and the 25 % quantile and then make df only with diags with 25% quantile above age 30 years
library(lubridate)

DIAG_select <- DIAG_select %>% 
  filter(!is.na(DIAG)) %>% 
  mutate(BIRTHDAY = format(as.POSIXct(BIRTHDAY,format='%Y-%m-%d %H:%M:%S'),format='%Y-%m-%d'),
         AGE = time_length(difftime(as.Date(D_INDDTO), as.Date(BIRTHDAY)), "years"),
         AGE = AGE + 0.002737851) %>% 
  group_by(DIAG) %>% 
  summarise(QUANTILE25 = quantile(AGE, probs = 0.25)) %>% 
  filter(QUANTILE25 >= 30) %>% 
  select(DIAG) %>% 
  left_join(diags_all_level3, by = "DIAG") %>% 
  select(DIAG, PERSON_ID, D_INDDTO) %>% 
  filter(DIAG != "DQ06", DIAG != "DQ23", DIAG != "DQ28", DIAG != "DQ33", DIAG != "DQ44", DIAG != "DQ61", DIAG != "DQ91") # Deleting congenital malformations


########## Save data ###############

dbWriteTable(conn_inci, "diags_all_level3", DIAG_select, overwrite = TRUE)



