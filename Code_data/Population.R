library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 
library(DSTformat)

##############################################################################
# The following script creates the study populations.                        #
##############################################################################

## Adgang til Incidence af alt
conn_LL <- OraGenvej(dbname = "STATPROD", dbuser = "[D220005]")
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
conn_PSD <- OraGenvej("DB_PSD", "X30")

HFAUDD <- DBI::dbGetQuery(conn_PSD, 'SELECT DISTINCT PERSON_ID, HFAUDD, HF_KILDE, HF_VFRA, HF_VTIL, REFERENCETID, VERSION FROM D400600. "PSD_HOEJSTE_FULDFOERT_UDD"')

KOEN <- DBI::dbReadTable(conn = conn_LL, name = "MERGED_LINES_KOEN")
KOEN <- KOEN %>% 
  group_by(PERSON_ID) %>% 
  filter(GYLDIG_FRA == max(GYLDIG_FRA)) # Select latest registration of sex


####### ------------ Preparing datasets - separate datasets for each time period ------------- ######

Population <- construct_cohort(age = c(30,200), date = c(as.Date("2012-01-01"), as.Date("2021-12-31")), open = TRUE, version = "MERGED_LINES")

Population <- Population %>% 
  filter(as.Date(BIRTHDAY) >= as.Date("1920-01-01")) %>% 
  left_join(KOEN, by = "PERSON_ID") %>% 
  dplyr::select(PERSON_ID, BIRTHDAY, EVENT_CAUSE_START, EVENT_START_DATE, EVENT_CAUSE_FINAL, EVENT_FINAL_DATE, STUDY_ENTRANCE, STUDY_EXIT, EVENT_START, EVENT_END, KOEN) %>% 
  mutate(AGE = time_length(difftime(as.Date(STUDY_ENTRANCE), as.Date(BIRTHDAY)), "years"))



####### ------------ Getting covariates from HFAUDD ------------ ######## 

udd <- left_join(dplyr::select(Population, PERSON_ID, STUDY_ENTRANCE), HFAUDD, by = "PERSON_ID") %>% 
  filter(!is.na(HFAUDD),
         STUDY_ENTRANCE >= HF_VFRA) %>% 
  mutate(udd = DSTformat(Input = HFAUDD, Format = "AUDD_NIVEAU_L1L2_KT"),
         udd = recode(udd, 
                      "10 Grundskole til og med 6. klasse" = 1,
                      "20 Grundskole 7.-10. klasse / Forberedende uddannelser" = 1,
                      "30 Gymnasiale og erhvervsfaglige uddannelser" = 2,
                      "50 Korte videregående uddannelser" = 3,
                      "60 Mellemlange videregående uddannelser / Bachelorer" = 3,
                      "70 Lange videregående uddannelser" = 3,
                      "80 Ph.d. og forskeruddannelser" = 3,
                      "4574" = 2,
                      "5544" = 3, 
                      "7966" = 3)) %>% 
  group_by(PERSON_ID) %>% 
  summarise(udd = max(udd))

Population <- left_join(Population, udd, by = "PERSON_ID") %>% 
  filter(!is.na(udd))


#Save the data
dbWriteTable(conn_inci, "Population_2012_2021", Population, overwrite = T)
