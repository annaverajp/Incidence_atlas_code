########### ------------- Checking assumptions for poisson ------------ #############

library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

##############################################################################
# The following script tests for overdispersion.                             #
##############################################################################

## Adgang til Incidence af alt
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")


####### ------------ Access the data ------------ ######## 

diags_mainanalysis <- dbReadTable(conn = conn_inci, name = "diags_mainanalysis")

Population <- dbReadTable(conn = conn_inci, name = "Population_2013_2022")

Population <- Population %>% 
  mutate(STUDY_ENTRANCE = cal.yr(STUDY_ENTRANCE),
         STUDY_END = cal.yr(STUDY_END),
         BIRTHDAY = cal.yr(BIRTHDAY))

diags_mainanalysis <- diags_mainanalysis %>% 
  mutate(D_INDDTO = cal.yr(as.Date(D_INDDTO)))


DIAG_LIST <- diags_mainanalysis %>% 
  filter(DIAG == c("DD35", "DD23", "DD22", "DD13", "DD05", "DD03", "DC78", "DC77", "DC51", "DC50", "DC44", "DC43", "DC34", "DC16"))

test <- diags_mainanalysis %>% 
  filter(DIAG == "DC50")

####### ------------ Loop for all diseases ------------ ######## 
library(Epi)
library(popEpi)
library(speedglm)


dispersion <- function(sample) {
  
  # Connect diag data to population data
  EligiblePop <- left_join(Population, sample, by = "PERSON_ID") %>% 
    mutate(prior = D_INDDTO < STUDY_ENTRANCE,
           prior = ifelse(is.na(prior), FALSE, prior)) %>% 
    filter(prior == FALSE | is.na(prior)) %>% 
    mutate(check = D_INDDTO <= STUDY_END,
           EVENT_END = ifelse(check == FALSE | is.na(check), EVENT_END, "Diag"),
           STUDY_END = ifelse(EVENT_END == "Diag", D_INDDTO, STUDY_END),
           STUDY_END = ifelse(STUDY_ENTRANCE == STUDY_END, STUDY_END + 0.001368925, STUDY_END),
           event = ifelse(EVENT_END == "Diag", 1, 0)) %>% 
    dplyr::select(PERSON_ID, BIRTHDAY, STUDY_ENTRANCE, STUDY_END, EVENT_START, EVENT_END, AGE, KOEN, UDD, BIRTH_COHORT, INCOMEQ, event) 
  
  
  split <- lexpand(EligiblePop, birth = BIRTHDAY, entry = STUDY_ENTRANCE, exit = STUDY_END,
                   status = event, entry.status = EVENT_START, breaks = list(age = c(29:103), per = c(2012:2023)))
  
  split_udd <- split %>% 
    mutate(event = ifelse(lex.Xst == "1", 1,0),
           AGE = floor(age),
           AGE = ifelse(AGE == 29, 30, AGE),
           PERIOD = floor(per),
           PERIOD = ifelse(PERIOD == 2012, 2013, PERIOD)) %>% 
    group_by(AGE, UDD, KOEN, BIRTH_COHORT) %>% 
    summarise(lex.dur = sum(lex.dur),
              event = sum(event))
  
  
  split_income <- split %>% 
    filter(!is.na(INCOMEQ)) %>% 
    mutate(event = ifelse(lex.Xst == 1, 1,0),
           AGE = floor(age),
           AGE = ifelse(AGE == 29, 30, AGE),
           PERIOD = floor(per),
           PERIOD = ifelse(PERIOD == 2012, 2013, PERIOD),
           BIRTH_COHORT = floor(BIRTHDAY)) %>% 
    group_by(AGE, INCOMEQ, KOEN, BIRTH_COHORT) %>% 
    summarise(lex.dur = sum(lex.dur),
              event = sum(event))
  
    
    
    # Estimating the model
  
  poisson.model.rate_udd1 <- speedglm(event ~ offset(log(lex.dur)) + as.factor(AGE)+ as.factor(BIRTH_COHORT), data = split_udd[split_udd$UDD == 1 & split_udd$KOEN == 2,], family = poisson(link = "log"))
  poisson.model.rate_udd2 <- speedglm(event ~ offset(log(lex.dur)) + as.factor(AGE)+ as.factor(BIRTH_COHORT), data = split_udd[split_udd$UDD == 2 & split_udd$KOEN == 2,], family = poisson(link = "log"))
  poisson.model.rate_udd3 <- speedglm(event ~ offset(log(lex.dur)) + as.factor(AGE)+ as.factor(BIRTH_COHORT), data = split_udd[split_udd$UDD == 3 & split_udd$KOEN == 2,], family = poisson(link = "log"))
  
    
    # results
    results <- data.frame(DISPERSION_udd1 = summary(poisson.model.rate_udd1)$dispersion,
             DISPERSION_udd2 = summary(poisson.model.rate_udd2)$dispersion,
             DISPERSION_udd3 = summary(poisson.model.rate_udd3)$dispersion)
    
    return (results)
    
  }
  
  
}

cluster <- multidplyr::new_cluster(5)
gc()
multidplyr::cluster_library(
  cluster, 
  c("tidyverse", "lubridate", "Epi", "popEpi","speedglm")
)
multidplyr::cluster_copy(cluster, c("dispersion", "Population"))


dispersion <- DIAG_LIST %>%
  group_by(DIAG) %>%
  multidplyr::partition(cluster) %>%
  summarize(
    result = dispersion(cur_data())
  ) %>%
  collect()





