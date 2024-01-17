library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

##############################################################################
# The following script calculates N and number of cases per disease analysis #
##############################################################################

## Adgang til Incidence af alt
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")


####### ------------ Access the data ------------ ######## 

diags_all_level3 <- dbReadTable(conn = conn_inci, name = "diags_all_level3")


Population <- dbReadTable(conn = conn_inci, name = "Population_2013_2022")


library(Epi)
Population <- Population %>% 
  mutate(STUDY_ENTRANCE = cal.yr(STUDY_ENTRANCE),
         STUDY_END = cal.yr(STUDY_END),
         BIRTHDAY = cal.yr(BIRTHDAY))

diags_all_level3 <- diags_all_level3 %>% 
  mutate(D_INDDTO = cal.yr(as.Date(D_INDDTO)))


DC50 <- diags_all_level3 %>% 
  filter(DIAG == "DC50")

####### ------------ Loop for all diseases ------------ ######## 


get_N <- function(sample) {
  
  # Connect diag data to population data
  EligiblePop_edu <- left_join(Population, sample, by = "PERSON_ID") %>% 
    mutate(prior = D_INDDTO < STUDY_ENTRANCE,
           prior = ifelse(is.na(prior), FALSE, prior)) %>% 
    filter(prior == FALSE | is.na(prior)) %>% 
    mutate(check = D_INDDTO <= STUDY_END,
           EVENT_END = ifelse(check == FALSE | is.na(check), EVENT_END, "Diag"),
           STUDY_END = ifelse(EVENT_END == "Diag", D_INDDTO, STUDY_END),
           STUDY_END = ifelse(STUDY_ENTRANCE == STUDY_END, STUDY_END + 0.001368925, STUDY_END),
           event = ifelse(EVENT_END == "Diag", 1, 0),
           PY = STUDY_END-STUDY_ENTRANCE) %>% 
    dplyr::select(PERSON_ID, BIRTHDAY, STUDY_ENTRANCE, STUDY_END, EVENT_START, EVENT_END, AGE, KOEN, UDD, BIRTH_COHORT, INCOMEQ, event,PY) 
  
  female_edu <- EligiblePop_edu %>% 
    filter(KOEN == 2) 
  
  male_edu <- EligiblePop_edu %>% 
    filter(KOEN == 1) 
  
  EligiblePop_inc <-  EligiblePop_edu %>% 
    filter(!is.na(INCOMEQ))
  
  female_inc <- EligiblePop_inc %>% 
    filter(KOEN == 2) 
  
  male_inc <- EligiblePop_inc %>% 
    filter(KOEN == 1) 
  
  
  # Get definition on N and number of cases
  N <- data.frame(N_edu = nrow(EligiblePop_edu),
                  PY_edu = sum(EligiblePop_edu$PY),
                  CASES_edu = sum(EligiblePop_edu$event),
                  FEMALE_edu_N = nrow(female_edu),
                  FEMALE_edu_cases = sum(female_edu$event),
                  FEMALE_edu_PY = sum(female_edu$PY),
                  MALE_edu_N = nrow(male_edu),
                  MALE_edu_cases = sum(male_edu$event),
                  MALE_edu_PY = sum(male_edu$PY),
                  N_inc = nrow(EligiblePop_inc),
                  PY_inc = sum(EligiblePop_inc$PY),
                  CASES_inc = sum(EligiblePop_inc$event),
                  FEMALE_inc_N = nrow(female_inc),
                  FEMALE_inc_cases = sum(female_inc$event),
                  FEMALE_inc_PY = sum(female_edu$PY),
                  MALE_inc_N = nrow(female_inc),
                  MALE_inc_cases = sum(male_inc$event),
                  MALE_inc_PY = sum(male_inc$PY)) 
    
    return (N)
    
  }
  
  

cluster <- multidplyr::new_cluster(4)
gc()
multidplyr::cluster_library(
  cluster, 
  c("tidyverse", "lubridate")
)
multidplyr::cluster_copy(cluster, c("get_N", "Population"))


N <- diags_all_level3 %>%
  group_by(DIAG) %>%
  multidplyr::partition(cluster) %>%
  summarize(
    result = get_N(cur_data())
  ) %>%
  collect()

N <- N %>% 
  mutate(N_edu = result$N_edu,
         PY_edu = result$PY_edu,
         CASES_edu = result$CASES_edu,
         FEMALE_edu_N = result$FEMALE_edu_N,
         FEMALE_edu_cases = result$FEMALE_edu_cases,
         FEMALE_edu_PY = result$FEMALE_edu_PY,
         MALE_edu_N = result$MALE_edu_N,
         MALE_edu_cases = result$MALE_edu_cases,
         MALE_edu_PY = result$MALE_edu_PY,
         N_inc = result$N_inc,
         PY_inc = result$PY_inc,
         CASES_inc = result$CASES_inc,
         FEMALE_inc_N = result$FEMALE_inc_N,
         FEMALE_inc_cases = result$FEMALE_inc_cases,
         FEMALE_inc_PY = result$FEMALE_inc_PY,
         MALE_inc_N = result$MALE_inc_N,
         MALE_inc_cases = result$MALE_inc_cases,
         MALE_inc_PY = result$MALE_inc_PY,) %>% 
  dplyr::select(DIAG, N_edu, PY_edu, CASES_edu, FEMALE_edu_N, FEMALE_edu_PY, FEMALE_edu_cases, 
                MALE_edu_N, MALE_edu_PY, MALE_edu_cases, 
                N_inc, CASES_inc, PY_inc, FEMALE_inc_N, FEMALE_inc_PY, FEMALE_inc_cases, 
                MALE_inc_N, MALE_inc_PY, MALE_inc_cases)
  


test <- N %>% 
  filter(CASES >= 100)

##### Select only disease with cases > 100
diags_mainanalysis <- N %>% 
  filter(CASES >= 100) %>% 
  left_join(diags_all_level3, by = "DIAG")%>% 
  dplyr::select(DIAG, PERSON_ID, D_INDDTO)




###### Save data

conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")

dbWriteTable(conn_inci, "N_CASES_mainanalysis", N, overwrite = TRUE)
#dbWriteTable(conn_inci, "diags_mainanalysis", diags_mainanalysis, overwrite = TRUE)



