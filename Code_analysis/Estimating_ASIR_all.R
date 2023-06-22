library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 
library(DSTformat)

##############################################################################
# The following script calculates age standardised incidence rates for all   #
# diseases.                                                                  #
##############################################################################

## Adgang til Incidence af alt
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")


####### ------------ Access the data ------------ ######## 

diags_all_level3 <- dbReadTable(conn = conn_inci, name = "diags_all_level3")


Population_2012_2021 <- dbReadTable(conn = conn_inci, name = "Population_2012_2021")
Population_2012_2021 <- Population_2012_2021 %>% 
  mutate(BIRTHDAY = as.Date(ymd_hms(BIRTHDAY)),
         STUDY_ENTRANCE = as.Date(ymd_hms(STUDY_ENTRANCE)),
         STUDY_EXIT = as.Date(ymd_hms(STUDY_EXIT))) 


####### ------------ Loop for all diseases ------------ ######## 
library(Epi)
library(popEpi)
library(speedglm)



# European standard population 2013 (age groups from "30-34 years" to "90+ years")
sp_prop <- c(0.098, 0.106, 0.106, 0.106, 0.106, 
                 0.098, 0.090, 0.083, 0.075, 0.060, 0.038, 0.022, 0.012) 


get_incidence_rate <- function(sample) {
  
  # Connect diag data to population data
  EligiblePop <- left_join(Population_2012_2021, sample, by = "PERSON_ID") %>% 
    mutate(prior = D_INDDTO < STUDY_ENTRANCE,
           prior = ifelse(is.na(prior), FALSE, prior)) %>% 
    filter(prior == FALSE | is.na(prior)) %>% 
    mutate(check = D_INDDTO <= STUDY_EXIT,
           EVENT_END = ifelse(check == FALSE | is.na(check), EVENT_END, "Diag"),
           STUDY_EXIT = ifelse(EVENT_END == "Diag", format(as.Date(D_INDDTO)), format(as.Date(STUDY_EXIT)))) %>% 
    dplyr::select(PERSON_ID, BIRTHDAY, STUDY_ENTRANCE, STUDY_EXIT, EVENT_START, EVENT_END, AGE, KOEN, udd) %>% 
    mutate(STUDY_EXIT = as.POSIXct(STUDY_EXIT)) %>% 
    mutate(event = ifelse(EVENT_END == "Diag", 1, 0))
  
  # Do not conduct analysis with N cases < 100
  if(sum(EligiblePop$event) < 100){
    return(data.frame())
  }
  else{
    
    # Split data by age
    Lexi <- EligiblePop %>% 
      mutate(STUDY_ENTRANCE = cal.yr(STUDY_ENTRANCE),
             STUDY_EXIT = cal.yr(STUDY_EXIT),
             BIRTHDAY = cal.yr(BIRTHDAY),
             event = ifelse(EVENT_END == "Diag", 1, 0))
    
    split <- Lexis(entry = list(per = STUDY_ENTRANCE,
                                     age = STUDY_ENTRANCE - BIRTHDAY),
                        exit = list(per =  STUDY_EXIT,
                                    age = STUDY_EXIT - BIRTHDAY),
                        exit.status = factor(event, labels = c("at_risk", "diagnosed")),
                        data = Lexi,
                        tol = -1) # Set at -1 to get all cases.
    
    split <- split %>% 
      mutate(lex.dur = ifelse(lex.dur == 0, 0.001368925, lex.dur)) # As obs w lex.dur == 0 will be excluded, their lex.dur is set to 0.5 day (=0.001368925 person-year)
    
    split <- splitLexisDT(split,
                               breaks = seq(30, 95, 5),
                               timeScale = "age") 
    
    split <- split %>% 
      mutate(AGE = floor(age),
             AGE = ifelse(AGE == 29, 30, AGE),
             AGE_CAT = cut(AGE, breaks = c(-Inf, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                           labels = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                      "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+")),
             event = ifelse(lex.Xst == "diagnosed", 1, 0),
             cohort = as.numeric(cut(floor(BIRTHDAY), breaks = c(-Inf, 1929, 1939, 1949, 1959, 1969, 1979, Inf),
                                     labels = c(1, 2, 3, 4, 5, 6, 7))))
    
    
    # Estimating the model
    poisson.model.rate <- speedglm(event ~ offset(log(lex.dur)) + as.factor(AGE_CAT)*(as.factor(udd)) + KOEN + cohort, data = split, family = poisson(link = "log"))
    
    df_predict <- data.frame(udd = rep(c(1,2,3), each = 91), 
                             AGE_CAT = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                             KOEN = mean(split$KOEN),
                             cohort = rep(c(1,2,3,4,5, 6, 7), each = 13),
                             lex.dur = 1)
    
    # Predicting
    predict <- predict(poisson.model.rate, df_predict, type = "response")
    
    # Results
    results <- df_predict %>% 
      mutate(predict = predict) %>% 
      group_by(udd,AGE_CAT) %>% 
      summarise(predict_average = sum(predict)/7*100000) %>% 
      mutate(sp = sp_prop,
             standardised = predict_average * sp) %>% 
      group_by(udd) %>% 
      summarise(ASIR = sum(standardised))
    
    
    return (results)
    
  }
  
  
}

cluster <- multidplyr::new_cluster(15)
gc()
multidplyr::cluster_library(
  cluster, 
  c("tidyverse", "lubridate", "Epi", "popEpi","speedglm")
)
multidplyr::cluster_copy(cluster, c("get_incidence_rate", "Population_2012_2021", "sp_prop"))


incidence <- diags_all_level3 %>%
  group_by(DIAG) %>%
  multidplyr::partition(cluster) %>%
  summarize(
    result = get_incidence_rate(cur_data())
  ) %>%
  collect()


ASIR_all <- data.frame(incidence) %>%
  mutate(udd = result$udd,
         ASIR = result$ASIR)

ASIR_all <- within(ASIR_all, rm(result))

dbWriteTable(conn_inci, "ASIR_all", ASIR_all, overwrite = TRUE) 

