library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
library(Epi)
library(popEpi)
library(mgcv)
library(dtplyr)
library(doSNOW)
#devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

##############################################################################
# The following script plots age and period specific incidence rates on a    #
# Lexis surface. Incidence rates are estimating using GAM and estimating a   #
# tensor product (te). Age standardisation is applied.                       #
# Comparing observed incidence rates with estimated incidence rates.         #
##############################################################################

## Adgang til Incidence af alt
conn_inci <- OraGenvej(dbname = "STATUDV")
diags_mainanalysis <- dbReadTable(conn = conn_inci, schema= "D222107", name = "diags_mainanalysis")

Population <- dbReadTable(conn = conn_inci, schema= "D222107", name = "Population_2013_2022")

initial_seed = 442

######## ------------ Defining standard population ------------- ###########

# European standard population 2013 (age groups from "30-34 years" to "90+ years")

sp_prop <- data.frame(AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                               "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"),
                       sp = c(0.098, 0.106, 0.106, 0.106, 0.106, 0.098, 0.090, 0.083, 0.075, 
                              0.060, 0.038, 0.022, 0.012)) 


######## ------------ Estimating incidence rates for breast cancer ------------- ###########


Population <- Population %>% 
  mutate(STUDY_ENTRANCE = cal.yr(STUDY_ENTRANCE),
         STUDY_END = cal.yr(STUDY_END),
         BIRTHDAY = cal.yr(BIRTHDAY))

diags_mainanalysis <- diags_mainanalysis %>% 
  mutate(D_INDDTO = cal.yr(as.Date(D_INDDTO)))

DI10 <- diags_mainanalysis %>% 
  filter(DIAG == "DI10")
DC44 <- diags_mainanalysis %>% 
  filter(DIAG == "DC44")
DO98 <- diags_mainanalysis %>% 
  filter(DIAG == "DO98")
DR97 <- diags_mainanalysis %>% 
  filter(DIAG == "DR97")



inference <- function(Population,bootstrapindices,diags,bootEDU=FALSE,bootINC= FALSE, return_names=FALSE){
  print("running inference function")
  
  #hundreddiags <- head(diags_mainanalysis %>% group_by(DIAG) %>% count(),100) %>% dplyr::select(c("DIAG"))
  #diags_mainanalysis <- filter(diags_mainanalysis, DIAG %in% hundreddiags$DIAG)
  
  ## Try to avoid OOM (clunkily) by randomly offsetting execution of most memory heavy block of code (next couple of lines until removal of split and eligible pop.))
  Sys.sleep(runif(n=1,min=0,max=40)) #sleep between 0 and 1 seconds
  
  
  get_incidence_rate <- function(sample, Population) {
    
    ######## First prepare data ########
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
      dplyr::select(PERSON_ID, BIRTHDAY, STUDY_ENTRANCE, STUDY_END, EVENT_START, AGE, KOEN, UDD, BIRTH_COHORT, INCOMEQ, event)  %>%
      collect()
    
    split <- lexpand(EligiblePop, birth = BIRTHDAY, entry = STUDY_ENTRANCE, exit = STUDY_END,
                     status = event, entry.status = EVENT_START, breaks = list(age = seq(30, 95, 5)))
    
    split_udd <- lazy_dt(split) %>% 
      mutate(event = ifelse(lex.Xst == "1", 1,0),
             AGE = floor(age),
             AGE = cut(AGE, breaks = c(-Inf, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                           labels = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                      "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"))) %>% 
      group_by(AGE, UDD, KOEN, BIRTH_COHORT) %>% 
      summarise(lex.dur = sum(lex.dur),
                event = sum(event)) %>% collect()
    
    
    split_income <- lazy_dt(split) %>% 
      filter(!is.na(INCOMEQ)) %>% 
      mutate(event = ifelse(lex.Xst == 1, 1,0),
             AGE = floor(age),
             AGE = cut(AGE, breaks = c(-Inf, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                       labels = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                  "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"))) %>% 
      group_by(AGE, INCOMEQ, KOEN, BIRTH_COHORT) %>% 
      summarise(lex.dur = sum(lex.dur),
                event = sum(event)) %>% collect()
    
    udd_big_enough_for_reg <- (nrow(filter(EligiblePop,event==1))>=150)
    udd_men_big_enough_for_reg <- (nrow(filter(EligiblePop,event==1 & KOEN==1))>=3*50)
    udd_women_big_enough_for_reg <- (nrow(filter(EligiblePop,event==1 & KOEN==2))>=3*50)
    
    inc_big_enough_for_reg <- (nrow(filter(EligiblePop,event==1))>=200)
    inc_men_big_enough_for_reg <- (nrow(filter(EligiblePop,event==1 & KOEN==1))>=2*4*50)
    inc_women_big_enough_for_reg <- (nrow(filter(EligiblePop,event==1 & KOEN==2))>=2*4*50)
    
    print(paste(udd_big_enough_for_reg,udd_men_big_enough_for_reg, udd_women_big_enough_for_reg, inc_big_enough_for_reg, inc_men_big_enough_for_reg, inc_women_big_enough_for_reg))
    
    rm(EligiblePop, split)
    gc()
    
    NA_tibble_returner <- function(isUDD=TRUE,KOEN=1.5, analysis="Education"){
      #returns a NA-tibble in the same way e.g. results_udd is given (where all numeric, non factors (so not age, period, koen) are replaced with NA)
      # this is done to easier aggregate all tables in the end -irrespective of whether a regression had enough observations to be run.
      
      
      if(isUDD){
        NA_tibble <- expand_grid(UDD=1:3, KOEN={{KOEN}}) %>% 
          mutate(SEP=str_c("udd",UDD)) %>% 
          dplyr::select(-c(UDD)) %>% 
          mutate(ASIR = NA, 
                 RR_type = case_when(SEP=="udd1"~"RR1vs3",SEP=="udd2"~"RR2vs3",SEP=="udd3"~"reference"), 
                 RR=NA, analysis={{analysis}}) %>% 
          relocate(SEP, .after = last_col())
      }else{
        NA_tibble <- expand_grid(INCOMEQ=1:4, KOEN={{KOEN}}) %>% 
          mutate(SEP=str_c("Q",INCOMEQ)) %>% 
          dplyr::select(-c(INCOMEQ)) %>% 
          mutate(ASIR = NA, 
                 RR_type = case_when(SEP=="Q1"~"RR1vs4",SEP=="Q2"~"RR2vs4",SEP=="Q3" ~"RR3vs4",SEP=="Q4"~"reference"), 
                 RR=NA, analysis={{analysis}}) %>% 
          relocate(SEP, .after = last_col())
      }
      
      return(NA_tibble)
    }
    
    ##### Run model ----- stratified by UDD
    if(udd_big_enough_for_reg){
      ### GAM with tensor product 
      modeludd1 <- gam(event ~ offset(log(lex.dur)) + AGE + KOEN + BIRTH_COHORT, data = split_udd[split_udd$UDD == 1,], family = "poisson")
      modeludd2 <- gam(event ~ offset(log(lex.dur)) + AGE + KOEN + BIRTH_COHORT, data = split_udd[split_udd$UDD == 2,], family = "poisson")
      modeludd3 <- gam(event ~ offset(log(lex.dur)) + AGE + KOEN + BIRTH_COHORT, data = split_udd[split_udd$UDD == 3,], family = "poisson")
      
      
      ### Predict udd1
      df_predict_udd1 <- data.frame(UDD = 1, 
                               AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                               KOEN = 1.5,
                               BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                               lex.dur = 1)
      
      
      predict_udd1 <- predict(modeludd1, df_predict_udd1, type = "response")
      
      grid_data_udd1 <- cbind(df_predict_udd1, predictions = predict_udd1) 
      
      
      ### Predict udd2
      df_predict_udd2 <- data.frame(UDD = 2, 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1.5,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_udd2 <- predict(modeludd2, df_predict_udd2, type = "response")
      
      grid_data_udd2 <- cbind(df_predict_udd2, predictions = predict_udd2)
      
      
      ### Predict udd3
      df_predict_udd3 <- data.frame(UDD = 3, 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1.5,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_udd3 <- predict(modeludd3, df_predict_udd3, type = "response")
      
      grid_data_udd3 <- cbind(df_predict_udd3, predictions = predict_udd3) 
      
      
      ### Gather results and get rates + ratios
      results_udd <- rbind(grid_data_udd1, grid_data_udd2, grid_data_udd3) %>% 
        group_by(UDD, AGE) %>% 
        summarise(predict_average = mean(predictions)) %>% 
        left_join(sp_prop, by = "AGE") %>% 
        mutate(standardised = predict_average * sp,
               UDD = recode(UDD,
                            "1" = "udd1",
                            "2" = "udd2",
                            "3" = "udd3")) %>% 
        group_by(UDD) %>% 
        summarise(ASIR = sum(standardised)*100000) %>% 
        pivot_wider(names_from = UDD, values_from = ASIR) %>% 
        mutate(RR1vs3 = udd1/udd3,
               RR2vs3 = udd2/udd3) %>% 
        pivot_longer(1:3, names_to = "UDD", values_to = "ASIR") %>% 
        pivot_longer(1:2, names_to = "RR_type", values_to = "RR") %>% 
        mutate(RR = ifelse(UDD == "udd3", "reference", RR),
               RR_type = ifelse(UDD == "udd3", "reference", RR_type),
               RR = ifelse(UDD == "udd1" & RR_type == "RR2vs3", "No match", RR),
               RR = ifelse(UDD == "udd2" & RR_type == "RR1vs3", "No match", RR)) %>% 
        filter(RR != "No match") %>% 
        unique() %>% 
        mutate(analysis = "Education",
               SEP = UDD,
               KOEN = 1.5) %>% 
        dplyr::select(-c(UDD), KOEN, ASIR, RR_type, RR, analysis, SEP) 

      rm(modeludd1, modeludd2, modeludd3, df_predict_udd1, df_predict_udd2, df_predict_udd3, 
         predict_udd1, predict_udd2, predict_udd3, grid_data_udd1, grid_data_udd2, grid_data_udd3)
      
    }else{
      print("else uddannelse")
      results_udd <- NA_tibble_returner(isUDD=TRUE, KOEN=1.5,analysis = "Education")}
    ##### Run model ----- stratified by UDD & KOEN
    
    if(udd_men_big_enough_for_reg){ 
      ### GAM with tensor product 
      ##### PREDICTING MALE EDUCATION
      modelu1k1 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_udd[split_udd$UDD == 1 & split_udd$KOEN == 1,], family = "poisson")
      modelu2k1 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_udd[split_udd$UDD == 2 & split_udd$KOEN == 1,], family = "poisson")
      modelu3k1 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_udd[split_udd$UDD == 3 & split_udd$KOEN == 1,], family = "poisson")
      
      ### Predict udd1 and koen1
      df_predict_u1k1 <- data.frame(UDD = 1, 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      
      predict_u1k1 <- predict(modelu1k1, df_predict_u1k1, type = "response")
      
      grid_data_u1k1 <- cbind(df_predict_u1k1, predictions = predict_u1k1) 
      
      
      
      ### Predict udd2 and koen1
      df_predict_u2k1 <- data.frame(UDD = 2, 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      
      predict_u2k1 <- predict(modelu2k1, df_predict_u2k1, type = "response")
      
      grid_data_u2k1 <- cbind(df_predict_u2k1, predictions = predict_u2k1) 
      
      
      
      ### Predict udd3 and koen1
      df_predict_u3k1 <- data.frame(UDD = 3, 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      
      predict_u3k1 <- predict(modelu3k1, df_predict_u3k1, type = "response")
      
      grid_data_u3k1 <- cbind(df_predict_u3k1, predictions = predict_u3k1) 
      
      
    }
    if(udd_women_big_enough_for_reg){
      ##### PREDICTING FEMALE EDUCATION
      
      modelu1k2 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_udd[split_udd$UDD == 1 & split_udd$KOEN == 2,], family = "poisson")
      modelu2k2 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_udd[split_udd$UDD == 2 & split_udd$KOEN == 2,], family = "poisson")
      modelu3k2 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_udd[split_udd$UDD == 3 & split_udd$KOEN == 2,], family = "poisson")
      
      ### Predict udd1 and koen2
      df_predict_u1k2 <- data.frame(UDD = 1, 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 2,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      
      predict_u1k2 <- predict(modelu1k2, df_predict_u1k2, type = "response")
      
      grid_data_u1k2 <- cbind(df_predict_u1k2, predictions = predict_u1k2) 
      
      
      
      ### Predict udd2 and koen2
      df_predict_u2k2 <- data.frame(UDD = 2, 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 2,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      
      predict_u2k2 <- predict(modelu2k2, df_predict_u2k2, type = "response")
      
      grid_data_u2k2 <- cbind(df_predict_u2k2, predictions = predict_u2k2) 
      
      
      
      ### Predict udd3 and koen2
      df_predict_u3k2 <- data.frame(UDD = 3, 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 2,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      
      predict_u3k2 <- predict(modelu3k2, df_predict_u3k2, type = "response")
      
      grid_data_u3k2 <- cbind(df_predict_u3k2, predictions = predict_u3k2) 

      
      ### Gather results
    }
    
    if(udd_men_big_enough_for_reg & udd_women_big_enough_for_reg){
      results_udd_koen <- rbind(grid_data_u1k1, grid_data_u2k1, grid_data_u3k1, grid_data_u1k2, grid_data_u2k2,  grid_data_u3k2)
    }else if(udd_men_big_enough_for_reg){
      results_udd_koen <- rbind(grid_data_u1k1, grid_data_u2k1, grid_data_u3k1)
    }else if(udd_women_big_enough_for_reg){
      results_udd_koen <- rbind(grid_data_u1k2, grid_data_u2k2, grid_data_u3k2)
    }
    
    if(udd_men_big_enough_for_reg | udd_women_big_enough_for_reg){
      ### Get rates by model 
      print("Hi")
      print(results_udd_koen)
      results_udd_koen <- results_udd_koen %>% 
        group_by(UDD, AGE, KOEN) %>% 
        summarise(predict_average = mean(predictions)) %>% 
        left_join(sp_prop, by = "AGE") %>% 
        mutate(standardised = predict_average * sp) %>% 
        ungroup() %>% 
        group_by(UDD, KOEN) %>% 
        summarise(ASIR = sum(standardised)*100000) %>% 
        mutate(UDD = recode(UDD,
                            "1" = "udd1",
                            "2" = "udd2",
                            "3" = "udd3")) %>% 
        pivot_wider(names_from = UDD, values_from = ASIR) %>% 
        mutate(RR1vs3 = udd1/udd3,
               RR2vs3 = udd2/udd3) %>% 
        pivot_longer(2:4, names_to = "UDD", values_to = "ASIR") %>% 
        pivot_longer(2:3, names_to = "RR_type", values_to = "RR") %>% 
        mutate(RR = ifelse(UDD == "udd3", "reference", RR),
               RR_type = ifelse(UDD == "udd3", "reference", RR_type),
               RR = ifelse(UDD == "udd1" & RR_type == "RR2vs3", "No match", RR),
               RR = ifelse(UDD == "udd2" & RR_type == "RR1vs3", "No match", RR)) %>% 
        filter(RR != "No match") %>% 
        unique() %>% 
        mutate(analysis = "Education and sex",
               SEP = UDD) %>% 
        dplyr::select(-c(UDD)) 
      
      print("results")
      print(results_udd_koen)
      
      rm(modelu1k1, modelu1k2, modelu2k1, modelu2k2, modelu3k1, modelu3k2, 
         df_predict_u1k1, df_predict_u1k2, df_predict_u2k1, df_predict_u2k2, df_predict_u3k1, df_predict_u3k2,
         predict_u1k1, predict_u1k2, predict_u2k1, predict_u2k2, predict_u3k1, predict_u3k2, 
         grid_data_u1k1, grid_data_u1k2, grid_data_u2k1, grid_data_u2k2, grid_data_u3k1, grid_data_u3k2, split_udd)
      
      if(udd_men_big_enough_for_reg & !udd_women_big_enough_for_reg){#create NA tibble of other sex
        results_udd_koen <- rbind(results_udd_koen,NA_tibble_returner(isUDD=TRUE, KOEN=2,analysis = "Education and sex"))
      }
      if(udd_women_big_enough_for_reg & !udd_men_big_enough_for_reg){#create NA tibble of other sex
        NA_tibble_returner(isUDD=TRUE, KOEN=1,analysis = "Education and sex")
        print(NA_tibble_returner(isUDD=TRUE, KOEN=1,analysis = "Education and sex"))
        print(colnames(NA_tibble_returner))
        print(colnames(results_udd_koen))
        results_udd_koen <- rbind(results_udd_koen,NA_tibble_returner(isUDD=TRUE, KOEN=1,analysis = "Education and sex"))
      }
      
    }else{
      results_udd_koen <- NA_tibble_returner(isUDD=TRUE, KOEN=1:2,analysis = "Education and sex")}
    
    
    ##### Run model ----- stratified by income
    
    if(inc_big_enough_for_reg){ 
      
      ### GAM with tensor product 
      modelQ1 <- gam(event ~ offset(log(lex.dur)) + AGE + KOEN + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q1",], family = "poisson")
      modelQ2 <- gam(event ~ offset(log(lex.dur)) + AGE + KOEN + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q2",], family = "poisson")
      modelQ3 <- gam(event ~ offset(log(lex.dur)) + AGE + KOEN + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q3",], family = "poisson")
      modelQ4 <- gam(event ~ offset(log(lex.dur)) + AGE + KOEN + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q4",], family = "poisson")
      
      
      ### Predict income1
      df_predict_Q1 <- data.frame(INCOMEQ = "Q1", 
                                  AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                  KOEN = 1.5,
                                  BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                  lex.dur = 1)
      
      predict_Q1 <- predict(modelQ1, df_predict_Q1, type = "response")
      
      grid_data_Q1 <- cbind(df_predict_Q1, predictions = predict_Q1) 
      
      
      ### Predict income2
      df_predict_Q2 <- data.frame(INCOMEQ = "Q2", 
                                  AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                  KOEN = 1.5,
                                  BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                  lex.dur = 1)
      
      predict_Q2 <- predict(modelQ2, df_predict_Q2, type = "response")
      
      grid_data_Q2 <- cbind(df_predict_Q2, predictions = predict_Q2) 
      
      
      ### Predict income3
      df_predict_Q3 <- data.frame(INCOMEQ = "Q3", 
                                  AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                  KOEN = 1.5,
                                  BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                  lex.dur = 1)
      
      predict_Q3 <- predict(modelQ3, df_predict_Q3, type = "response")
      
      grid_data_Q3 <- cbind(df_predict_Q3, predictions = predict_Q3) 
      
      
      ### Predict income4
      df_predict_Q4 <- data.frame(INCOMEQ = "Q4", 
                                  AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                  KOEN = 1.5,
                                  BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                  lex.dur = 1)
      
      predict_Q4 <- predict(modelQ4, df_predict_Q4, type = "response")
      
      grid_data_Q4 <- cbind(df_predict_Q4, predictions = predict_Q4) 
      
      
      ### Gather results
      results_income <- rbind(grid_data_Q1, grid_data_Q2, grid_data_Q3, grid_data_Q4) %>% 
        group_by(INCOMEQ, AGE) %>% 
        summarise(predict_average = mean(predictions)) %>% 
        left_join(sp_prop, by = "AGE") %>% 
        mutate(standardised = predict_average * sp) %>% 
        group_by(INCOMEQ) %>% 
        summarise(ASIR = sum(standardised)*100000)%>% 
        pivot_wider(names_from = INCOMEQ, values_from = ASIR) %>% 
        mutate(RR1vs4 = Q1/Q4,
               RR2vs4 = Q2/Q4,
               RR3vs4 = Q3/Q4) %>% 
        pivot_longer(1:4, names_to = "INCOMEQ", values_to = "ASIR") %>% 
        pivot_longer(1:3, names_to = "RR_type", values_to = "RR") %>% 
        mutate(RR = ifelse(INCOMEQ == "Q4", "reference", RR),
               RR_type = ifelse(INCOMEQ == "Q4", "reference", RR_type),
               RR = ifelse(INCOMEQ == "Q1" & RR_type == "RR2vs4" | INCOMEQ == "Q1" & RR_type == "RR3vs4", "No match", RR),
               RR = ifelse(INCOMEQ == "Q2" & RR_type == "RR1vs4" | INCOMEQ == "Q2" & RR_type == "RR3vs4", "No match", RR),
               RR = ifelse(INCOMEQ == "Q3" & RR_type == "RR1vs4" | INCOMEQ == "Q3" & RR_type == "RR2vs4", "No match", RR)) %>% 
        filter(RR != "No match") %>% 
        unique() %>% 
        mutate(analysis = "Income",
               SEP = INCOMEQ,
               KOEN = 1.5) %>% 
        dplyr::select(-c(INCOMEQ), KOEN, ASIR, RR_type, RR, analysis, SEP) 
      
      rm(modelQ1, modelQ2, modelQ3, modelQ4,
         df_predict_Q1, df_predict_Q2,df_predict_Q3, df_predict_Q4, 
         predict_Q1, predict_Q2, predict_Q3, predict_Q4,
         grid_data_Q1, grid_data_Q2, grid_data_Q3, grid_data_Q4)
      
    }else{
      results_income <- NA_tibble_returner(isUDD=FALSE, KOEN=1.5,analysis = "Income")}
    
    ##### Run model ----- stratified by INCOMEQ & KOEN
    
    if(inc_men_big_enough_for_reg){ 
      ### GAM with tensor product 
      modelQ1k1 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q1" & split_income$KOEN == 1,], family = "poisson")
      modelQ2k1 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q2" & split_income$KOEN == 1,], family = "poisson")
      modelQ3k1 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q3" & split_income$KOEN == 1,], family = "poisson")
      modelQ4k1 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q4" & split_income$KOEN == 1,], family = "poisson")
      
      
      ##### PREDICTING MALE EDUCATION
      
      ### Predict Q1 and koen1
      df_predict_Q1k1 <- data.frame(INCOMEQ = "Q1", 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_Q1k1 <- predict(modelQ1k1, df_predict_Q1k1, type = "response")
      
      grid_data_Q1k1 <- cbind(df_predict_Q1k1, predictions = predict_Q1k1) 
      
      
      ### Predict Q2 and koen1
      df_predict_Q2k1 <- data.frame(INCOMEQ = "Q2", 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_Q2k1 <- predict(modelQ2k1, df_predict_Q2k1, type = "response")
      
      grid_data_Q2k1 <- cbind(df_predict_Q2k1, predictions = predict_Q2k1) 
      
      
      ### Predict Q3 and koen1
      df_predict_Q3k1 <- data.frame(INCOMEQ = "Q3", 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_Q3k1 <- predict(modelQ3k1, df_predict_Q3k1, type = "response")
      
      grid_data_Q3k1 <- cbind(df_predict_Q3k1, predictions = predict_Q3k1) 
      
      
      ### Predict Q4 and koen1
      df_predict_Q4k1 <- data.frame(INCOMEQ = "Q4", 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 1,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_Q4k1 <- predict(modelQ4k1, df_predict_Q4k1, type = "response")
      
      grid_data_Q4k1 <- cbind(df_predict_Q4k1, predictions = predict_Q4k1) 
    }
    
    ##### PREDICTING FEMALE EDUCATION
    if(inc_women_big_enough_for_reg){
      modelQ1k2 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q1" & split_income$KOEN == 2,], family = "poisson")
      modelQ2k2 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q2" & split_income$KOEN == 2,], family = "poisson")
      modelQ3k2 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q3" & split_income$KOEN == 2,], family = "poisson")
      modelQ4k2 <- gam(event ~ offset(log(lex.dur)) + AGE + BIRTH_COHORT, data = split_income[split_income$INCOMEQ == "Q4" & split_income$KOEN == 2,], family = "poisson")
      
      ### Predict Q1 and koen2
      df_predict_Q1k2 <- data.frame(INCOMEQ = "Q1", 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 2,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_Q1k2 <- predict(modelQ1k2, df_predict_Q1k2, type = "response")
      
      grid_data_Q1k2 <- cbind(df_predict_Q1k2, predictions = predict_Q1k2) 
      
      
      ### Predict Q2 and koen2
      df_predict_Q2k2 <- data.frame(INCOMEQ = "Q2", 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 2,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_Q2k2 <- predict(modelQ2k2, df_predict_Q2k2, type = "response")
      
      grid_data_Q2k2 <- cbind(df_predict_Q2k2, predictions = predict_Q2k2) 
      
      
      ### Predict Q3 and koen2
      df_predict_Q3k2 <- data.frame(INCOMEQ = "Q3", 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 2,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_Q3k2 <- predict(modelQ3k2, df_predict_Q3k2, type = "response")
      
      grid_data_Q3k2 <- cbind(df_predict_Q3k2, predictions = predict_Q3k2) 
      
      
      ### Predict Q1 and koen2
      df_predict_Q4k2 <- data.frame(INCOMEQ = "Q4", 
                                    AGE = c("30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90+"), 
                                    KOEN = 2,
                                    BIRTH_COHORT = rep(c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992"), each = 13),
                                    lex.dur = 1)
      
      predict_Q4k2 <- predict(modelQ4k2, df_predict_Q4k2, type = "response")
      
      grid_data_Q4k2 <- cbind(df_predict_Q4k2, predictions = predict_Q4k2) 
      
    }
    
    
    ### Gather results
    if(inc_men_big_enough_for_reg & inc_women_big_enough_for_reg){
      results_income_koen <- rbind(grid_data_Q1k1, grid_data_Q2k1, grid_data_Q3k1, grid_data_Q4k1,
                                   grid_data_Q1k2, grid_data_Q2k2,  grid_data_Q3k2, grid_data_Q4k2)
    }else if(inc_men_big_enough_for_reg){
      results_income_koen<- rbind(grid_data_Q1k1, grid_data_Q2k1, grid_data_Q3k1, grid_data_Q4k1)
    }else if(inc_women_big_enough_for_reg){
      results_income_koen <- rbind(grid_data_Q1k2, grid_data_Q2k2, grid_data_Q3k2, grid_data_Q4k2)
    }
    
    if(inc_men_big_enough_for_reg | inc_women_big_enough_for_reg){
      
      
      
      
      ## Get rates from model
      results_income_koen <- results_income_koen %>% 
        group_by(INCOMEQ, AGE, KOEN) %>% 
        summarise(predict_average = mean(predictions)) %>% 
        left_join(sp_prop, by = "AGE") %>% 
        mutate(standardised = predict_average * sp) %>% 
        ungroup() %>% 
        group_by(INCOMEQ, KOEN) %>% 
        summarise(ASIR = sum(standardised)*100000) %>% 
        pivot_wider(names_from = INCOMEQ, values_from = ASIR) %>% 
        mutate(RR1vs4 = Q1/Q4,
               RR2vs4 = Q2/Q4,
               RR3vs4 = Q3/Q4) %>% 
        pivot_longer(2:5, names_to = "INCOMEQ", values_to = "ASIR") %>% 
        pivot_longer(2:4, names_to = "RR_type", values_to = "RR") %>% 
        mutate(RR = ifelse(INCOMEQ == "Q4", "reference", RR),
               RR_type = ifelse(INCOMEQ == "Q4", "reference", RR_type),
               RR = ifelse(INCOMEQ == "Q1" & RR_type == "RR2vs4" | INCOMEQ == "Q1" & RR_type == "RR3vs4", "No match", RR),
               RR = ifelse(INCOMEQ == "Q2" & RR_type == "RR1vs4" | INCOMEQ == "Q2" & RR_type == "RR3vs4", "No match", RR),
               RR = ifelse(INCOMEQ == "Q3" & RR_type == "RR1vs4" | INCOMEQ == "Q3" & RR_type == "RR2vs4", "No match", RR)) %>% 
        filter(RR != "No match") %>% 
        unique() %>% 
        mutate(analysis = "Income and sex",
               SEP = INCOMEQ) %>% 
        dplyr::select(-c(INCOMEQ))
      
      
      
      
      rm(modelQ1k2, modelQ2k1, modelQ2k2, modelQ3k1, modelQ3k2, modelQ4k1, modelQ4k2, 
         df_predict_Q1k2, df_predict_Q2k1, df_predict_Q2k2, df_predict_Q3k1, 
         df_predict_Q3k2, df_predict_Q4k1, df_predict_Q4k2, 
         predict_Q1k2, predict_Q2k1, predict_Q2k2, predict_Q3k1, predict_Q3k2, 
         predict_Q4k1, predict_Q4k2,
         grid_data_Q1k2, grid_data_Q2k1, grid_data_Q2k2, grid_data_Q3k1, grid_data_Q3k2,
         grid_data_Q4k1, grid_data_Q4k2, split_income)
      #  
      # rm(modelQ1k1, modelQ1k2, modelQ2k1, modelQ2k2, modelQ3k1, modelQ3k2, modelQ4k1, modelQ4k2, 
      #    df_predict_Q1k1, df_predict_Q1k2, df_predict_Q2k1, df_predict_Q2k2, df_predict_Q3k1, 
      #    df_predict_Q3k2, df_predict_Q4k1, df_predict_Q4k2, 
      #    predict_Q1k1, predict_Q1k2, predict_Q2k1, predict_Q2k2, predict_Q3k1, predict_Q3k2, 
      #    predict_Q4k1, predict_Q4k2,
      #    grid_data_Q1k1, grid_data_Q1k2, grid_data_Q2k1, grid_data_Q2k2, grid_data_Q3k1, grid_data_Q3k2,
      #    grid_data_Q4k1, grid_data_Q4k2, split_income)
      # 
      if(inc_men_big_enough_for_reg & !inc_women_big_enough_for_reg){#create NA tibble of other sex
        results_income_koen <- rbind(results_income_koen,NA_tibble_returner(isUDD=FALSE, KOEN=2,analysis = "Income and sex"))
      }
      if(inc_women_big_enough_for_reg & !inc_men_big_enough_for_reg){#create NA tibble of other sex
        results_income_koen <- rbind(results_income_koen,NA_tibble_returner(isUDD=FALSE, KOEN=1,analysis = "Income and sex"))
      }
      
    }else{
      results_income_koen <- NA_tibble_returner(isUDD=FALSE, KOEN=1:2,analysis = "Income and sex")}
    
    # 
    # print("results_udd")
    # print(results_udd)
    # # 
    # print("results_udd_koen")
    # print(results_udd_koen)
    # 
    # print("results_income")
    # print(results_income)
    # # 
    # print("results_income_koen")
    # print(results_income_koen)
    # 
    ### Gather all results
    results <- rbind(results_udd, results_udd_koen, results_income, results_income_koen) 
    
    results <- rbind(results %>% mutate(estimator=str_c("log_RR_",RR_type), value=log(as.numeric(RR))),
                               results %>% mutate(estimator="asir", value=ASIR)) %>%
      dplyr::select(-c(ASIR, RR_type , RR, analysis)) %>%
      filter(!str_detect(estimator,"reference"))
    
    return (results)
    
    
  }
  
  
  
  
  Population_lazy <- lazy_dt(Population) 
  if(bootEDU){#if bootstrapping for H0=education has no effect: permute education vector
    Population_lazy <- mutate(Population_lazy, UDD = UDD[sample.int(nrow(Population))])
  }
  if(bootINC){
    Population_lazy <- mutate(Population_lazy, INCOMEQ = INCOMEQ[sample.int(nrow(Population))])
  }
  
  
  Population_lazy <- Population_lazy %>% slice(bootstrapindices) 
  #print(diags )
  #print(Population_lazy)
  
  diags_mainanalysis_test <-diags  %>%  filter(DIAG=="DC44"|DIAG=="DO98" | DIAG == "DR97" |DIAG=="DI10")
  # print("hi")
  
  
  
  incidence <- diags%>% 
    group_by(DIAG) %>%
    #multidplyr::partition(cluster) %>%
    summarize(
      result = get_incidence_rate(cur_data(), Population=Population_lazy)
    ) %>% collect()
  
  
  incidence <- left_join(tibble(DIAG = incidence$DIAG,ID = seq.int(length(incidence$DIAG))), tibble(incidence$result, ID = seq.int(nrow(incidence$result)))) %>%
    dplyr::select(-c(ID)) 
  if(bootEDU==TRUE&bootINC==TRUE){s <-0 #no change
  }else if(bootEDU){#only select EDU cols (Reason:for aggregating later) #changed to be done after simulation
    incidence <- incidence
  }else if (bootINC){
    incidence <- incidence
  }
  if(return_names){
    return(incidence)
  }
  return (incidence)}
# 
# cluster <- multidplyr::new_cluster(4)
# gc()
# multidplyr::cluster_library(
#   cluster, 
#   c("tidyverse", "lubridate", "Epi", "popEpi","mgcv")
# )
# multidplyr::cluster_copy(cluster, c("get_incidence_rate", "Population", "sp_prop"))
# 
#diags3 <- rbind(DC44,DO98,DR97,DI10)
# incidence <- diags3 %>% 
#   group_by(DIAG) %>%
#   multidplyr::partition(cluster) %>%
#   summarize(
#     result = get_incidence_rate(cur_data())
#   ) %>%
#   collect()



####################SIMULATION TIME !!! ############### 
#Kill earlier cluster, if it is online.

#stopCluster(cl)
start <- Sys.time()
numcores = 10 


#setup new cluster
max_threads_dt = 10 #ensure maximum 16 cores, including subthreads by workers.
data.table::setDTthreads(max_threads_dt,restore_after_fork = FALSE) # dont use multiple threads per worker in datatables (which lexis relies on) - it takes all the space on cluster.
cl = makeCluster(numcores, type = "SOCK")
registerDoSNOW(cl)
setupCluster <- function(){
  
  
  #set seed for each worker.
  
  set.seed(initial_seed)
  seeds_for_cluster = sample(x=1:100000,size=numcores, replace=FALSE)
  
  clusterApply(cl, fun = set.seed, x=seeds_for_cluster)
  
  #setup libraries in each process (each process is started as "new R-session")
  clusterEvalQ(cl, {
    tempdir(check=TRUE)
    Sys.setenv(TMPDIR='D:\\WORK\\FKC')
    unlink(tempdir(),recursive=TRUE)
    tempdir(check=TRUE)
    library(DSTora)
    library(DBI)
    library(ROracle)
    library(tidyverse)
    library(dtplyr)
    library(lubridate)
    library(profvis)
    library(Epi)
    library(popEpi)
    library(mgcv)
    library(speedglm)
    
    max_threads_dt = 2
    data.table::setDTthreads(max_threads_dt,restore_after_fork = FALSE) # dont use any number threads per worker - it takes all the space on cluster.
    
    conn_inci <- OraGenvej(dbname = "STATUDV")#, dbuser = "FKC")
    
    
    
    ####### ------------ Access the data ------------ ########
    
    diags_mainanalysis <- dbReadTable(conn = conn_inci, schema= "D222107", name = "diags_mainanalysis")
    
    Population <- dbReadTable(conn = conn_inci, schema= "D222107", name = "Population_2013_2022")
    
    ###### ------------- Clean data ------------------------###########
    diags_mainanalysis <-diags_mainanalysis  %>% mutate(D_INDDTO = cal.yr(D_INDDTO))
    Population <- Population %>% mutate(STUDY_END = cal.yr(STUDY_END), STUDY_ENTRANCE= cal.yr(STUDY_ENTRANCE), BIRTHDAY = cal.yr(BIRTHDAY))
    
    ####### ------------ Loop for all diseases ------------ ########
    ##for easy testing:
    DI10 <- diags_mainanalysis %>%
      filter(DIAG == "DI10")
    DC44 <- diags_mainanalysis %>%
      filter(DIAG == "DC44")
    DO98 <- diags_mainanalysis %>%
      filter(DIAG == "DO98")
    DR97 <- diags_mainanalysis %>%
      filter(DIAG == "DR97")
    
    #diags4 <- rbind(DI10,DC44,DO98,DR97)
    
    
    # European standard population 2013 (age groups from "30-34 years" to "90+ years")
    sp_prop <- c(0.098, 0.106, 0.106, 0.106, 0.106, 0.098, 0.090, 0.083, 0.075,
                 0.060, 0.038, 0.022, 0.012)
    
    
  })
  
  #check seed.
  
  
  
  clusterEvalQ(cl, {sample(x=1:100000,size=1)})
}



setupCluster()
#if run with seed 442 as inital, first 25 numbers returned (only returning one per core) are:
# 80576 56064 1847 55998 16651 10321 12969 64966 94675 2938  41779 65578 49737 670   53295 12279 32542 65019 1616  13698 6368  43205 53135 25131 45206
# so to check reproducibilty: see that e.g. when run with 10 cores, the function returns 80576 56064 1847 55998 16651 10321 12969 64966 94675 2938
gc()

#actually run simulation
# suppressWarnings(
#   for(i in 1:(30/numcores)){
#     booted_all <-  foreach(i=1:numcores, .verbose=TRUE, .combine=bind_rows) %dopar% {
#       diags <- diags_mainanalysis
#       #print(diags)
#       #resampled rows.
#       inference(Population = Population,bootstrapindices = sample(1:nrow(Population), replace=TRUE), diags = diags,return_names = TRUE)
#     }
#     #write to csvfile here, every minirun..
#     
#     write_csv2(booted_all, paste0( "K:\\Metode\\Challenge\\7_FKC\\age_cohort\\booted_all_i",i,"_diags.csv"))
#     print(Sys.time()-start) 
#   }
# )
# 
# suppressWarnings(
#   for(i in 1:(30/numcores)){
#     booted_edu <-  foreach(i=1:numcores, .verbose=TRUE, .combine=bind_rows) %dopar% {
#       diags <- diags_mainanalysis
#       #print(diags)
#       #resampled rows.
#       inference(Population = Population,bootstrapindices = sample(1:nrow(Population), replace=TRUE), diags = diags, bootEDU = TRUE,return_names = TRUE)
#     }
#     #write to csvfile here, every minirun..
#     
#     write_csv2(booted_edu, paste0( "K:\\Metode\\Challenge\\7_FKC\\age_cohort\\booted_edu_i",i,"_diags.csv"))
#     print(Sys.time()-start) 
#   }
# ) 

# 
# suppressWarnings(
#   for(i in 2:2){ #dirty run due to error in first run.
#     booted_inc <-  foreach(i=1:numcores, .verbose=TRUE, .combine=bind_rows) %dopar% {
#       diags <- diags_mainanalysis
#       #print(diags)
#       #resampled rows.
#       inference(Population = Population,bootstrapindices = sample(1:nrow(Population), replace=TRUE), diags = diags, bootINC = TRUE,return_names = TRUE)
#     }
#     #write to csvfile here, every minirun..
#   
#     write_csv2(booted_inc, paste0( "~/kdrev/Metode/Challenge/7_FKC/age_cohort/booted_inc_i",2,"_diags.csv"))
# 
#     print(Sys.time()-start) 
#   }
# )
# set.seed(initial_seed) #set seed here, if not done earlier
stopCluster(cl)
gc()
original_estimates <- inference(Population = Population,bootstrapindices = 1:nrow(Population), diags = diags_mainanalysis,return_names = TRUE)

write_csv2(original_estimates, paste0( "~/kdrev/Metode/Challenge/7_FKC/age_cohort/original_estimates_diags_all.csv"))
# ASIR_all <- data.frame(incidence) %>%
#   mutate(AGE = result$AGE,
#          KOEN = result$KOEN, 
#          SEP = result$SEP,
#          ASIR = result$ASIR,
#          RR_type = result$RR_type,
#          RR = result$RR,
#          ANALYSIS = result$analysis)
# 
# ASIR_all <- within(ASIR_all, rm(result))

#Save the data
#dbWriteTable(conn_inci, "SMOOTH_diags3", ASIR_all, overwrite = T)


