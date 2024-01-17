### Prepare results - table/excel sheets

library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(ggplot2)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

source("H:/KT22SVN/X30/PhD_Study1/Code_visualization/store_diag_names.R")

##############################################################################
# The following script creates plots to visualise results from PhD study 1   #
##############################################################################

conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")


results_diagall <- read.csv2("H:/KT22SVN/X30/PhD_Study1/Lancet review/Main analysis/diags_confints.csv")
N_all <- dbReadTable(conn_inci, "N_CASES_mainanalysis")

results_CD <- read.csv2("H:/KT22SVN/X30/PhD_Study1/Lancet review/Main analysis/CD_confints.csv")
N_CD <- dbReadTable(conn_inci, "N_CASES_CD")

results_NCD <- read.csv2("H:/KT22SVN/X30/PhD_Study1/Lancet review/Main analysis/NCD_confints.csv")
N_NCD <- dbReadTable(conn_inci, "N_CASES_NCD")


## ALLDIAG


# Education and men

###RR
ALLDIAG_edu_men_RR <- results_diagall %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs3" & SEP == "udd1", "log_RR_udd1vsudd3", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs3" & SEP == "udd2", "log_RR_udd2vsudd3", estimator)) %>% 
  select(DIAG, estimator, original_estimate, sd_confint, pvalue_EDU, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_EDU, q_value, accept)) %>% 
  left_join(select(N_all, DIAG, MALE_edu_N, MALE_edu_PY, MALE_edu_cases), by = "DIAG") %>% 
  mutate(N = MALE_edu_N,
         PERSON_YEARS = MALE_edu_PY,
         CASES = MALE_edu_cases,
         log_RR_udd1vs3 = original_estimate_log_RR_udd1vsudd3,
         LOW_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3-(1.96*sd_confint_log_RR_udd1vsudd3),
         HIGH_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3+(1.96*sd_confint_log_RR_udd1vsudd3),
         PVALUE_udd1vs3 = pvalue_EDU_log_RR_udd1vsudd3,
         QVALUE_udd1vs3 = q_value_log_RR_udd1vsudd3,
         BH_accept_udd1vs3 = accept_log_RR_udd1vsudd3,
         log_RR_udd2vs3 = original_estimate_log_RR_udd2vsudd3,
         LOW_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3-(1.96*sd_confint_log_RR_udd2vsudd3),
         HIGH_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3+(1.96*sd_confint_log_RR_udd2vsudd3),
         PVALUE_udd2vs3 = pvalue_EDU_log_RR_udd2vsudd3,
         QVALUE_udd2vs3 = q_value_log_RR_udd2vsudd3,
         BH_accept_udd2vs3 = accept_log_RR_udd2vsudd3) %>% 
  select(DIAG, N, PERSON_YEARS, CASES, log_RR_udd1vs3, LOW_CI_udd1vs3, HIGH_CI_udd1vs3, PVALUE_udd1vs3, QVALUE_udd1vs3, BH_accept_udd1vs3, log_RR_udd2vs3, LOW_CI_udd2vs3, HIGH_CI_udd2vs3, PVALUE_udd2vs3, QVALUE_udd2vs3, BH_accept_udd2vs3)


####ASIR
ALLDIAG_edu_men_ASIR <- results_diagall %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "udd1", "ASIR_udd1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd2", "ASIR_udd2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd3", "ASIR_udd3", estimator)) %>% 
  select(DIAG, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_all, DIAG, MALE_edu_N, MALE_edu_PY, MALE_edu_cases), by = "DIAG") %>% 
  mutate(N = MALE_edu_N,
         PERSON_YEARS = MALE_edu_PY,
         CASES = MALE_edu_cases,
         ASIR_udd1 = original_estimate_ASIR_udd1,
         LOW_CI_udd1 = original_estimate_ASIR_udd1-(1.96*sd_confint_ASIR_udd1),
         HIGH_CI_udd1 = original_estimate_ASIR_udd1+(1.96*sd_confint_ASIR_udd1),
         ASIR_udd2 = original_estimate_ASIR_udd2,
         LOW_CI_udd2 = original_estimate_ASIR_udd2-(1.96*sd_confint_ASIR_udd2),
         HIGH_CI_udd2 = original_estimate_ASIR_udd2+(1.96*sd_confint_ASIR_udd2),
         ASIR_udd3 = original_estimate_ASIR_udd3,
         LOW_CI_udd3 = original_estimate_ASIR_udd3-(1.96*sd_confint_ASIR_udd3),
         HIGH_CI_udd3 = original_estimate_ASIR_udd3+(1.96*sd_confint_ASIR_udd3),
         DIFFERENCE_udd3_udd1 = ASIR_udd3-ASIR_udd1,
         DIFFERENCE_udd3_udd2 = ASIR_udd3-ASIR_udd2) %>% 
  select(DIAG, N, PERSON_YEARS, CASES, ASIR_udd1, LOW_CI_udd1, HIGH_CI_udd1, ASIR_udd2, LOW_CI_udd2, HIGH_CI_udd2, ASIR_udd3, LOW_CI_udd3, HIGH_CI_udd3, DIFFERENCE_udd3_udd1, DIFFERENCE_udd3_udd2)




# Education and women
###RR
ALLDIAG_edu_women_RR <- results_diagall %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs3" & SEP == "udd1", "log_RR_udd1vsudd3", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs3" & SEP == "udd2", "log_RR_udd2vsudd3", estimator)) %>% 
  select(DIAG, estimator, original_estimate, sd_confint, pvalue_EDU, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_EDU, q_value, accept)) %>% 
  left_join(select(N_all, DIAG, FEMALE_edu_N, FEMALE_edu_PY, FEMALE_edu_cases), by = "DIAG") %>% 
  mutate(N = FEMALE_edu_N,
         PERSON_YEARS = FEMALE_edu_PY,
         CASES = FEMALE_edu_cases,
         log_RR_udd1vs3 = original_estimate_log_RR_udd1vsudd3,
         LOW_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3-(1.96*sd_confint_log_RR_udd1vsudd3),
         HIGH_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3+(1.96*sd_confint_log_RR_udd1vsudd3),
         PVALUE_udd1vs3 = pvalue_EDU_log_RR_udd1vsudd3,
         QVALUE_udd1vs3 = q_value_log_RR_udd1vsudd3,
         BH_accept_udd1vs3 = accept_log_RR_udd1vsudd3,
         log_RR_udd2vs3 = original_estimate_log_RR_udd2vsudd3,
         LOW_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3-(1.96*sd_confint_log_RR_udd2vsudd3),
         HIGH_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3+(1.96*sd_confint_log_RR_udd2vsudd3),
         PVALUE_udd2vs3 = pvalue_EDU_log_RR_udd2vsudd3,
         QVALUE_udd2vs3 = q_value_log_RR_udd2vsudd3,
         BH_accept_udd2vs3 = accept_log_RR_udd2vsudd3) %>% 
  select(DIAG, N, PERSON_YEARS, CASES, log_RR_udd1vs3, LOW_CI_udd1vs3, HIGH_CI_udd1vs3, PVALUE_udd1vs3, QVALUE_udd1vs3, BH_accept_udd1vs3, log_RR_udd2vs3, LOW_CI_udd2vs3, HIGH_CI_udd2vs3, PVALUE_udd2vs3, QVALUE_udd2vs3, BH_accept_udd2vs3)


####ASIR
ALLDIAG_edu_women_ASIR <- results_diagall %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "udd1", "ASIR_udd1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd2", "ASIR_udd2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd3", "ASIR_udd3", estimator)) %>% 
  select(DIAG, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_all, DIAG, FEMALE_edu_N, FEMALE_edu_PY, FEMALE_edu_cases), by = "DIAG") %>% 
  mutate(N = FEMALE_edu_N,
         PERSON_YEARS = FEMALE_edu_PY,
         CASES = FEMALE_edu_cases,
         ASIR_udd1 = original_estimate_ASIR_udd1,
         LOW_CI_udd1 = original_estimate_ASIR_udd1-(1.96*sd_confint_ASIR_udd1),
         HIGH_CI_udd1 = original_estimate_ASIR_udd1+(1.96*sd_confint_ASIR_udd1),
         ASIR_udd2 = original_estimate_ASIR_udd2,
         LOW_CI_udd2 = original_estimate_ASIR_udd2-(1.96*sd_confint_ASIR_udd2),
         HIGH_CI_udd2 = original_estimate_ASIR_udd2+(1.96*sd_confint_ASIR_udd2),
         ASIR_udd3 = original_estimate_ASIR_udd3,
         LOW_CI_udd3 = original_estimate_ASIR_udd3-(1.96*sd_confint_ASIR_udd3),
         HIGH_CI_udd3 = original_estimate_ASIR_udd3+(1.96*sd_confint_ASIR_udd3),
         DIFFERENCE_udd3_udd1 = ASIR_udd3-ASIR_udd1,
         DIFFERENCE_udd3_udd2 = ASIR_udd3-ASIR_udd2) %>% 
  select(DIAG, N, PERSON_YEARS, CASES, ASIR_udd1, LOW_CI_udd1, HIGH_CI_udd1, ASIR_udd2, LOW_CI_udd2, HIGH_CI_udd2, ASIR_udd3, LOW_CI_udd3, HIGH_CI_udd3, DIFFERENCE_udd3_udd1, DIFFERENCE_udd3_udd2)


# Income and men
###RR
ALLDIAG_inc_men_RR <- results_diagall %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs4" & SEP == "Q1", "log_RR_Q1vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs4" & SEP == "Q2", "log_RR_Q2vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR3vs4" & SEP == "Q3", "log_RR_Q3vsQ4", estimator)) %>% 
  select(DIAG, estimator, original_estimate, sd_confint, pvalue_INC, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_INC, q_value, accept)) %>% 
  left_join(select(N_all, DIAG, MALE_inc_N, MALE_inc_PY, MALE_inc_cases), by = "DIAG") %>% 
  mutate(N = MALE_inc_N,
         PERSON_YEARS = MALE_inc_PY,
         CASES = MALE_inc_cases,
         log_RR_Q1vs4 = original_estimate_log_RR_Q1vsQ4,
         LOW_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4-(1.96*sd_confint_log_RR_Q1vsQ4),
         HIGH_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4+(1.96*sd_confint_log_RR_Q1vsQ4),
         PVALUE_Q1vs4 = pvalue_INC_log_RR_Q1vsQ4,
         QVALUE_Q1vs4 = q_value_log_RR_Q1vsQ4,
         BH_accept_Q1vs4 = accept_log_RR_Q1vsQ4,
         log_RR_Q2vs4 = original_estimate_log_RR_Q2vsQ4,
         LOW_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4-(1.96*sd_confint_log_RR_Q2vsQ4),
         HIGH_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4+(1.96*sd_confint_log_RR_Q2vsQ4),
         PVALUE_Q2vs4 = pvalue_INC_log_RR_Q2vsQ4,
         QVALUE_Q2vs4 = q_value_log_RR_Q2vsQ4,
         BH_accept_Q2vs4 = accept_log_RR_Q2vsQ4,
         log_RR_Q3vs4 = original_estimate_log_RR_Q3vsQ4,
         LOW_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4-(1.96*sd_confint_log_RR_Q3vsQ4),
         HIGH_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4+(1.96*sd_confint_log_RR_Q3vsQ4),
         PVALUE_Q3vs4 = pvalue_INC_log_RR_Q3vsQ4,
         QVALUE_Q3vs4 = q_value_log_RR_Q3vsQ4,
         BH_accept_Q3vs4 = accept_log_RR_Q3vsQ4) %>% 
  select(DIAG, N, PERSON_YEARS, CASES, log_RR_Q1vs4, LOW_CI_Q1vs4, HIGH_CI_Q1vs4, PVALUE_Q1vs4, QVALUE_Q1vs4, BH_accept_Q1vs4, log_RR_Q2vs4, LOW_CI_Q2vs4, HIGH_CI_Q2vs4, PVALUE_Q2vs4, QVALUE_Q2vs4, BH_accept_Q2vs4, log_RR_Q3vs4, LOW_CI_Q3vs4, HIGH_CI_Q3vs4, PVALUE_Q3vs4, QVALUE_Q3vs4, BH_accept_Q3vs4)


####ASIR
ALLDIAG_inc_men_ASIR <- results_diagall %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "Q1", "ASIR_Q1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q2", "ASIR_Q2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q3", "ASIR_Q3", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q4", "ASIR_Q4", estimator)) %>% 
  select(DIAG, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_all, DIAG, MALE_inc_N, MALE_inc_PY, MALE_inc_cases), by = "DIAG") %>% 
  mutate(N = MALE_inc_N,
         PERSON_YEARS = MALE_inc_PY,
         CASES = MALE_inc_cases,
         ASIR_Q1 = original_estimate_ASIR_Q1,
         LOW_CI_Q1 = original_estimate_ASIR_Q1-(1.96*sd_confint_ASIR_Q1),
         HIGH_CI_Q1 = original_estimate_ASIR_Q1+(1.96*sd_confint_ASIR_Q1),
         ASIR_Q2 = original_estimate_ASIR_Q2,
         LOW_CI_Q2 = original_estimate_ASIR_Q2-(1.96*sd_confint_ASIR_Q2),
         HIGH_CI_Q2 = original_estimate_ASIR_Q2+(1.96*sd_confint_ASIR_Q2),
         ASIR_Q3 = original_estimate_ASIR_Q3,
         LOW_CI_Q3 = original_estimate_ASIR_Q3-(1.96*sd_confint_ASIR_Q3),
         HIGH_CI_Q3 = original_estimate_ASIR_Q3+(1.96*sd_confint_ASIR_Q3),
         ASIR_Q4 = original_estimate_ASIR_Q4,
         LOW_CI_Q4 = original_estimate_ASIR_Q4-(1.96*sd_confint_ASIR_Q4),
         HIGH_CI_Q4 = original_estimate_ASIR_Q4+(1.96*sd_confint_ASIR_Q4),
         DIFFERENCE_Q4_Q1 = ASIR_Q4-ASIR_Q1,
         DIFFERENCE_Q4_Q2 = ASIR_Q4-ASIR_Q2,
         DIFFERENCE_Q4_Q3 = ASIR_Q4-ASIR_Q3) %>% 
  select(DIAG, N, PERSON_YEARS, CASES, ASIR_Q1, LOW_CI_Q1, HIGH_CI_Q1, ASIR_Q2, LOW_CI_Q2, HIGH_CI_Q2, ASIR_Q3, LOW_CI_Q3, HIGH_CI_Q3, ASIR_Q4, LOW_CI_Q4, HIGH_CI_Q4, DIFFERENCE_Q4_Q1, DIFFERENCE_Q4_Q2, DIFFERENCE_Q4_Q3)



# Income and women
###RR
ALLDIAG_inc_women_RR <- results_diagall %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs4" & SEP == "Q1", "log_RR_Q1vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs4" & SEP == "Q2", "log_RR_Q2vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR3vs4" & SEP == "Q3", "log_RR_Q3vsQ4", estimator)) %>% 
  select(DIAG, estimator, original_estimate, sd_confint, pvalue_INC, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_INC, q_value, accept)) %>% 
  left_join(select(N_all, DIAG, FEMALE_inc_N, FEMALE_inc_PY, FEMALE_inc_cases), by = "DIAG") %>% 
  mutate(N = FEMALE_inc_N,
         PERSON_YEARS = FEMALE_inc_PY,
         CASES = FEMALE_inc_cases,
         log_RR_Q1vs4 = original_estimate_log_RR_Q1vsQ4,
         LOW_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4-(1.96*sd_confint_log_RR_Q1vsQ4),
         HIGH_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4+(1.96*sd_confint_log_RR_Q1vsQ4),
         PVALUE_Q1vs4 = pvalue_INC_log_RR_Q1vsQ4,
         QVALUE_Q1vs4 = q_value_log_RR_Q1vsQ4,
         BH_accept_Q1vs4 = accept_log_RR_Q1vsQ4,
         log_RR_Q2vs4 = original_estimate_log_RR_Q2vsQ4,
         LOW_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4-(1.96*sd_confint_log_RR_Q2vsQ4),
         HIGH_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4+(1.96*sd_confint_log_RR_Q2vsQ4),
         PVALUE_Q2vs4 = pvalue_INC_log_RR_Q2vsQ4,
         QVALUE_Q2vs4 = q_value_log_RR_Q2vsQ4,
         BH_accept_Q2vs4 = accept_log_RR_Q2vsQ4,
         log_RR_Q3vs4 = original_estimate_log_RR_Q3vsQ4,
         LOW_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4-(1.96*sd_confint_log_RR_Q3vsQ4),
         HIGH_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4+(1.96*sd_confint_log_RR_Q3vsQ4),
         PVALUE_Q3vs4 = pvalue_INC_log_RR_Q3vsQ4,
         QVALUE_Q3vs4 = q_value_log_RR_Q3vsQ4,
         BH_accept_Q3vs4 = accept_log_RR_Q3vsQ4) %>% 
  select(DIAG, N, PERSON_YEARS, CASES, log_RR_Q1vs4, LOW_CI_Q1vs4, HIGH_CI_Q1vs4, PVALUE_Q1vs4, QVALUE_Q1vs4, BH_accept_Q1vs4, log_RR_Q2vs4, LOW_CI_Q2vs4, HIGH_CI_Q2vs4, PVALUE_Q2vs4, QVALUE_Q2vs4, BH_accept_Q2vs4, log_RR_Q3vs4, LOW_CI_Q3vs4, HIGH_CI_Q3vs4, PVALUE_Q3vs4, QVALUE_Q3vs4, BH_accept_Q3vs4)


####ASIR
ALLDIAG_inc_women_ASIR <- results_diagall %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "Q1", "ASIR_Q1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q2", "ASIR_Q2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q3", "ASIR_Q3", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q4", "ASIR_Q4", estimator)) %>% 
  select(DIAG, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_all, DIAG, FEMALE_inc_N, FEMALE_inc_PY, FEMALE_inc_cases), by = "DIAG") %>% 
  mutate(N = FEMALE_inc_N,
         PERSON_YEARS = FEMALE_inc_PY,
         CASES = FEMALE_inc_cases,
         ASIR_Q1 = original_estimate_ASIR_Q1,
         LOW_CI_Q1 = original_estimate_ASIR_Q1-(1.96*sd_confint_ASIR_Q1),
         HIGH_CI_Q1 = original_estimate_ASIR_Q1+(1.96*sd_confint_ASIR_Q1),
         ASIR_Q2 = original_estimate_ASIR_Q2,
         LOW_CI_Q2 = original_estimate_ASIR_Q2-(1.96*sd_confint_ASIR_Q2),
         HIGH_CI_Q2 = original_estimate_ASIR_Q2+(1.96*sd_confint_ASIR_Q2),
         ASIR_Q3 = original_estimate_ASIR_Q3,
         LOW_CI_Q3 = original_estimate_ASIR_Q3-(1.96*sd_confint_ASIR_Q3),
         HIGH_CI_Q3 = original_estimate_ASIR_Q3+(1.96*sd_confint_ASIR_Q3),
         ASIR_Q4 = original_estimate_ASIR_Q4,
         LOW_CI_Q4 = original_estimate_ASIR_Q4-(1.96*sd_confint_ASIR_Q4),
         HIGH_CI_Q4 = original_estimate_ASIR_Q4+(1.96*sd_confint_ASIR_Q4),
         DIFFERENCE_Q4_Q1 = ASIR_Q4-ASIR_Q1,
         DIFFERENCE_Q4_Q2 = ASIR_Q4-ASIR_Q2,
         DIFFERENCE_Q4_Q3 = ASIR_Q4-ASIR_Q3) %>% 
  select(DIAG, N, PERSON_YEARS, CASES, ASIR_Q1, LOW_CI_Q1, HIGH_CI_Q1, ASIR_Q2, LOW_CI_Q2, HIGH_CI_Q2, ASIR_Q3, LOW_CI_Q3, HIGH_CI_Q3, ASIR_Q4, LOW_CI_Q4, HIGH_CI_Q4, DIFFERENCE_Q4_Q1, DIFFERENCE_Q4_Q2, DIFFERENCE_Q4_Q3)



### Save data
library(xlsx)

write.xlsx(ALLDIAG_edu_men_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_edu_men_RR.xlsx")
write.xlsx(ALLDIAG_edu_women_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_edu_women_RR.xlsx")
write.xlsx(ALLDIAG_inc_men_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_inc_men_RR.xlsx")
write.xlsx(ALLDIAG_inc_women_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_inc_women_RR.xlsx")

write.xlsx(ALLDIAG_edu_men_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_edu_men_ASIR.xlsx")
write.xlsx(ALLDIAG_edu_women_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_edu_women_ASIR.xlsx")
write.xlsx(ALLDIAG_inc_men_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_inc_men_ASIR.xlsx")
write.xlsx(ALLDIAG_inc_women_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_inc_women_ASIR.xlsx")



## NCD


# Education and men


###RR
NCD_edu_men_RR <- results_NCD %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs3" & SEP == "udd1", "log_RR_udd1vsudd3", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs3" & SEP == "udd2", "log_RR_udd2vsudd3", estimator)) %>% 
  select(NCD, estimator, original_estimate, sd_confint, pvalue_EDU, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_EDU, q_value, accept)) %>% 
  left_join(select(N_NCD, NCD, MALE_edu_N, MALE_edu_PY, MALE_edu_cases), by = "NCD") %>% 
  mutate(N = MALE_edu_N,
         PERSON_YEARS = MALE_edu_PY,
         CASES = MALE_edu_cases,
         log_RR_udd1vs3 = original_estimate_log_RR_udd1vsudd3,
         LOW_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3-(1.96*sd_confint_log_RR_udd1vsudd3),
         HIGH_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3+(1.96*sd_confint_log_RR_udd1vsudd3),
         PVALUE_udd1vs3 = pvalue_EDU_log_RR_udd1vsudd3,
         QVALUE_udd1vs3 = q_value_log_RR_udd1vsudd3,
         BH_accept_udd1vs3 = accept_log_RR_udd1vsudd3,
         log_RR_udd2vs3 = original_estimate_log_RR_udd2vsudd3,
         LOW_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3-(1.96*sd_confint_log_RR_udd2vsudd3),
         HIGH_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3+(1.96*sd_confint_log_RR_udd2vsudd3),
         PVALUE_udd2vs3 = pvalue_EDU_log_RR_udd2vsudd3,
         QVALUE_udd2vs3 = q_value_log_RR_udd2vsudd3,
         BH_accept_udd2vs3 = accept_log_RR_udd2vsudd3) %>% 
  select(NCD, N, PERSON_YEARS, CASES, log_RR_udd1vs3, LOW_CI_udd1vs3, HIGH_CI_udd1vs3, PVALUE_udd1vs3, QVALUE_udd1vs3, BH_accept_udd1vs3, log_RR_udd2vs3, LOW_CI_udd2vs3, HIGH_CI_udd2vs3, PVALUE_udd2vs3, QVALUE_udd2vs3, BH_accept_udd2vs3)


####ASIR
NCD_edu_men_ASIR <- results_NCD %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "udd1", "ASIR_udd1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd2", "ASIR_udd2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd3", "ASIR_udd3", estimator)) %>% 
  select(NCD, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_NCD, NCD, MALE_edu_N, MALE_edu_PY, MALE_edu_cases), by = "NCD") %>% 
  mutate(N = MALE_edu_N,
         PERSON_YEARS = MALE_edu_PY,
         CASES = MALE_edu_cases,
         ASIR_udd1 = original_estimate_ASIR_udd1,
         LOW_CI_udd1 = original_estimate_ASIR_udd1-(1.96*sd_confint_ASIR_udd1),
         HIGH_CI_udd1 = original_estimate_ASIR_udd1+(1.96*sd_confint_ASIR_udd1),
         ASIR_udd2 = original_estimate_ASIR_udd2,
         LOW_CI_udd2 = original_estimate_ASIR_udd2-(1.96*sd_confint_ASIR_udd2),
         HIGH_CI_udd2 = original_estimate_ASIR_udd2+(1.96*sd_confint_ASIR_udd2),
         ASIR_udd3 = original_estimate_ASIR_udd3,
         LOW_CI_udd3 = original_estimate_ASIR_udd3-(1.96*sd_confint_ASIR_udd3),
         HIGH_CI_udd3 = original_estimate_ASIR_udd3+(1.96*sd_confint_ASIR_udd3),
         DIFFERENCE_udd3_udd1 = ASIR_udd3-ASIR_udd1,
         DIFFERENCE_udd3_udd2 = ASIR_udd3-ASIR_udd2) %>% 
  select(NCD, N, PERSON_YEARS, CASES, ASIR_udd1, LOW_CI_udd1, HIGH_CI_udd1, ASIR_udd2, LOW_CI_udd2, HIGH_CI_udd2, ASIR_udd3, LOW_CI_udd3, HIGH_CI_udd3, DIFFERENCE_udd3_udd1, DIFFERENCE_udd3_udd2)




# Education and women
###RR
NCD_edu_women_RR <- results_NCD %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs3" & SEP == "udd1", "log_RR_udd1vsudd3", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs3" & SEP == "udd2", "log_RR_udd2vsudd3", estimator)) %>% 
  select(NCD, estimator, original_estimate, sd_confint, pvalue_EDU, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_EDU, q_value, accept)) %>% 
  left_join(select(N_NCD, NCD, FEMALE_edu_N, FEMALE_edu_PY, FEMALE_edu_cases), by = "NCD") %>% 
  mutate(N = FEMALE_edu_N,
         PERSON_YEARS = FEMALE_edu_PY,
         CASES = FEMALE_edu_cases,
         log_RR_udd1vs3 = original_estimate_log_RR_udd1vsudd3,
         LOW_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3-(1.96*sd_confint_log_RR_udd1vsudd3),
         HIGH_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3+(1.96*sd_confint_log_RR_udd1vsudd3),
         PVALUE_udd1vs3 = pvalue_EDU_log_RR_udd1vsudd3,
         QVALUE_udd1vs3 = q_value_log_RR_udd1vsudd3,
         BH_accept_udd1vs3 = accept_log_RR_udd1vsudd3,
         log_RR_udd2vs3 = original_estimate_log_RR_udd2vsudd3,
         LOW_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3-(1.96*sd_confint_log_RR_udd2vsudd3),
         HIGH_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3+(1.96*sd_confint_log_RR_udd2vsudd3),
         PVALUE_udd2vs3 = pvalue_EDU_log_RR_udd2vsudd3,
         QVALUE_udd2vs3 = q_value_log_RR_udd2vsudd3,
         BH_accept_udd2vs3 = accept_log_RR_udd2vsudd3) %>% 
  select(NCD, N, PERSON_YEARS, CASES, log_RR_udd1vs3, LOW_CI_udd1vs3, HIGH_CI_udd1vs3, PVALUE_udd1vs3, QVALUE_udd1vs3, BH_accept_udd1vs3, log_RR_udd2vs3, LOW_CI_udd2vs3, HIGH_CI_udd2vs3, PVALUE_udd2vs3, QVALUE_udd2vs3, BH_accept_udd2vs3)


####ASIR
NCD_edu_women_ASIR <- results_NCD %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "udd1", "ASIR_udd1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd2", "ASIR_udd2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd3", "ASIR_udd3", estimator)) %>% 
  select(NCD, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_NCD, NCD, FEMALE_edu_N, FEMALE_edu_PY, FEMALE_edu_cases), by = "NCD") %>% 
  mutate(N = FEMALE_edu_N,
         PERSON_YEARS = FEMALE_edu_PY,
         CASES = FEMALE_edu_cases,
         ASIR_udd1 = original_estimate_ASIR_udd1,
         LOW_CI_udd1 = original_estimate_ASIR_udd1-(1.96*sd_confint_ASIR_udd1),
         HIGH_CI_udd1 = original_estimate_ASIR_udd1+(1.96*sd_confint_ASIR_udd1),
         ASIR_udd2 = original_estimate_ASIR_udd2,
         LOW_CI_udd2 = original_estimate_ASIR_udd2-(1.96*sd_confint_ASIR_udd2),
         HIGH_CI_udd2 = original_estimate_ASIR_udd2+(1.96*sd_confint_ASIR_udd2),
         ASIR_udd3 = original_estimate_ASIR_udd3,
         LOW_CI_udd3 = original_estimate_ASIR_udd3-(1.96*sd_confint_ASIR_udd3),
         HIGH_CI_udd3 = original_estimate_ASIR_udd3+(1.96*sd_confint_ASIR_udd3),
         DIFFERENCE_udd3_udd1 = ASIR_udd3-ASIR_udd1,
         DIFFERENCE_udd3_udd2 = ASIR_udd3-ASIR_udd2) %>% 
  select(NCD, N, PERSON_YEARS, CASES, ASIR_udd1, LOW_CI_udd1, HIGH_CI_udd1, ASIR_udd2, LOW_CI_udd2, HIGH_CI_udd2, ASIR_udd3, LOW_CI_udd3, HIGH_CI_udd3, DIFFERENCE_udd3_udd1, DIFFERENCE_udd3_udd2)


# Income and men
###RR
NCD_inc_men_RR <- results_NCD %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs4" & SEP == "Q1", "log_RR_Q1vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs4" & SEP == "Q2", "log_RR_Q2vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR3vs4" & SEP == "Q3", "log_RR_Q3vsQ4", estimator)) %>% 
  select(NCD, estimator, original_estimate, sd_confint, pvalue_INC, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_INC, q_value, accept)) %>% 
  left_join(select(N_NCD, NCD, MALE_inc_N, MALE_inc_PY, MALE_inc_cases), by = "NCD") %>% 
  mutate(N = MALE_inc_N,
         PERSON_YEARS = MALE_inc_PY,
         CASES = MALE_inc_cases,
         log_RR_Q1vs4 = original_estimate_log_RR_Q1vsQ4,
         LOW_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4-(1.96*sd_confint_log_RR_Q1vsQ4),
         HIGH_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4+(1.96*sd_confint_log_RR_Q1vsQ4),
         PVALUE_Q1vs4 = pvalue_INC_log_RR_Q1vsQ4,
         QVALUE_Q1vs4 = q_value_log_RR_Q1vsQ4,
         BH_accept_Q1vs4 = accept_log_RR_Q1vsQ4,
         log_RR_Q2vs4 = original_estimate_log_RR_Q2vsQ4,
         LOW_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4-(1.96*sd_confint_log_RR_Q2vsQ4),
         HIGH_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4+(1.96*sd_confint_log_RR_Q2vsQ4),
         PVALUE_Q2vs4 = pvalue_INC_log_RR_Q2vsQ4,
         QVALUE_Q2vs4 = q_value_log_RR_Q2vsQ4,
         BH_accept_Q2vs4 = accept_log_RR_Q2vsQ4,
         log_RR_Q3vs4 = original_estimate_log_RR_Q3vsQ4,
         LOW_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4-(1.96*sd_confint_log_RR_Q3vsQ4),
         HIGH_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4+(1.96*sd_confint_log_RR_Q3vsQ4),
         PVALUE_Q3vs4 = pvalue_INC_log_RR_Q3vsQ4,
         QVALUE_Q3vs4 = q_value_log_RR_Q3vsQ4,
         BH_accept_Q3vs4 = accept_log_RR_Q3vsQ4) %>% 
  select(NCD, N, PERSON_YEARS, CASES, log_RR_Q1vs4, LOW_CI_Q1vs4, HIGH_CI_Q1vs4, PVALUE_Q1vs4, QVALUE_Q1vs4, BH_accept_Q1vs4, log_RR_Q2vs4, LOW_CI_Q2vs4, HIGH_CI_Q2vs4, PVALUE_Q2vs4, QVALUE_Q2vs4, BH_accept_Q2vs4, log_RR_Q3vs4, LOW_CI_Q3vs4, HIGH_CI_Q3vs4, PVALUE_Q3vs4, QVALUE_Q3vs4, BH_accept_Q3vs4)


####ASIR
NCD_inc_men_ASIR <- results_NCD %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "Q1", "ASIR_Q1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q2", "ASIR_Q2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q3", "ASIR_Q3", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q4", "ASIR_Q4", estimator)) %>% 
  select(NCD, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_NCD, NCD, MALE_inc_N, MALE_inc_PY, MALE_inc_cases), by = "NCD") %>% 
  mutate(N = MALE_inc_N,
         PERSON_YEARS = MALE_inc_PY,
         CASES = MALE_inc_cases,
         ASIR_Q1 = original_estimate_ASIR_Q1,
         LOW_CI_Q1 = original_estimate_ASIR_Q1-(1.96*sd_confint_ASIR_Q1),
         HIGH_CI_Q1 = original_estimate_ASIR_Q1+(1.96*sd_confint_ASIR_Q1),
         ASIR_Q2 = original_estimate_ASIR_Q2,
         LOW_CI_Q2 = original_estimate_ASIR_Q2-(1.96*sd_confint_ASIR_Q2),
         HIGH_CI_Q2 = original_estimate_ASIR_Q2+(1.96*sd_confint_ASIR_Q2),
         ASIR_Q3 = original_estimate_ASIR_Q3,
         LOW_CI_Q3 = original_estimate_ASIR_Q3-(1.96*sd_confint_ASIR_Q3),
         HIGH_CI_Q3 = original_estimate_ASIR_Q3+(1.96*sd_confint_ASIR_Q3),
         ASIR_Q4 = original_estimate_ASIR_Q4,
         LOW_CI_Q4 = original_estimate_ASIR_Q4-(1.96*sd_confint_ASIR_Q4),
         HIGH_CI_Q4 = original_estimate_ASIR_Q4+(1.96*sd_confint_ASIR_Q4),
         DIFFERENCE_Q4_Q1 = ASIR_Q4-ASIR_Q1,
         DIFFERENCE_Q4_Q2 = ASIR_Q4-ASIR_Q2,
         DIFFERENCE_Q4_Q3 = ASIR_Q4-ASIR_Q3) %>% 
  select(NCD, N, PERSON_YEARS, CASES, ASIR_Q1, LOW_CI_Q1, HIGH_CI_Q1, ASIR_Q2, LOW_CI_Q2, HIGH_CI_Q2, ASIR_Q3, LOW_CI_Q3, HIGH_CI_Q3, ASIR_Q4, LOW_CI_Q4, HIGH_CI_Q4, DIFFERENCE_Q4_Q1, DIFFERENCE_Q4_Q2, DIFFERENCE_Q4_Q3)



# Income and women
###RR
NCD_inc_women_RR <- results_NCD %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs4" & SEP == "Q1", "log_RR_Q1vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs4" & SEP == "Q2", "log_RR_Q2vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR3vs4" & SEP == "Q3", "log_RR_Q3vsQ4", estimator)) %>% 
  select(NCD, estimator, original_estimate, sd_confint, pvalue_INC, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_INC, q_value, accept)) %>% 
  left_join(select(N_NCD, NCD, FEMALE_inc_N, FEMALE_inc_PY, FEMALE_inc_cases), by = "NCD") %>% 
  mutate(N = FEMALE_inc_N,
         PERSON_YEARS = FEMALE_inc_PY,
         CASES = FEMALE_inc_cases,
         log_RR_Q1vs4 = original_estimate_log_RR_Q1vsQ4,
         LOW_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4-(1.96*sd_confint_log_RR_Q1vsQ4),
         HIGH_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4+(1.96*sd_confint_log_RR_Q1vsQ4),
         PVALUE_Q1vs4 = pvalue_INC_log_RR_Q1vsQ4,
         QVALUE_Q1vs4 = q_value_log_RR_Q1vsQ4,
         BH_accept_Q1vs4 = accept_log_RR_Q1vsQ4,
         log_RR_Q2vs4 = original_estimate_log_RR_Q2vsQ4,
         LOW_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4-(1.96*sd_confint_log_RR_Q2vsQ4),
         HIGH_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4+(1.96*sd_confint_log_RR_Q2vsQ4),
         PVALUE_Q2vs4 = pvalue_INC_log_RR_Q2vsQ4,
         QVALUE_Q2vs4 = q_value_log_RR_Q2vsQ4,
         BH_accept_Q2vs4 = accept_log_RR_Q2vsQ4,
         log_RR_Q3vs4 = original_estimate_log_RR_Q3vsQ4,
         LOW_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4-(1.96*sd_confint_log_RR_Q3vsQ4),
         HIGH_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4+(1.96*sd_confint_log_RR_Q3vsQ4),
         PVALUE_Q3vs4 = pvalue_INC_log_RR_Q3vsQ4,
         QVALUE_Q3vs4 = q_value_log_RR_Q3vsQ4,
         BH_accept_Q3vs4 = accept_log_RR_Q3vsQ4) %>% 
  select(NCD, N, PERSON_YEARS, CASES, log_RR_Q1vs4, LOW_CI_Q1vs4, HIGH_CI_Q1vs4, PVALUE_Q1vs4, QVALUE_Q1vs4, BH_accept_Q1vs4, log_RR_Q2vs4, LOW_CI_Q2vs4, HIGH_CI_Q2vs4, PVALUE_Q2vs4, QVALUE_Q2vs4, BH_accept_Q2vs4, log_RR_Q3vs4, LOW_CI_Q3vs4, HIGH_CI_Q3vs4, PVALUE_Q3vs4, QVALUE_Q3vs4, BH_accept_Q3vs4)


####ASIR
NCD_inc_women_ASIR <- results_NCD %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "Q1", "ASIR_Q1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q2", "ASIR_Q2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q3", "ASIR_Q3", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q4", "ASIR_Q4", estimator)) %>% 
  select(NCD, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_NCD, NCD, FEMALE_inc_N, FEMALE_inc_PY, FEMALE_inc_cases), by = "NCD") %>% 
  mutate(N = FEMALE_inc_N,
         PERSON_YEARS = FEMALE_inc_PY,
         CASES = FEMALE_inc_cases,
         ASIR_Q1 = original_estimate_ASIR_Q1,
         LOW_CI_Q1 = original_estimate_ASIR_Q1-(1.96*sd_confint_ASIR_Q1),
         HIGH_CI_Q1 = original_estimate_ASIR_Q1+(1.96*sd_confint_ASIR_Q1),
         ASIR_Q2 = original_estimate_ASIR_Q2,
         LOW_CI_Q2 = original_estimate_ASIR_Q2-(1.96*sd_confint_ASIR_Q2),
         HIGH_CI_Q2 = original_estimate_ASIR_Q2+(1.96*sd_confint_ASIR_Q2),
         ASIR_Q3 = original_estimate_ASIR_Q3,
         LOW_CI_Q3 = original_estimate_ASIR_Q3-(1.96*sd_confint_ASIR_Q3),
         HIGH_CI_Q3 = original_estimate_ASIR_Q3+(1.96*sd_confint_ASIR_Q3),
         ASIR_Q4 = original_estimate_ASIR_Q4,
         LOW_CI_Q4 = original_estimate_ASIR_Q4-(1.96*sd_confint_ASIR_Q4),
         HIGH_CI_Q4 = original_estimate_ASIR_Q4+(1.96*sd_confint_ASIR_Q4),
         DIFFERENCE_Q4_Q1 = ASIR_Q4-ASIR_Q1,
         DIFFERENCE_Q4_Q2 = ASIR_Q4-ASIR_Q2,
         DIFFERENCE_Q4_Q3 = ASIR_Q4-ASIR_Q3) %>% 
  select(NCD, N, PERSON_YEARS, CASES, ASIR_Q1, LOW_CI_Q1, HIGH_CI_Q1, ASIR_Q2, LOW_CI_Q2, HIGH_CI_Q2, ASIR_Q3, LOW_CI_Q3, HIGH_CI_Q3, ASIR_Q4, LOW_CI_Q4, HIGH_CI_Q4, DIFFERENCE_Q4_Q1, DIFFERENCE_Q4_Q2, DIFFERENCE_Q4_Q3)




### Save data
library(xlsx)

write.xlsx(NCD_edu_men_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/NCD_edu_men_RR.xlsx")
write.xlsx(NCD_edu_women_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/NCD_edu_women_RR.xlsx")
write.xlsx(NCD_inc_men_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/NCD_inc_men_RR.xlsx")
write.xlsx(NCD_inc_women_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/NCD_inc_women_RR.xlsx")

write.xlsx(NCD_edu_men_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/NCD_edu_men_ASIR.xlsx")
write.xlsx(NCD_edu_women_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/NCD_edu_women_ASIR.xlsx")
write.xlsx(NCD_inc_men_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/NCD_inc_men_ASIR.xlsx")
write.xlsx(NCD_inc_women_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/NCD_inc_women_ASIR.xlsx")



## CD

# Education and men

###RR
CD_edu_men_RR <- results_CD %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs3" & SEP == "udd1", "log_RR_udd1vsudd3", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs3" & SEP == "udd2", "log_RR_udd2vsudd3", estimator)) %>% 
  select(CD, estimator, original_estimate, sd_confint, pvalue_EDU, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_EDU, q_value, accept)) %>% 
  left_join(select(N_CD, CD, MALE_edu_N, MALE_edu_PY, MALE_edu_cases), by = "CD") %>% 
  mutate(N = MALE_edu_N,
         PERSON_YEARS = MALE_edu_PY,
         CASES = MALE_edu_cases,
         log_RR_udd1vs3 = original_estimate_log_RR_udd1vsudd3,
         LOW_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3-(1.96*sd_confint_log_RR_udd1vsudd3),
         HIGH_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3+(1.96*sd_confint_log_RR_udd1vsudd3),
         PVALUE_udd1vs3 = pvalue_EDU_log_RR_udd1vsudd3,
         QVALUE_udd1vs3 = q_value_log_RR_udd1vsudd3,
         BH_accept_udd1vs3 = accept_log_RR_udd1vsudd3,
         log_RR_udd2vs3 = original_estimate_log_RR_udd2vsudd3,
         LOW_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3-(1.96*sd_confint_log_RR_udd2vsudd3),
         HIGH_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3+(1.96*sd_confint_log_RR_udd2vsudd3),
         PVALUE_udd2vs3 = pvalue_EDU_log_RR_udd2vsudd3,
         QVALUE_udd2vs3 = q_value_log_RR_udd2vsudd3,
         BH_accept_udd2vs3 = accept_log_RR_udd2vsudd3) %>% 
  select(CD, N, PERSON_YEARS, CASES, log_RR_udd1vs3, LOW_CI_udd1vs3, HIGH_CI_udd1vs3, PVALUE_udd1vs3, QVALUE_udd1vs3, BH_accept_udd1vs3, log_RR_udd2vs3, LOW_CI_udd2vs3, HIGH_CI_udd2vs3, PVALUE_udd2vs3, QVALUE_udd2vs3, BH_accept_udd2vs3)


####ASIR
CD_edu_men_ASIR <- results_CD %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "udd1", "ASIR_udd1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd2", "ASIR_udd2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd3", "ASIR_udd3", estimator)) %>% 
  select(CD, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_CD, CD, MALE_edu_N, MALE_edu_PY, MALE_edu_cases), by = "CD") %>% 
  mutate(N = MALE_edu_N,
         PERSON_YEARS = MALE_edu_PY,
         CASES = MALE_edu_cases,
         ASIR_udd1 = original_estimate_ASIR_udd1,
         LOW_CI_udd1 = original_estimate_ASIR_udd1-(1.96*sd_confint_ASIR_udd1),
         HIGH_CI_udd1 = original_estimate_ASIR_udd1+(1.96*sd_confint_ASIR_udd1),
         ASIR_udd2 = original_estimate_ASIR_udd2,
         LOW_CI_udd2 = original_estimate_ASIR_udd2-(1.96*sd_confint_ASIR_udd2),
         HIGH_CI_udd2 = original_estimate_ASIR_udd2+(1.96*sd_confint_ASIR_udd2),
         ASIR_udd3 = original_estimate_ASIR_udd3,
         LOW_CI_udd3 = original_estimate_ASIR_udd3-(1.96*sd_confint_ASIR_udd3),
         HIGH_CI_udd3 = original_estimate_ASIR_udd3+(1.96*sd_confint_ASIR_udd3),
         DIFFERENCE_udd3_udd1 = ASIR_udd3-ASIR_udd1,
         DIFFERENCE_udd3_udd2 = ASIR_udd3-ASIR_udd2) %>% 
  select(CD, N, PERSON_YEARS, CASES, ASIR_udd1, LOW_CI_udd1, HIGH_CI_udd1, ASIR_udd2, LOW_CI_udd2, HIGH_CI_udd2, ASIR_udd3, LOW_CI_udd3, HIGH_CI_udd3, DIFFERENCE_udd3_udd1, DIFFERENCE_udd3_udd2)




# Education and women
###RR
CD_edu_women_RR <- results_CD %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs3" & SEP == "udd1", "log_RR_udd1vsudd3", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs3" & SEP == "udd2", "log_RR_udd2vsudd3", estimator)) %>% 
  select(CD, estimator, original_estimate, sd_confint, pvalue_EDU, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_EDU, q_value, accept)) %>% 
  left_join(select(N_CD, CD, FEMALE_edu_N, FEMALE_edu_PY, FEMALE_edu_cases), by = "CD") %>% 
  mutate(N = FEMALE_edu_N,
         PERSON_YEARS = FEMALE_edu_PY,
         CASES = FEMALE_edu_cases,
         log_RR_udd1vs3 = original_estimate_log_RR_udd1vsudd3,
         LOW_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3-(1.96*sd_confint_log_RR_udd1vsudd3),
         HIGH_CI_udd1vs3 = original_estimate_log_RR_udd1vsudd3+(1.96*sd_confint_log_RR_udd1vsudd3),
         PVALUE_udd1vs3 = pvalue_EDU_log_RR_udd1vsudd3,
         QVALUE_udd1vs3 = q_value_log_RR_udd1vsudd3,
         BH_accept_udd1vs3 = accept_log_RR_udd1vsudd3,
         log_RR_udd2vs3 = original_estimate_log_RR_udd2vsudd3,
         LOW_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3-(1.96*sd_confint_log_RR_udd2vsudd3),
         HIGH_CI_udd2vs3 = original_estimate_log_RR_udd2vsudd3+(1.96*sd_confint_log_RR_udd2vsudd3),
         PVALUE_udd2vs3 = pvalue_EDU_log_RR_udd2vsudd3,
         QVALUE_udd2vs3 = q_value_log_RR_udd2vsudd3,
         BH_accept_udd2vs3 = accept_log_RR_udd2vsudd3) %>% 
  select(CD, N, PERSON_YEARS, CASES, log_RR_udd1vs3, LOW_CI_udd1vs3, HIGH_CI_udd1vs3, PVALUE_udd1vs3, QVALUE_udd1vs3, BH_accept_udd1vs3, log_RR_udd2vs3, LOW_CI_udd2vs3, HIGH_CI_udd2vs3, PVALUE_udd2vs3, QVALUE_udd2vs3, BH_accept_udd2vs3)


####ASIR
CD_edu_women_ASIR <- results_CD %>% 
  mutate(SEP_indicator = ifelse(SEP == "udd1", "UDD", NA),
         SEP_indicator = ifelse(SEP == "udd2", "UDD", SEP_indicator),
         SEP_indicator = ifelse(SEP == "udd3", "UDD", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "UDD",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "udd1", "ASIR_udd1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd2", "ASIR_udd2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "udd3", "ASIR_udd3", estimator)) %>% 
  select(CD, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_CD, CD, FEMALE_edu_N, FEMALE_edu_PY, FEMALE_edu_cases), by = "CD") %>% 
  mutate(N = FEMALE_edu_N,
         PERSON_YEARS = FEMALE_edu_PY,
         CASES = FEMALE_edu_cases,
         ASIR_udd1 = original_estimate_ASIR_udd1,
         LOW_CI_udd1 = original_estimate_ASIR_udd1-(1.96*sd_confint_ASIR_udd1),
         HIGH_CI_udd1 = original_estimate_ASIR_udd1+(1.96*sd_confint_ASIR_udd1),
         ASIR_udd2 = original_estimate_ASIR_udd2,
         LOW_CI_udd2 = original_estimate_ASIR_udd2-(1.96*sd_confint_ASIR_udd2),
         HIGH_CI_udd2 = original_estimate_ASIR_udd2+(1.96*sd_confint_ASIR_udd2),
         ASIR_udd3 = original_estimate_ASIR_udd3,
         LOW_CI_udd3 = original_estimate_ASIR_udd3-(1.96*sd_confint_ASIR_udd3),
         HIGH_CI_udd3 = original_estimate_ASIR_udd3+(1.96*sd_confint_ASIR_udd3),
         DIFFERENCE_udd3_udd1 = ASIR_udd3-ASIR_udd1,
         DIFFERENCE_udd3_udd2 = ASIR_udd3-ASIR_udd2) %>% 
  select(CD, N, PERSON_YEARS, CASES, ASIR_udd1, LOW_CI_udd1, HIGH_CI_udd1, ASIR_udd2, LOW_CI_udd2, HIGH_CI_udd2, ASIR_udd3, LOW_CI_udd3, HIGH_CI_udd3, DIFFERENCE_udd3_udd1, DIFFERENCE_udd3_udd2)


# Income and men
###RR
CD_inc_men_RR <- results_CD %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs4" & SEP == "Q1", "log_RR_Q1vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs4" & SEP == "Q2", "log_RR_Q2vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR3vs4" & SEP == "Q3", "log_RR_Q3vsQ4", estimator)) %>% 
  select(CD, estimator, original_estimate, sd_confint, pvalue_INC, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_INC, q_value, accept)) %>% 
  left_join(select(N_CD, CD, MALE_inc_N, MALE_inc_PY, MALE_inc_cases), by = "CD") %>% 
  mutate(N = MALE_inc_N,
         PERSON_YEARS = MALE_inc_PY,
         CASES = MALE_inc_cases,
         log_RR_Q1vs4 = original_estimate_log_RR_Q1vsQ4,
         LOW_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4-(1.96*sd_confint_log_RR_Q1vsQ4),
         HIGH_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4+(1.96*sd_confint_log_RR_Q1vsQ4),
         PVALUE_Q1vs4 = pvalue_INC_log_RR_Q1vsQ4,
         QVALUE_Q1vs4 = q_value_log_RR_Q1vsQ4,
         BH_accept_Q1vs4 = accept_log_RR_Q1vsQ4,
         log_RR_Q2vs4 = original_estimate_log_RR_Q2vsQ4,
         LOW_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4-(1.96*sd_confint_log_RR_Q2vsQ4),
         HIGH_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4+(1.96*sd_confint_log_RR_Q2vsQ4),
         PVALUE_Q2vs4 = pvalue_INC_log_RR_Q2vsQ4,
         QVALUE_Q2vs4 = q_value_log_RR_Q2vsQ4,
         BH_accept_Q2vs4 = accept_log_RR_Q2vsQ4,
         log_RR_Q3vs4 = original_estimate_log_RR_Q3vsQ4,
         LOW_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4-(1.96*sd_confint_log_RR_Q3vsQ4),
         HIGH_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4+(1.96*sd_confint_log_RR_Q3vsQ4),
         PVALUE_Q3vs4 = pvalue_INC_log_RR_Q3vsQ4,
         QVALUE_Q3vs4 = q_value_log_RR_Q3vsQ4,
         BH_accept_Q3vs4 = accept_log_RR_Q3vsQ4) %>% 
  select(CD, N, PERSON_YEARS, CASES, log_RR_Q1vs4, LOW_CI_Q1vs4, HIGH_CI_Q1vs4, PVALUE_Q1vs4, QVALUE_Q1vs4, BH_accept_Q1vs4, log_RR_Q2vs4, LOW_CI_Q2vs4, HIGH_CI_Q2vs4, PVALUE_Q2vs4, QVALUE_Q2vs4, BH_accept_Q2vs4, log_RR_Q3vs4, LOW_CI_Q3vs4, HIGH_CI_Q3vs4, PVALUE_Q3vs4, QVALUE_Q3vs4, BH_accept_Q3vs4)


####ASIR
CD_inc_men_ASIR <- results_CD %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 1,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "Q1", "ASIR_Q1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q2", "ASIR_Q2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q3", "ASIR_Q3", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q4", "ASIR_Q4", estimator)) %>% 
  select(CD, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_CD, CD, MALE_inc_N, MALE_inc_PY, MALE_inc_cases), by = "CD") %>% 
  mutate(N = MALE_inc_N,
         PERSON_YEARS = MALE_inc_PY,
         CASES = MALE_inc_cases,
         ASIR_Q1 = original_estimate_ASIR_Q1,
         LOW_CI_Q1 = original_estimate_ASIR_Q1-(1.96*sd_confint_ASIR_Q1),
         HIGH_CI_Q1 = original_estimate_ASIR_Q1+(1.96*sd_confint_ASIR_Q1),
         ASIR_Q2 = original_estimate_ASIR_Q2,
         LOW_CI_Q2 = original_estimate_ASIR_Q2-(1.96*sd_confint_ASIR_Q2),
         HIGH_CI_Q2 = original_estimate_ASIR_Q2+(1.96*sd_confint_ASIR_Q2),
         ASIR_Q3 = original_estimate_ASIR_Q3,
         LOW_CI_Q3 = original_estimate_ASIR_Q3-(1.96*sd_confint_ASIR_Q3),
         HIGH_CI_Q3 = original_estimate_ASIR_Q3+(1.96*sd_confint_ASIR_Q3),
         ASIR_Q4 = original_estimate_ASIR_Q4,
         LOW_CI_Q4 = original_estimate_ASIR_Q4-(1.96*sd_confint_ASIR_Q4),
         HIGH_CI_Q4 = original_estimate_ASIR_Q4+(1.96*sd_confint_ASIR_Q4),
         DIFFERENCE_Q4_Q1 = ASIR_Q4-ASIR_Q1,
         DIFFERENCE_Q4_Q2 = ASIR_Q4-ASIR_Q2,
         DIFFERENCE_Q4_Q3 = ASIR_Q4-ASIR_Q3) %>% 
  select(CD, N, PERSON_YEARS, CASES, ASIR_Q1, LOW_CI_Q1, HIGH_CI_Q1, ASIR_Q2, LOW_CI_Q2, HIGH_CI_Q2, ASIR_Q3, LOW_CI_Q3, HIGH_CI_Q3, ASIR_Q4, LOW_CI_Q4, HIGH_CI_Q4, DIFFERENCE_Q4_Q1, DIFFERENCE_Q4_Q2, DIFFERENCE_Q4_Q3)



# Income and women
###RR
CD_inc_women_RR <- results_CD %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator != "asir") %>% 
  mutate(estimator = ifelse(estimator == "log_RR_RR1vs4" & SEP == "Q1", "log_RR_Q1vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR2vs4" & SEP == "Q2", "log_RR_Q2vsQ4", estimator),
         estimator = ifelse(estimator == "log_RR_RR3vs4" & SEP == "Q3", "log_RR_Q3vsQ4", estimator)) %>% 
  select(CD, estimator, original_estimate, sd_confint, pvalue_INC, q_value, accept) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint, pvalue_INC, q_value, accept)) %>% 
  left_join(select(N_CD, CD, FEMALE_inc_N, FEMALE_inc_PY, FEMALE_inc_cases), by = "CD") %>% 
  mutate(N = FEMALE_inc_N,
         PERSON_YEARS = FEMALE_inc_PY,
         CASES = FEMALE_inc_cases,
         log_RR_Q1vs4 = original_estimate_log_RR_Q1vsQ4,
         LOW_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4-(1.96*sd_confint_log_RR_Q1vsQ4),
         HIGH_CI_Q1vs4 = original_estimate_log_RR_Q1vsQ4+(1.96*sd_confint_log_RR_Q1vsQ4),
         PVALUE_Q1vs4 = pvalue_INC_log_RR_Q1vsQ4,
         QVALUE_Q1vs4 = q_value_log_RR_Q1vsQ4,
         BH_accept_Q1vs4 = accept_log_RR_Q1vsQ4,
         log_RR_Q2vs4 = original_estimate_log_RR_Q2vsQ4,
         LOW_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4-(1.96*sd_confint_log_RR_Q2vsQ4),
         HIGH_CI_Q2vs4 = original_estimate_log_RR_Q2vsQ4+(1.96*sd_confint_log_RR_Q2vsQ4),
         PVALUE_Q2vs4 = pvalue_INC_log_RR_Q2vsQ4,
         QVALUE_Q2vs4 = q_value_log_RR_Q2vsQ4,
         BH_accept_Q2vs4 = accept_log_RR_Q2vsQ4,
         log_RR_Q3vs4 = original_estimate_log_RR_Q3vsQ4,
         LOW_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4-(1.96*sd_confint_log_RR_Q3vsQ4),
         HIGH_CI_Q3vs4 = original_estimate_log_RR_Q3vsQ4+(1.96*sd_confint_log_RR_Q3vsQ4),
         PVALUE_Q3vs4 = pvalue_INC_log_RR_Q3vsQ4,
         QVALUE_Q3vs4 = q_value_log_RR_Q3vsQ4,
         BH_accept_Q3vs4 = accept_log_RR_Q3vsQ4) %>% 
  select(CD, N, PERSON_YEARS, CASES, log_RR_Q1vs4, LOW_CI_Q1vs4, HIGH_CI_Q1vs4, PVALUE_Q1vs4, QVALUE_Q1vs4, BH_accept_Q1vs4, log_RR_Q2vs4, LOW_CI_Q2vs4, HIGH_CI_Q2vs4, PVALUE_Q2vs4, QVALUE_Q2vs4, BH_accept_Q2vs4, log_RR_Q3vs4, LOW_CI_Q3vs4, HIGH_CI_Q3vs4, PVALUE_Q3vs4, QVALUE_Q3vs4, BH_accept_Q3vs4)


####ASIR
CD_inc_women_ASIR <- results_CD %>% 
  mutate(SEP_indicator = ifelse(SEP == "Q1", "IND", NA),
         SEP_indicator = ifelse(SEP == "Q2", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q3", "IND", SEP_indicator),
         SEP_indicator = ifelse(SEP == "Q4", "IND", SEP_indicator)) %>% 
  filter(KOEN == 2,
         SEP_indicator == "IND",
         !is.na(original_estimate),
         estimator == "asir") %>% 
  mutate(estimator = ifelse(estimator == "asir" & SEP == "Q1", "ASIR_Q1", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q2", "ASIR_Q2", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q3", "ASIR_Q3", estimator),
         estimator = ifelse(estimator == "asir" & SEP == "Q4", "ASIR_Q4", estimator)) %>% 
  select(CD, estimator, original_estimate, sd_confint) %>% 
  pivot_wider(names_from = estimator, values_from = c(original_estimate, sd_confint)) %>% 
  left_join(select(N_CD, CD, FEMALE_inc_N, FEMALE_inc_PY, FEMALE_inc_cases), by = "CD") %>% 
  mutate(N = FEMALE_inc_N,
         PERSON_YEARS = FEMALE_inc_PY,
         CASES = FEMALE_inc_cases,
         ASIR_Q1 = original_estimate_ASIR_Q1,
         LOW_CI_Q1 = original_estimate_ASIR_Q1-(1.96*sd_confint_ASIR_Q1),
         HIGH_CI_Q1 = original_estimate_ASIR_Q1+(1.96*sd_confint_ASIR_Q1),
         ASIR_Q2 = original_estimate_ASIR_Q2,
         LOW_CI_Q2 = original_estimate_ASIR_Q2-(1.96*sd_confint_ASIR_Q2),
         HIGH_CI_Q2 = original_estimate_ASIR_Q2+(1.96*sd_confint_ASIR_Q2),
         ASIR_Q3 = original_estimate_ASIR_Q3,
         LOW_CI_Q3 = original_estimate_ASIR_Q3-(1.96*sd_confint_ASIR_Q3),
         HIGH_CI_Q3 = original_estimate_ASIR_Q3+(1.96*sd_confint_ASIR_Q3),
         ASIR_Q4 = original_estimate_ASIR_Q4,
         LOW_CI_Q4 = original_estimate_ASIR_Q4-(1.96*sd_confint_ASIR_Q4),
         HIGH_CI_Q4 = original_estimate_ASIR_Q4+(1.96*sd_confint_ASIR_Q4),
         DIFFERENCE_Q4_Q1 = ASIR_Q4-ASIR_Q1,
         DIFFERENCE_Q4_Q2 = ASIR_Q4-ASIR_Q2,
         DIFFERENCE_Q4_Q3 = ASIR_Q4-ASIR_Q3) %>% 
  select(CD, N, PERSON_YEARS, CASES, ASIR_Q1, LOW_CI_Q1, HIGH_CI_Q1, ASIR_Q2, LOW_CI_Q2, HIGH_CI_Q2, ASIR_Q3, LOW_CI_Q3, HIGH_CI_Q3, ASIR_Q4, LOW_CI_Q4, HIGH_CI_Q4, DIFFERENCE_Q4_Q1, DIFFERENCE_Q4_Q2, DIFFERENCE_Q4_Q3)



### Save data
library(xlsx)

write.xlsx(CD_edu_men_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/CD_edu_men_RR.xlsx")
write.xlsx(CD_edu_women_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/CD_edu_women_RR.xlsx")
write.xlsx(CD_inc_men_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/CD_inc_men_RR.xlsx")
write.xlsx(CD_inc_women_RR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/CD_inc_women_RR.xlsx")

write.xlsx(CD_edu_men_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/CD_edu_men_ASIR.xlsx")
write.xlsx(CD_edu_women_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/CD_edu_women_ASIR.xlsx")
write.xlsx(CD_inc_men_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/CD_inc_men_ASIR.xlsx")
write.xlsx(CD_inc_women_ASIR, "H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/CD_inc_women_ASIR.xlsx")


