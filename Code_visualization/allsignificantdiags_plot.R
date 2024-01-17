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
N_all <- dbReadTable(conn_inci, "N_CASES_mainanalysis") %>% 
  left_join(dplyr::select(names, DIAG, KAP), by = "DIAG") %>% 
  unique() %>% 
  mutate(KAP = ifelse(DIAG == "DA97", 1, KAP),
         KAP = ifelse(DIAG == "DC99", 2, KAP),
         KAP = ifelse(DIAG == "DR97", 18, KAP)) %>% 
  mutate(FEMALE_edu_prop = (FEMALE_edu_cases/sum(FEMALE_edu_cases)) * 100,
         MALE_edu_prop = (MALE_edu_cases/sum(MALE_edu_cases)) * 100,
         FEMALE_inc_prop = (FEMALE_inc_cases/sum(FEMALE_inc_cases)) * 100,
         MALE_inc_prop = (MALE_inc_cases/sum(MALE_inc_cases)) * 100)


################### WOMEN ###################

###### data: education & women

conf_diags_women_edu <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_edu_women_RR.xlsx")

ALLDIAG_women_edu <- conf_diags_women_edu %>% 
  select(-"...1") %>% 
  pivot_longer(c(5,11), names_to = "ESTIMATOR", values_to = "VALUE")%>% 
  pivot_longer(c(5,10), names_to = "CI_low_temp", values_to = "CI_LOW")%>% 
  pivot_longer(c(5,9), names_to = "CI_high_temp", values_to = "CI_HIGH")%>% 
  pivot_longer(c(5,8), names_to = "PVALUE_temp", values_to = "P_VALUE")%>% 
  pivot_longer(c(5,7), names_to = "QVALUE_temp", values_to = "QVALUE")%>% 
  pivot_longer(c(5,6), names_to = "BH_ACCEPT_temp", values_to = "BH_ACCEPT") %>% 
  filter(ESTIMATOR == "log_RR_udd1vs3" & CI_low_temp == "LOW_CI_udd1vs3" & CI_high_temp == "HIGH_CI_udd1vs3" & PVALUE_temp == "PVALUE_udd1vs3" & QVALUE_temp == "QVALUE_udd1vs3" & BH_ACCEPT_temp == "BH_accept_udd1vs3" |
           ESTIMATOR == "log_RR_udd2vs3" & CI_low_temp == "LOW_CI_udd2vs3" & CI_high_temp == "HIGH_CI_udd2vs3" & PVALUE_temp == "PVALUE_udd2vs3" & QVALUE_temp == "QVALUE_udd2vs3" & BH_ACCEPT_temp == "BH_accept_udd2vs3") %>% 
  select(DIAG, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT) %>% 
  left_join(dplyr::select(names, DIAG, KAP), by = "DIAG") %>% 
  unique() %>% 
  mutate(KAP = ifelse(DIAG == "DA97", 1, KAP),
         KAP = ifelse(DIAG == "DC99", 2, KAP),
         KAP = ifelse(DIAG == "DR97", 18, KAP))


# Copenhagen_plot data - education

Copenhagen_women_edu <- left_join(ALLDIAG_women_edu, select(N_all, DIAG, KAP, FEMALE_edu_prop), by = c("DIAG", "KAP")) %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))



###### data: income & women

conf_diags_women_inc <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_inc_women_RR.xlsx")

ALLDIAG_women_inc <- conf_diags_women_inc %>% 
  select(-"...1") %>% 
  pivot_longer(c(5,11,17), names_to = "ESTIMATOR", values_to = "VALUE")%>% 
  pivot_longer(c(5,10,15), names_to = "CI_low_temp", values_to = "CI_LOW")%>% 
  pivot_longer(c(5,9,13), names_to = "CI_high_temp", values_to = "CI_HIGH")%>% 
  pivot_longer(c(5,8,11), names_to = "PVALUE_temp", values_to = "P_VALUE")%>% 
  pivot_longer(c(5,7,9), names_to = "QVALUE_temp", values_to = "QVALUE")%>% 
  pivot_longer(c(5,6,7), names_to = "BH_ACCEPT_temp", values_to = "BH_ACCEPT") %>% 
  filter(ESTIMATOR == "log_RR_Q1vs4" & CI_low_temp == "LOW_CI_Q1vs4" & CI_high_temp == "HIGH_CI_Q1vs4" & PVALUE_temp == "PVALUE_Q1vs4" & QVALUE_temp == "QVALUE_Q1vs4" & BH_ACCEPT_temp == "BH_accept_Q1vs4" |
           ESTIMATOR == "log_RR_Q2vs4" & CI_low_temp == "LOW_CI_Q2vs4" & CI_high_temp == "HIGH_CI_Q2vs4" & PVALUE_temp == "PVALUE_Q2vs4" & QVALUE_temp == "QVALUE_Q2vs4" & BH_ACCEPT_temp == "BH_accept_Q2vs4" |
           ESTIMATOR == "log_RR_Q3vs4" & CI_low_temp == "LOW_CI_Q3vs4" & CI_high_temp == "HIGH_CI_Q3vs4" & PVALUE_temp == "PVALUE_Q3vs4" & QVALUE_temp == "QVALUE_Q3vs4" & BH_ACCEPT_temp == "BH_accept_Q3vs4") %>% 
  select(DIAG, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT) %>% 
left_join(dplyr::select(names, DIAG, KAP), by = "DIAG") %>% 
  unique() %>% 
  mutate(KAP = ifelse(DIAG == "DA97", 1, KAP),
         KAP = ifelse(DIAG == "DC99", 2, KAP),
         KAP = ifelse(DIAG == "DR97", 18, KAP))


# Copenhagen_plot data - income

Copenhagen_women_inc <- left_join(ALLDIAG_women_inc, select(N_all, DIAG, KAP, FEMALE_inc_prop), by = c("DIAG", "KAP")) %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))


# Making list of chapters

chap_women_edu <- Copenhagen_women_edu %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(KAP) %>% 
  unique() %>% 
  as.data.frame()


chap_women_inc <- Copenhagen_women_inc %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(KAP) %>% 
  unique() %>% 
  as.data.frame()

chap_list <- rbind(chap_women_edu, chap_women_inc) %>% 
  unique()


#### Finishing data

Copenhagen_women_edu <- left_join(chap_list, Copenhagen_women_edu, by = "KAP")

Copenhagen_women_inc <- left_join(chap_list, Copenhagen_women_inc, by = "KAP")



# plot : eduation
group_colors <- viridis::viridis(n=2, begin = 0, end = 0.8)


ggplot(Copenhagen_women_edu, 
       aes(x = VALUE, y = as.factor(KAP), color = as.factor(ESTIMATOR), size = FEMALE_edu_prop)) +
  geom_jitter(width = 0, height = 0.25) + 
  theme_minimal() +
  theme_classic() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        strip.background = element_rect(fill=NA, color="black")) +
  scale_y_discrete(limits=rev, labels = c('19',
                                          '18',
                                          '14', 
                                          '13',
                                          '12',
                                          '11',
                                          '10',
                                          '9',
                                          '8',
                                          '7',
                                          '6', 
                                          '5',
                                          '4',
                                          '3',
                                          '2',
                                          '1'))  +
  scale_color_manual(name = "Education", labels = c("Low vs. high", "Medium vs. High"), values = group_colors)+
  scale_size_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "Black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'ICD-10 chapter')+
  #ggtitle("Age-standardised incidence rate ratios for 2012-2021 presented on a log scale") +
  xlim(-5,5)


# plot: income

group_colors_inc <- viridis::viridis(n=3, begin = 0, end = 1)


ggplot(Copenhagen_women_inc, 
       aes(x = VALUE, y = as.factor(KAP), color = as.factor(ESTIMATOR), size = FEMALE_inc_prop)) +
  geom_jitter(width = 0, height = 0.25) + 
  theme_minimal() +
  theme_classic() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        strip.background = element_rect(fill=NA, color="black")) +
  scale_y_discrete(limits=rev, labels = c('19',
                                          '18',
                                          '14', 
                                          '13',
                                          '12',
                                          '11',
                                          '10',
                                          '9',
                                          '8',
                                          '7',
                                          '6', 
                                          '5',
                                          '4',
                                          '3',
                                          '2',
                                          '1'))  +
  scale_color_manual(name = "Income", labels = c("Q1 vs. Q4", "Q2 vs. Q4", "Q3 vs. Q4"), values = group_colors_inc)+
  scale_size_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "Black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'ICD-10 chapter')+
  #ggtitle("Age-standardised incidence rate ratios for 2012-2021 presented on a log scale") +
  xlim(-5,5)


################### MEN ###################

###### data: education & men

conf_diags_men_edu <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_edu_men_RR.xlsx")

ALLDIAG_men_edu <- conf_diags_men_edu %>% 
  select(-"...1") %>% 
  pivot_longer(c(5,11), names_to = "ESTIMATOR", values_to = "VALUE")%>% 
  pivot_longer(c(5,10), names_to = "CI_low_temp", values_to = "CI_LOW")%>% 
  pivot_longer(c(5,9), names_to = "CI_high_temp", values_to = "CI_HIGH")%>% 
  pivot_longer(c(5,8), names_to = "PVALUE_temp", values_to = "P_VALUE")%>% 
  pivot_longer(c(5,7), names_to = "QVALUE_temp", values_to = "QVALUE")%>% 
  pivot_longer(c(5,6), names_to = "BH_ACCEPT_temp", values_to = "BH_ACCEPT") %>% 
  filter(ESTIMATOR == "log_RR_udd1vs3" & CI_low_temp == "LOW_CI_udd1vs3" & CI_high_temp == "HIGH_CI_udd1vs3" & PVALUE_temp == "PVALUE_udd1vs3" & QVALUE_temp == "QVALUE_udd1vs3" & BH_ACCEPT_temp == "BH_accept_udd1vs3" |
           ESTIMATOR == "log_RR_udd2vs3" & CI_low_temp == "LOW_CI_udd2vs3" & CI_high_temp == "HIGH_CI_udd2vs3" & PVALUE_temp == "PVALUE_udd2vs3" & QVALUE_temp == "QVALUE_udd2vs3" & BH_ACCEPT_temp == "BH_accept_udd2vs3") %>% 
  select(DIAG, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT) %>% 
  left_join(dplyr::select(names, DIAG, KAP), by = "DIAG") %>% 
  unique() %>% 
  mutate(KAP = ifelse(DIAG == "DA97", 1, KAP),
         KAP = ifelse(DIAG == "DC99", 2, KAP),
         KAP = ifelse(DIAG == "DR97", 18, KAP))


# Copenhagen_plot data - education

Copenhagen_men_edu <- left_join(ALLDIAG_men_edu, select(N_all, DIAG, KAP, MALE_edu_prop), by = c("DIAG", "KAP")) %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))



###### data: income & men

conf_diags_men_inc <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/ALLDIAG_inc_men_RR.xlsx")

ALLDIAG_men_inc <- conf_diags_men_inc %>% 
  select(-"...1") %>% 
  pivot_longer(c(5,11,17), names_to = "ESTIMATOR", values_to = "VALUE")%>% 
  pivot_longer(c(5,10,15), names_to = "CI_low_temp", values_to = "CI_LOW")%>% 
  pivot_longer(c(5,9,13), names_to = "CI_high_temp", values_to = "CI_HIGH")%>% 
  pivot_longer(c(5,8,11), names_to = "PVALUE_temp", values_to = "P_VALUE")%>% 
  pivot_longer(c(5,7,9), names_to = "QVALUE_temp", values_to = "QVALUE")%>% 
  pivot_longer(c(5,6,7), names_to = "BH_ACCEPT_temp", values_to = "BH_ACCEPT") %>% 
  filter(ESTIMATOR == "log_RR_Q1vs4" & CI_low_temp == "LOW_CI_Q1vs4" & CI_high_temp == "HIGH_CI_Q1vs4" & PVALUE_temp == "PVALUE_Q1vs4" & QVALUE_temp == "QVALUE_Q1vs4" & BH_ACCEPT_temp == "BH_accept_Q1vs4" |
           ESTIMATOR == "log_RR_Q2vs4" & CI_low_temp == "LOW_CI_Q2vs4" & CI_high_temp == "HIGH_CI_Q2vs4" & PVALUE_temp == "PVALUE_Q2vs4" & QVALUE_temp == "QVALUE_Q2vs4" & BH_ACCEPT_temp == "BH_accept_Q2vs4" |
           ESTIMATOR == "log_RR_Q3vs4" & CI_low_temp == "LOW_CI_Q3vs4" & CI_high_temp == "HIGH_CI_Q3vs4" & PVALUE_temp == "PVALUE_Q3vs4" & QVALUE_temp == "QVALUE_Q3vs4" & BH_ACCEPT_temp == "BH_accept_Q3vs4") %>% 
  select(DIAG, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT) %>% 
  left_join(dplyr::select(names, DIAG, KAP), by = "DIAG") %>% 
  unique() %>% 
  mutate(KAP = ifelse(DIAG == "DA97", 1, KAP),
         KAP = ifelse(DIAG == "DC99", 2, KAP),
         KAP = ifelse(DIAG == "DR97", 18, KAP))


# Copenhagen_plot data - income

Copenhagen_men_inc <- left_join(ALLDIAG_men_inc, select(N_all, DIAG, KAP, MALE_inc_prop), by = c("DIAG", "KAP")) %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))


# Making list of chapters

chap_men_edu <- Copenhagen_men_edu %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(KAP) %>% 
  unique() %>% 
  as.data.frame()


chap_men_inc <- Copenhagen_men_inc %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(KAP) %>% 
  unique() %>% 
  as.data.frame()

chap_list <- rbind(chap_men_edu, chap_men_inc) %>% 
  unique()


#### Finishing data

Copenhagen_men_edu <- left_join(chap_list, Copenhagen_men_edu, by = "KAP")

Copenhagen_men_inc <- left_join(chap_list, Copenhagen_men_inc, by = "KAP")



# plot : eduation
group_colors <- viridis::viridis(n=2, begin = 0, end = 0.8)


ggplot(Copenhagen_men_edu, 
       aes(x = VALUE, y = as.factor(KAP), color = as.factor(ESTIMATOR), size = MALE_edu_prop)) +
  geom_jitter(width = 0, height = 0.25) + 
  theme_minimal() +
  theme_classic() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        strip.background = element_rect(fill=NA, color="black")) +
  scale_y_discrete(limits=rev, labels = c('19',
                                          '18',
                                          '14', 
                                          '13',
                                          '12',
                                          '11',
                                          '10',
                                          '9',
                                          '8',
                                          '7',
                                          '6', 
                                          '5',
                                          '4',
                                          '3',
                                          '2',
                                          '1'))  +
  scale_color_manual(name = "Education", labels = c("Low vs. high", "Medium vs. High"), values = group_colors)+
  scale_size_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "Black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'ICD-10 chapter')+
  #ggtitle("Age-standardised incidence rate ratios for 2012-2021 presented on a log scale") +
  xlim(-5,5)


# plot: income

group_colors_inc <- viridis::viridis(n=3, begin = 0, end = 1)


ggplot(Copenhagen_men_inc, 
       aes(x = VALUE, y = as.factor(KAP), color = as.factor(ESTIMATOR), size = MALE_inc_prop)) +
  geom_jitter(width = 0, height = 0.25) + 
  theme_minimal() +
  theme_classic() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        strip.background = element_rect(fill=NA, color="black")) +
  scale_y_discrete(limits=rev, labels = c('19',
                                          '18',
                                          '14', 
                                          '13',
                                          '12',
                                          '11',
                                          '10',
                                          '9',
                                          '8',
                                          '7',
                                          '6', 
                                          '5',
                                          '4',
                                          '3',
                                          '2',
                                          '1'))  +
  scale_color_manual(name = "Income", labels = c("Q1 vs. Q4", "Q2 vs. Q4", "Q3 vs. Q4"), values = group_colors_inc)+
  scale_size_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "Black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'ICD-10 chapter')+
  #ggtitle("Age-standardised incidence rate ratios for 2012-2021 presented on a log scale") +
  xlim(-5,5)

