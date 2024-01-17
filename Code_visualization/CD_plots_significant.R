library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(ggplot2)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 


##############################################################################
# The following script creates plots to visualise results from PhD study 1   #
##############################################################################

conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
N_CD <- dbReadTable(conn_inci, "N_CASES_CD")%>% 
  mutate(FEMALE_edu_prop = (FEMALE_edu_cases/sum(FEMALE_edu_cases)) * 100,
         MALE_edu_prop = (MALE_edu_cases/sum(MALE_edu_cases)) * 100,
         FEMALE_inc_prop = (FEMALE_inc_cases/sum(FEMALE_inc_cases)) * 100,
         MALE_inc_prop = (MALE_inc_cases/sum(MALE_inc_cases)) * 100)



################### WOMEN ###################

###### data: education & men

conf_CD_women_edu <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/SupplementaryData12_CD_education_women_RR.xlsx")

conf_CD_women_edu <- left_join(conf_CD_women_edu, N_CD, by = "CD") %>% 
  mutate(GBD_cause_name = ifelse(CD == "Tuberculosis", "Tuberculosis", NA),
         GBD_cause_name = ifelse(CD == "Syphilis", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Chlamydia", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Gonorrhea", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other sexually transmitted diseases", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "HIV/AIDS", "HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Diarrheal diseases", "Diarrheal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Pertussis", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Poliomyelitis", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Diphtheria", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Measles", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Tetanus", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Meningitis", "Meningitis", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis A", "Hepatitis A", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis B", "Hepatitis B", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis C", "Hepatitis C", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Malaria", "Malaria", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trypanosomiasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Chagas' disease", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Schistosomiasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Leishmaniasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Lymphatic filariasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Onchocerciasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Leprosy", "Leprosy", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Dengue", "Dengue", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Japanese encephalitis", "Japanese ecephalitis", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trachoma", "Trachoma", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Ascariasis", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trichuriasis", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hookworm disease", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other intestinal infections", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other infectious diseases", "Other infectious diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Lower respiratory infections", "Respiratory infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Upper respiratory infections", "Respiratory infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Otitis media", "Respiratory infections", GBD_cause_name)) %>% 
  mutate(order = ifelse(GBD_cause_name == "Tuberculosis", 1, NA),
         order = ifelse(GBD_cause_name == "Sexually transmitted diseases excluding HIV/AIDS", 2, order),
         order = ifelse(GBD_cause_name == "HIV/AIDS", 3, order),
         order = ifelse(GBD_cause_name == "Diarrheal diseases", 4, order),
         order = ifelse(GBD_cause_name == "Childhood-cluster diseases", 5, order),
         order = ifelse(GBD_cause_name == "Meningitis", 6, order),
         order = ifelse(GBD_cause_name == "Hepatitis A", 7, order),
         order = ifelse(GBD_cause_name == "Hepatitis B", 8, order),
         order = ifelse(GBD_cause_name == "Hepatitis C", 9, order),
         order = ifelse(GBD_cause_name == "Malaria", 10, order),
         order = ifelse(GBD_cause_name == "Tropical-cluster diseases", 11, order),
         order = ifelse(GBD_cause_name == "Leprosy", 12, order),
         order = ifelse(GBD_cause_name == "Dengue", 13, order),
         order = ifelse(GBD_cause_name == "Japanese ecephalitis", 14, order),
         order = ifelse(GBD_cause_name == "Trachoma", 15, order),
         order = ifelse(GBD_cause_name == "Intestinal nomatode infections", 16, order),
         order = ifelse(GBD_cause_name == "Other infectious diseases", 17, order),
         order = ifelse(GBD_cause_name == "Respiratory infections", 18, order)) %>% 
  mutate(CD = ifelse(CD == "Tuberculosis", "AA: Tuberculosis", CD),
         CD = ifelse(CD == "Syphilis", "BA: Syphilis", CD),
         CD = ifelse(CD == "Chlamydia", "BB: Chlamydia", CD),
         CD = ifelse(CD == "Gonorrhea", "BC: Gonorrhea", CD),
         CD = ifelse(CD == "Other sexually transmitted diseases", "BD: Other sexually transmitted diseases", CD),
         CD = ifelse(CD == "HIV/AIDS", "CA: HIV/AIDS", CD),
         CD = ifelse(CD == "Diarrheal diseases", "DA: Diarrheal diseases", CD),
         CD = ifelse(CD == "Pertussis", "EA: Pertussis", CD),
         CD = ifelse(CD == "Poliomyelitis", "EB: Poliomyelitis", CD),
         CD = ifelse(CD == "Diphtheria", "EC: Diphtheria", CD),
         CD = ifelse(CD == "Measles", "ED: Measles", CD),
         CD = ifelse(CD == "Tetanus", "EF: Tetanus", CD),
         CD = ifelse(CD == "Meningitis", "FA: Meningitis", CD),
         CD = ifelse(CD == "Hepatitis A", "GA: Hepatitis A", CD),
         CD = ifelse(CD == "Hepatitis B", "HA: Hepatitis B", CD),
         CD = ifelse(CD == "Hepatitis C", "IA: Hepatitis C", CD),
         CD = ifelse(CD == "Malaria", "JA: Malaria", CD),
         CD = ifelse(CD == "Trypanosomiasis", "KA: Trypanosomiasis", CD),
         CD = ifelse(CD == "Chagas' disease", "KB: Chagas' disease", CD),
         CD = ifelse(CD == "Schistosomiasis", "KC: Schistosomiasis", CD),
         CD = ifelse(CD == "Leishmaniasis", "KD: Leishmaniasis", CD),
         CD = ifelse(CD == "Lymphatic filariasis", "KE: Lymphatic filariasis", CD),
         CD = ifelse(CD == "Onchocerciasis", "KF: Onchocerciasis", CD),
         CD = ifelse(CD == "Leprosy", "LA: Leprosy", CD),
         CD = ifelse(CD == "Dengue", "MA: Dengue", CD),
         CD = ifelse(CD == "Japanese encephalitis", "NA: Japanese encephalitis", CD),
         CD = ifelse(CD == "Trachoma", "OA: Trachoma", CD),
         CD = ifelse(CD == "Ascariasis", "PA: Ascariasis", CD),
         CD = ifelse(CD == "Trichuriasis", "PB: Trichuriasis", CD),
         CD = ifelse(CD == "Hookworm disease", "PC: Hookworm disease", CD),
         CD = ifelse(CD == "Other intestinal infections", "QA: Other intestinal infections", CD),
         CD = ifelse(CD == "Other infectious diseases", "QB: Other infectious diseases", CD),
         CD = ifelse(CD == "Lower respiratory infections", "RA: Lower respiratory infections", CD),
         CD = ifelse(CD == "Upper respiratory infections", "RB: Upper respiratory infections", CD),
         CD = ifelse(CD == "Otitis media", "RC: Otitis media", CD))

CD_women_edu <- conf_CD_women_edu %>% 
  select(-"...1") %>% 
  pivot_longer(c(5,11), names_to = "ESTIMATOR", values_to = "VALUE")%>% 
  pivot_longer(c(5,10), names_to = "CI_low_temp", values_to = "CI_LOW")%>% 
  pivot_longer(c(5,9), names_to = "CI_high_temp", values_to = "CI_HIGH")%>% 
  pivot_longer(c(5,8), names_to = "PVALUE_temp", values_to = "P_VALUE")%>% 
  pivot_longer(c(5,7), names_to = "QVALUE_temp", values_to = "QVALUE")%>% 
  pivot_longer(c(5,6), names_to = "BH_ACCEPT_temp", values_to = "BH_ACCEPT") %>% 
  filter(ESTIMATOR == "log_RR_udd1vs3" & CI_low_temp == "LOW_CI_udd1vs3" & CI_high_temp == "HIGH_CI_udd1vs3" & PVALUE_temp == "PVALUE_udd1vs3" & QVALUE_temp == "QVALUE_udd1vs3" & BH_ACCEPT_temp == "BH_accept_udd1vs3" |
           ESTIMATOR == "log_RR_udd2vs3" & CI_low_temp == "LOW_CI_udd2vs3" & CI_high_temp == "HIGH_CI_udd2vs3" & PVALUE_temp == "PVALUE_udd2vs3" & QVALUE_temp == "QVALUE_udd2vs3" & BH_ACCEPT_temp == "BH_accept_udd2vs3") %>% 
  select(CD, GBD_cause_name, order, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT, FEMALE_edu_prop) 


###### data: income & wowomen

conf_CD_women_inc <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/SupplementaryData16_CD_income_women_RR.xlsx")

conf_CD_women_inc <- left_join(conf_CD_women_inc, N_CD, by = "CD") %>% 
  mutate(GBD_cause_name = ifelse(CD == "Tuberculosis", "Tuberculosis", NA),
         GBD_cause_name = ifelse(CD == "Syphilis", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Chlamydia", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Gonorrhea", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other sexually transmitted diseases", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "HIV/AIDS", "HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Diarrheal diseases", "Diarrheal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Pertussis", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Poliomyelitis", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Diphtheria", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Measles", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Tetanus", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Meningitis", "Meningitis", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis A", "Hepatitis A", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis B", "Hepatitis B", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis C", "Hepatitis C", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Malaria", "Malaria", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trypanosomiasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Chagas' disease", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Schistosomiasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Leishmaniasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Lymphatic filariasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Onchocerciasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Leprosy", "Leprosy", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Dengue", "Dengue", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Japanese encephalitis", "Japanese ecephalitis", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trachoma", "Trachoma", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Ascariasis", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trichuriasis", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hookworm disease", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other intestinal infections", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other infectious diseases", "Other infectious diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Lower respiratory infections", "Respiratory infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Upper respiratory infections", "Respiratory infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Otitis media", "Respiratory infections", GBD_cause_name)) %>% 
  mutate(order = ifelse(GBD_cause_name == "Tuberculosis", 1, NA),
         order = ifelse(GBD_cause_name == "Sexually transmitted diseases excluding HIV/AIDS", 2, order),
         order = ifelse(GBD_cause_name == "HIV/AIDS", 3, order),
         order = ifelse(GBD_cause_name == "Diarrheal diseases", 4, order),
         order = ifelse(GBD_cause_name == "Childhood-cluster diseases", 5, order),
         order = ifelse(GBD_cause_name == "Meningitis", 6, order),
         order = ifelse(GBD_cause_name == "Hepatitis A", 7, order),
         order = ifelse(GBD_cause_name == "Hepatitis B", 8, order),
         order = ifelse(GBD_cause_name == "Hepatitis C", 9, order),
         order = ifelse(GBD_cause_name == "Malaria", 10, order),
         order = ifelse(GBD_cause_name == "Tropical-cluster diseases", 11, order),
         order = ifelse(GBD_cause_name == "Leprosy", 12, order),
         order = ifelse(GBD_cause_name == "Dengue", 13, order),
         order = ifelse(GBD_cause_name == "Japanese ecephalitis", 14, order),
         order = ifelse(GBD_cause_name == "Trachoma", 15, order),
         order = ifelse(GBD_cause_name == "Intestinal nomatode infections", 16, order),
         order = ifelse(GBD_cause_name == "Other infectious diseases", 17, order),
         order = ifelse(GBD_cause_name == "Respiratory infections", 18, order)) %>% 
  mutate(CD = ifelse(CD == "Tuberculosis", "AA: Tuberculosis", CD),
         CD = ifelse(CD == "Syphilis", "BA: Syphilis", CD),
         CD = ifelse(CD == "Chlamydia", "BB: Chlamydia", CD),
         CD = ifelse(CD == "Gonorrhea", "BC: Gonorrhea", CD),
         CD = ifelse(CD == "Other sexually transmitted diseases", "BD: Other sexually transmitted diseases", CD),
         CD = ifelse(CD == "HIV/AIDS", "CA: HIV/AIDS", CD),
         CD = ifelse(CD == "Diarrheal diseases", "DA: Diarrheal diseases", CD),
         CD = ifelse(CD == "Pertussis", "EA: Pertussis", CD),
         CD = ifelse(CD == "Poliomyelitis", "EB: Poliomyelitis", CD),
         CD = ifelse(CD == "Diphtheria", "EC: Diphtheria", CD),
         CD = ifelse(CD == "Measles", "ED: Measles", CD),
         CD = ifelse(CD == "Tetanus", "EF: Tetanus", CD),
         CD = ifelse(CD == "Meningitis", "FA: Meningitis", CD),
         CD = ifelse(CD == "Hepatitis A", "GA: Hepatitis A", CD),
         CD = ifelse(CD == "Hepatitis B", "HA: Hepatitis B", CD),
         CD = ifelse(CD == "Hepatitis C", "IA: Hepatitis C", CD),
         CD = ifelse(CD == "Malaria", "JA: Malaria", CD),
         CD = ifelse(CD == "Trypanosomiasis", "KA: Trypanosomiasis", CD),
         CD = ifelse(CD == "Chagas' disease", "KB: Chagas' disease", CD),
         CD = ifelse(CD == "Schistosomiasis", "KC: Schistosomiasis", CD),
         CD = ifelse(CD == "Leishmaniasis", "KD: Leishmaniasis", CD),
         CD = ifelse(CD == "Lymphatic filariasis", "KE: Lymphatic filariasis", CD),
         CD = ifelse(CD == "Onchocerciasis", "KF: Onchocerciasis", CD),
         CD = ifelse(CD == "Leprosy", "LA: Leprosy", CD),
         CD = ifelse(CD == "Dengue", "MA: Dengue", CD),
         CD = ifelse(CD == "Japanese encephalitis", "NA: Japanese encephalitis", CD),
         CD = ifelse(CD == "Trachoma", "OA: Trachoma", CD),
         CD = ifelse(CD == "Ascariasis", "PA: Ascariasis", CD),
         CD = ifelse(CD == "Trichuriasis", "PB: Trichuriasis", CD),
         CD = ifelse(CD == "Hookworm disease", "PC: Hookworm disease", CD),
         CD = ifelse(CD == "Other intestinal infections", "QA: Other intestinal infections", CD),
         CD = ifelse(CD == "Other infectious diseases", "QB: Other infectious diseases", CD),
         CD = ifelse(CD == "Lower respiratory infections", "RA: Lower respiratory infections", CD),
         CD = ifelse(CD == "Upper respiratory infections", "RB: Upper respiratory infections", CD),
         CD = ifelse(CD == "Otitis media", "RC: Otitis media", CD))


CD_women_inc <- conf_CD_women_inc %>% 
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
  select(CD, GBD_cause_name, order, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT, FEMALE_inc_prop) 



# Copenhagen_plot data - education

Copenhagen_CD_women_edu <- CD_women_edu %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))




# Copenhagen_plot data - income

Copenhagen_CD_women_inc <- CD_women_inc %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))




# Making list of diagnoses

diag_women_edu <- Copenhagen_CD_women_edu %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(CD) %>% 
  unique() %>% 
  as.data.frame()


diag_women_inc <- Copenhagen_CD_women_inc %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(CD) %>% 
  unique() %>% 
  as.data.frame()

diag_list <- rbind(diag_women_edu, diag_women_inc) %>% 
  unique()


#### Finishing data

Copenhagen_CD_women_edu <- left_join(diag_list, Copenhagen_CD_women_edu, by = "CD")

Copenhagen_CD_women_inc <- left_join(diag_list, Copenhagen_CD_women_inc, by = "CD")



# plot : eduation
group_colors <- viridis::viridis(n=2, begin = 0, end = 0.8)


p <- ggplot(Copenhagen_CD_women_edu, 
       aes(x = VALUE, y = as.factor(CD), color = as.factor(ESTIMATOR), size = FEMALE_edu_prop)) +
  geom_point(position = position_dodge(0.5))+ 
  geom_errorbar(aes(xmin=CI_LOW, xmax=CI_HIGH), position = position_dodge(0.5), size = 0.5)+
  theme_minimal() +
  theme_classic() + 
  facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
             labeller = labeller(order = c("1" = "Tuberculosis",
                                           "2" = "Sexually transmitted diseases excluding HIV/AIDS", 
                                           "3" = "HIV/AIDS",
                                           "4" = "Diarrheal diseases",
                                           "5" = "Childhood-cluster diseases",
                                           "6" = "Meningitis", 
                                           "7" = "Hepatitis A", 
                                           "8" = "Hepatitis B",
                                           "9" = "Hepatitis C", 
                                           "10" = "Malaria",
                                           "11" = "Tropical-cluster diseases",
                                           "12" = "Leprosy", 
                                           "13" = "Dengue",
                                           "14" = "Japanese ecephalitis",
                                           "15" = "Trachoma",
                                           "16" = "Intestinal nomatode infections",
                                           "17" = "Other infectious diseases",
                                           "18" = "Respiratory infections")))+
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  scale_color_manual(values = group_colors, name = "Education", labels = c("Low vs. high", "Medium vs. high")) +
  scale_size_continuous(name = "Percentage") +
  scale_alpha_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  scale_y_discrete(limits=rev, label = function(CD){
    CD %>% sub("....", "", .)
  }) +
  coord_cartesian(xlim = c(-0.5, 1))


# Making a line for each GBD_cause_name to indicate length/size of group
library(grid)
q <- ggplotGrob(p)
lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                gp=gpar(col="black", lwd=3))

for (k in grep("strip-l",q$layout$name)) {
  q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
}

grid.draw(q)



# plot: income

group_colors_inc <- viridis::viridis(n=3, begin = 0, end = 1)


p <- ggplot(Copenhagen_CD_women_inc, 
       aes(x = VALUE, y = as.factor(CD), color = as.factor(ESTIMATOR), size = FEMALE_inc_prop)) +
  geom_point(position = position_dodge(0.5))+ 
  geom_errorbar(aes(xmin=CI_LOW, xmax=CI_HIGH), position = position_dodge(0.5), size = 0.5)+
  theme_minimal() +
  theme_classic() + 
  facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
             labeller = labeller(order = c("1" = "Tuberculosis",
                                           "2" = "Sexually transmitted diseases excluding HIV/AIDS", 
                                           "3" = "HIV/AIDS",
                                           "4" = "Diarrheal diseases",
                                           "5" = "Childhood-cluster diseases",
                                           "6" = "Meningitis", 
                                           "7" = "Hepatitis A", 
                                           "8" = "Hepatitis B",
                                           "9" = "Hepatitis C", 
                                           "10" = "Malaria",
                                           "11" = "Tropical-cluster diseases",
                                           "12" = "Leprosy", 
                                           "13" = "Dengue",
                                           "14" = "Japanese ecephalitis",
                                           "15" = "Trachoma",
                                           "16" = "Intestinal nomatode infections",
                                           "17" = "Other infectious diseases",
                                           "18" = "Respiratory infections")))+
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  scale_color_manual(name = "Income", labels = c("Q1 vs. Q4", "Q2 vs. Q4", "Q3 vs. Q4"), values = group_colors_inc)+
  scale_size_continuous(name = "Percentage") +
  scale_alpha_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  scale_y_discrete(limits=rev, label = function(CD){
    CD %>% sub("....", "", .)
  }) +
  coord_cartesian(xlim = c(-0.5, 1))

# Making a line for each GBD_cause_name to indicate length/size of group
library(grid)
q <- ggplotGrob(p)
lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                gp=gpar(col="black", lwd=3))

for (k in grep("strip-l",q$layout$name)) {
  q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
}

grid.draw(q)



################### MEN ###################

###### data: education & men

conf_CD_men_edu <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/SupplementaryData10_CD_education_men_RR.xlsx")

conf_CD_men_edu <- left_join(conf_CD_men_edu, N_CD, by = "CD") %>% 
  mutate(GBD_cause_name = ifelse(CD == "Tuberculosis", "Tuberculosis", NA),
         GBD_cause_name = ifelse(CD == "Syphilis", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Chlamydia", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Gonorrhea", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other sexually transmitted diseases", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "HIV/AIDS", "HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Diarrheal diseases", "Diarrheal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Pertussis", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Poliomyelitis", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Diphtheria", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Measles", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Tetanus", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Meningitis", "Meningitis", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis A", "Hepatitis A", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis B", "Hepatitis B", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis C", "Hepatitis C", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Malaria", "Malaria", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trypanosomiasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Chagas' disease", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Schistosomiasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Leishmaniasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Lymphatic filariasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Onchocerciasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Leprosy", "Leprosy", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Dengue", "Dengue", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Japanese encephalitis", "Japanese ecephalitis", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trachoma", "Trachoma", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Ascariasis", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trichuriasis", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hookworm disease", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other intestinal infections", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other infectious diseases", "Other infectious diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Lower respiratory infections", "Respiratory infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Upper respiratory infections", "Respiratory infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Otitis media", "Respiratory infections", GBD_cause_name)) %>% 
  mutate(order = ifelse(GBD_cause_name == "Tuberculosis", 1, NA),
         order = ifelse(GBD_cause_name == "Sexually transmitted diseases excluding HIV/AIDS", 2, order),
         order = ifelse(GBD_cause_name == "HIV/AIDS", 3, order),
         order = ifelse(GBD_cause_name == "Diarrheal diseases", 4, order),
         order = ifelse(GBD_cause_name == "Childhood-cluster diseases", 5, order),
         order = ifelse(GBD_cause_name == "Meningitis", 6, order),
         order = ifelse(GBD_cause_name == "Hepatitis A", 7, order),
         order = ifelse(GBD_cause_name == "Hepatitis B", 8, order),
         order = ifelse(GBD_cause_name == "Hepatitis C", 9, order),
         order = ifelse(GBD_cause_name == "Malaria", 10, order),
         order = ifelse(GBD_cause_name == "Tropical-cluster diseases", 11, order),
         order = ifelse(GBD_cause_name == "Leprosy", 12, order),
         order = ifelse(GBD_cause_name == "Dengue", 13, order),
         order = ifelse(GBD_cause_name == "Japanese ecephalitis", 14, order),
         order = ifelse(GBD_cause_name == "Trachoma", 15, order),
         order = ifelse(GBD_cause_name == "Intestinal nomatode infections", 16, order),
         order = ifelse(GBD_cause_name == "Other infectious diseases", 17, order),
         order = ifelse(GBD_cause_name == "Respiratory infections", 18, order)) %>% 
  mutate(CD = ifelse(CD == "Tuberculosis", "AA: Tuberculosis", CD),
         CD = ifelse(CD == "Syphilis", "BA: Syphilis", CD),
         CD = ifelse(CD == "Chlamydia", "BB: Chlamydia", CD),
         CD = ifelse(CD == "Gonorrhea", "BC: Gonorrhea", CD),
         CD = ifelse(CD == "Other sexually transmitted diseases", "BD: Other sexually transmitted diseases", CD),
         CD = ifelse(CD == "HIV/AIDS", "CA: HIV/AIDS", CD),
         CD = ifelse(CD == "Diarrheal diseases", "DA: Diarrheal diseases", CD),
         CD = ifelse(CD == "Pertussis", "EA: Pertussis", CD),
         CD = ifelse(CD == "Poliomyelitis", "EB: Poliomyelitis", CD),
         CD = ifelse(CD == "Diphtheria", "EC: Diphtheria", CD),
         CD = ifelse(CD == "Measles", "ED: Measles", CD),
         CD = ifelse(CD == "Tetanus", "EF: Tetanus", CD),
         CD = ifelse(CD == "Meningitis", "FA: Meningitis", CD),
         CD = ifelse(CD == "Hepatitis A", "GA: Hepatitis A", CD),
         CD = ifelse(CD == "Hepatitis B", "HA: Hepatitis B", CD),
         CD = ifelse(CD == "Hepatitis C", "IA: Hepatitis C", CD),
         CD = ifelse(CD == "Malaria", "JA: Malaria", CD),
         CD = ifelse(CD == "Trypanosomiasis", "KA: Trypanosomiasis", CD),
         CD = ifelse(CD == "Chagas' disease", "KB: Chagas' disease", CD),
         CD = ifelse(CD == "Schistosomiasis", "KC: Schistosomiasis", CD),
         CD = ifelse(CD == "Leishmaniasis", "KD: Leishmaniasis", CD),
         CD = ifelse(CD == "Lymphatic filariasis", "KE: Lymphatic filariasis", CD),
         CD = ifelse(CD == "Onchocerciasis", "KF: Onchocerciasis", CD),
         CD = ifelse(CD == "Leprosy", "LA: Leprosy", CD),
         CD = ifelse(CD == "Dengue", "MA: Dengue", CD),
         CD = ifelse(CD == "Japanese encephalitis", "NA: Japanese encephalitis", CD),
         CD = ifelse(CD == "Trachoma", "OA: Trachoma", CD),
         CD = ifelse(CD == "Ascariasis", "PA: Ascariasis", CD),
         CD = ifelse(CD == "Trichuriasis", "PB: Trichuriasis", CD),
         CD = ifelse(CD == "Hookworm disease", "PC: Hookworm disease", CD),
         CD = ifelse(CD == "Other intestinal infections", "QA: Other intestinal infections", CD),
         CD = ifelse(CD == "Other infectious diseases", "QB: Other infectious diseases", CD),
         CD = ifelse(CD == "Lower respiratory infections", "RA: Lower respiratory infections", CD),
         CD = ifelse(CD == "Upper respiratory infections", "RB: Upper respiratory infections", CD),
         CD = ifelse(CD == "Otitis media", "RC: Otitis media", CD))


CD_men_edu <- conf_CD_men_edu %>% 
  select(-"...1") %>% 
  pivot_longer(c(5,11), names_to = "ESTIMATOR", values_to = "VALUE")%>% 
  pivot_longer(c(5,10), names_to = "CI_low_temp", values_to = "CI_LOW")%>% 
  pivot_longer(c(5,9), names_to = "CI_high_temp", values_to = "CI_HIGH")%>% 
  pivot_longer(c(5,8), names_to = "PVALUE_temp", values_to = "P_VALUE")%>% 
  pivot_longer(c(5,7), names_to = "QVALUE_temp", values_to = "QVALUE")%>% 
  pivot_longer(c(5,6), names_to = "BH_ACCEPT_temp", values_to = "BH_ACCEPT") %>% 
  filter(ESTIMATOR == "log_RR_udd1vs3" & CI_low_temp == "LOW_CI_udd1vs3" & CI_high_temp == "HIGH_CI_udd1vs3" & PVALUE_temp == "PVALUE_udd1vs3" & QVALUE_temp == "QVALUE_udd1vs3" & BH_ACCEPT_temp == "BH_accept_udd1vs3" |
           ESTIMATOR == "log_RR_udd2vs3" & CI_low_temp == "LOW_CI_udd2vs3" & CI_high_temp == "HIGH_CI_udd2vs3" & PVALUE_temp == "PVALUE_udd2vs3" & QVALUE_temp == "QVALUE_udd2vs3" & BH_ACCEPT_temp == "BH_accept_udd2vs3") %>% 
  select(CD, GBD_cause_name, order, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT, MALE_edu_prop) 


###### data: income & women

conf_CD_men_inc <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/SupplementaryData14_CD_income_men_RR.xlsx")

conf_CD_men_inc <- left_join(conf_CD_men_inc, N_CD, by = "CD") %>% 
  mutate(GBD_cause_name = ifelse(CD == "Tuberculosis", "Tuberculosis", NA),
         GBD_cause_name = ifelse(CD == "Syphilis", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Chlamydia", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Gonorrhea", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other sexually transmitted diseases", "Sexually transmitted diseases excluding HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "HIV/AIDS", "HIV/AIDS", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Diarrheal diseases", "Diarrheal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Pertussis", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Poliomyelitis", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Diphtheria", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Measles", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Tetanus", "Childhood-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Meningitis", "Meningitis", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis A", "Hepatitis A", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis B", "Hepatitis B", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hepatitis C", "Hepatitis C", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Malaria", "Malaria", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trypanosomiasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Chagas' disease", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Schistosomiasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Leishmaniasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Lymphatic filariasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Onchocerciasis", "Tropical-cluster diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Leprosy", "Leprosy", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Dengue", "Dengue", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Japanese encephalitis", "Japanese ecephalitis", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trachoma", "Trachoma", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Ascariasis", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Trichuriasis", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Hookworm disease", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other intestinal infections", "Intestinal nomatode infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Other infectious diseases", "Other infectious diseases", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Lower respiratory infections", "Respiratory infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Upper respiratory infections", "Respiratory infections", GBD_cause_name),
         GBD_cause_name = ifelse(CD == "Otitis media", "Respiratory infections", GBD_cause_name)) %>% 
  mutate(order = ifelse(GBD_cause_name == "Tuberculosis", 1, NA),
         order = ifelse(GBD_cause_name == "Sexually transmitted diseases excluding HIV/AIDS", 2, order),
         order = ifelse(GBD_cause_name == "HIV/AIDS", 3, order),
         order = ifelse(GBD_cause_name == "Diarrheal diseases", 4, order),
         order = ifelse(GBD_cause_name == "Childhood-cluster diseases", 5, order),
         order = ifelse(GBD_cause_name == "Meningitis", 6, order),
         order = ifelse(GBD_cause_name == "Hepatitis A", 7, order),
         order = ifelse(GBD_cause_name == "Hepatitis B", 8, order),
         order = ifelse(GBD_cause_name == "Hepatitis C", 9, order),
         order = ifelse(GBD_cause_name == "Malaria", 10, order),
         order = ifelse(GBD_cause_name == "Tropical-cluster diseases", 11, order),
         order = ifelse(GBD_cause_name == "Leprosy", 12, order),
         order = ifelse(GBD_cause_name == "Dengue", 13, order),
         order = ifelse(GBD_cause_name == "Japanese ecephalitis", 14, order),
         order = ifelse(GBD_cause_name == "Trachoma", 15, order),
         order = ifelse(GBD_cause_name == "Intestinal nomatode infections", 16, order),
         order = ifelse(GBD_cause_name == "Other infectious diseases", 17, order),
         order = ifelse(GBD_cause_name == "Respiratory infections", 18, order)) %>% 
  mutate(CD = ifelse(CD == "Tuberculosis", "AA: Tuberculosis", CD),
         CD = ifelse(CD == "Syphilis", "BA: Syphilis", CD),
         CD = ifelse(CD == "Chlamydia", "BB: Chlamydia", CD),
         CD = ifelse(CD == "Gonorrhea", "BC: Gonorrhea", CD),
         CD = ifelse(CD == "Other sexually transmitted diseases", "BD: Other sexually transmitted diseases", CD),
         CD = ifelse(CD == "HIV/AIDS", "CA: HIV/AIDS", CD),
         CD = ifelse(CD == "Diarrheal diseases", "DA: Diarrheal diseases", CD),
         CD = ifelse(CD == "Pertussis", "EA: Pertussis", CD),
         CD = ifelse(CD == "Poliomyelitis", "EB: Poliomyelitis", CD),
         CD = ifelse(CD == "Diphtheria", "EC: Diphtheria", CD),
         CD = ifelse(CD == "Measles", "ED: Measles", CD),
         CD = ifelse(CD == "Tetanus", "EF: Tetanus", CD),
         CD = ifelse(CD == "Meningitis", "FA: Meningitis", CD),
         CD = ifelse(CD == "Hepatitis A", "GA: Hepatitis A", CD),
         CD = ifelse(CD == "Hepatitis B", "HA: Hepatitis B", CD),
         CD = ifelse(CD == "Hepatitis C", "IA: Hepatitis C", CD),
         CD = ifelse(CD == "Malaria", "JA: Malaria", CD),
         CD = ifelse(CD == "Trypanosomiasis", "KA: Trypanosomiasis", CD),
         CD = ifelse(CD == "Chagas' disease", "KB: Chagas' disease", CD),
         CD = ifelse(CD == "Schistosomiasis", "KC: Schistosomiasis", CD),
         CD = ifelse(CD == "Leishmaniasis", "KD: Leishmaniasis", CD),
         CD = ifelse(CD == "Lymphatic filariasis", "KE: Lymphatic filariasis", CD),
         CD = ifelse(CD == "Onchocerciasis", "KF: Onchocerciasis", CD),
         CD = ifelse(CD == "Leprosy", "LA: Leprosy", CD),
         CD = ifelse(CD == "Dengue", "MA: Dengue", CD),
         CD = ifelse(CD == "Japanese encephalitis", "NA: Japanese encephalitis", CD),
         CD = ifelse(CD == "Trachoma", "OA: Trachoma", CD),
         CD = ifelse(CD == "Ascariasis", "PA: Ascariasis", CD),
         CD = ifelse(CD == "Trichuriasis", "PB: Trichuriasis", CD),
         CD = ifelse(CD == "Hookworm disease", "PC: Hookworm disease", CD),
         CD = ifelse(CD == "Other intestinal infections", "QA: Other intestinal infections", CD),
         CD = ifelse(CD == "Other infectious diseases", "QB: Other infectious diseases", CD),
         CD = ifelse(CD == "Lower respiratory infections", "RA: Lower respiratory infections", CD),
         CD = ifelse(CD == "Upper respiratory infections", "RB: Upper respiratory infections", CD),
         CD = ifelse(CD == "Otitis media", "RC: Otitis media", CD))


CD_men_inc <- conf_CD_men_inc %>% 
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
  select(CD, GBD_cause_name, order, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT, MALE_inc_prop) 



# Copenhagen_plot data - education

Copenhagen_CD_men_edu <- CD_men_edu %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))




# Copenhagen_plot data - income

Copenhagen_CD_men_inc <- CD_men_inc %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))




# Making list of diagnoses

diag_men_edu <- Copenhagen_CD_men_edu %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(CD) %>% 
  unique() %>% 
  as.data.frame()


diag_men_inc <- Copenhagen_CD_men_inc %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(CD) %>% 
  unique() %>% 
  as.data.frame()

diag_list <- rbind(diag_men_edu, diag_men_inc) %>% 
  unique()


#### Finishing data

Copenhagen_CD_men_edu <- left_join(diag_list, Copenhagen_CD_men_edu, by = "CD")

Copenhagen_CD_men_inc <- left_join(diag_list, Copenhagen_CD_men_inc, by = "CD")



# plot : eduation
group_colors <- viridis::viridis(n=2, begin = 0, end = 0.8)


p <- ggplot(Copenhagen_CD_men_edu, 
       aes(x = VALUE, y = as.factor(CD), color = as.factor(ESTIMATOR), size = MALE_edu_prop)) +
  geom_point(position = position_dodge(0.5))+ 
  geom_errorbar(aes(xmin=CI_LOW, xmax=CI_HIGH), position = position_dodge(0.5), size = 0.5)+
  theme_minimal() +
  theme_classic() + 
  facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
             labeller = labeller(order = c("1" = "Tuberculosis",
                                           "2" = "Sexually transmitted diseases excluding HIV/AIDS", 
                                           "3" = "HIV/AIDS",
                                           "4" = "Diarrheal diseases",
                                           "5" = "Childhood-cluster diseases",
                                           "6" = "Meningitis", 
                                           "7" = "Hepatitis A", 
                                           "8" = "Hepatitis B",
                                           "9" = "Hepatitis C", 
                                           "10" = "Malaria",
                                           "11" = "Tropical-cluster diseases",
                                           "12" = "Leprosy", 
                                           "13" = "Dengue",
                                           "14" = "Japanese ecephalitis",
                                           "15" = "Trachoma",
                                           "16" = "Intestinal nomatode infections",
                                           "17" = "Other infectious diseases",
                                           "18" = "Respiratory infections")))+
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  scale_color_manual(values = group_colors, name = "Education", labels = c("Low vs. high", "Medium vs. high")) +
  scale_size_continuous(name = "Percentage") +
  scale_alpha_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  scale_y_discrete(limits=rev, label = function(CD){
    CD %>% sub("....", "", .)
  }) +
  coord_cartesian(xlim = c(-0.5, 1))

# Making a line for each GBD_cause_name to indicate length/size of group
library(grid)
q <- ggplotGrob(p)
lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                gp=gpar(col="black", lwd=3))

for (k in grep("strip-l",q$layout$name)) {
  q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
}

grid.draw(q)



# plot: income

group_colors_inc <- viridis::viridis(n=3, begin = 0, end = 1)


p <- ggplot(Copenhagen_CD_men_inc, 
       aes(x = VALUE, y = as.factor(CD), color = as.factor(ESTIMATOR), size = MALE_inc_prop)) +
  geom_point(position = position_dodge(0.5))+ 
  geom_errorbar(aes(xmin=CI_LOW, xmax=CI_HIGH), position = position_dodge(0.5), size = 0.5)+
  theme_minimal() +
  theme_classic() + 
  facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
             labeller = labeller(order = c("1" = "Tuberculosis",
                                           "2" = "Sexually transmitted diseases excluding HIV/AIDS", 
                                           "3" = "HIV/AIDS",
                                           "4" = "Diarrheal diseases",
                                           "5" = "Childhood-cluster diseases",
                                           "6" = "Meningitis", 
                                           "7" = "Hepatitis A", 
                                           "8" = "Hepatitis B",
                                           "9" = "Hepatitis C", 
                                           "10" = "Malaria",
                                           "11" = "Tropical-cluster diseases",
                                           "12" = "Leprosy", 
                                           "13" = "Dengue",
                                           "14" = "Japanese ecephalitis",
                                           "15" = "Trachoma",
                                           "16" = "Intestinal nomatode infections",
                                           "17" = "Other infectious diseases",
                                           "18" = "Respiratory infections")))+
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  scale_color_manual(name = "Income", labels = c("Q1 vs. Q4", "Q2 vs. Q4", "Q3 vs. Q4"), values = group_colors_inc)+
  scale_size_continuous(name = "Percentage") +
  scale_alpha_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  scale_y_discrete(limits=rev, label = function(CD){
    CD %>% sub("....", "", .)
  }) +
  coord_cartesian(xlim = c(-0.5, 1))

# Making a line for each GBD_cause_name to indicate length/size of group
library(grid)
q <- ggplotGrob(p)
lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                gp=gpar(col="black", lwd=3))

for (k in grep("strip-l",q$layout$name)) {
  q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
}

grid.draw(q)


