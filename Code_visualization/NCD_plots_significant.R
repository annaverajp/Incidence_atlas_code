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
N_NCD <- dbReadTable(conn_inci, "N_CASES_NCD")%>% 
  mutate(FEMALE_edu_prop = (FEMALE_edu_cases/sum(FEMALE_edu_cases)) * 100,
         MALE_edu_prop = (MALE_edu_cases/sum(MALE_edu_cases)) * 100,
         FEMALE_inc_prop = (FEMALE_inc_cases/sum(FEMALE_inc_cases)) * 100,
         MALE_inc_prop = (MALE_inc_cases/sum(MALE_inc_cases)) * 100)



################### WOMEN ###################

###### data: education & men

conf_NCD_women_edu <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/SupplementaryData20_NCD_education_women_RR.xlsx")

conf_NCD_women_edu <- left_join(conf_NCD_women_edu, N_NCD, by = "NCD") %>% 
  mutate(NCD = ifelse(NCD == "Mouth and oropharynx cancers", "A1: Mouth and oropharynx cancers", NCD),
         NCD = ifelse(NCD == "Esophageal cancer", "AB: Esophageal cancer", NCD),
         NCD = ifelse(NCD == "Stomach cancer", "AC: Stomach cancer", NCD),
         NCD = ifelse(NCD == "Colon and rectal cancers", "AD: Colon and rectal cancers", NCD),
         NCD = ifelse(NCD == "Liver cancer", "AE: Liver cancer", NCD),
         NCD = ifelse(NCD == "Pancreas cancer", "AF: Pancreas cancer", NCD),
         NCD = ifelse(NCD == "Trachea, bronchus, and lung cancers", "AG: Trachea, bronchus, and lung cancers", NCD),
         NCD = ifelse(NCD == "Melanoma and other skin cancers", "AH: Melanoma and other skin cancers", NCD),
         NCD = ifelse(NCD == "Breast cancer", "AI: Breast cancer", NCD),
         NCD = ifelse(NCD == "Cervix uteri cancer", "AJ: Cervix uteri cancer", NCD),
         NCD = ifelse(NCD == "Corpus uteri cancer", "AK: Corpus uteri cancer", NCD),
         NCD = ifelse(NCD == "Ovarian cancer", "AL: Ovarian cancer", NCD),
         NCD = ifelse(NCD == "Prostate cancer", "AM: Prostate cancer", NCD),
         NCD = ifelse(NCD == "Bladder cancer", "AN: Bladder cancer", NCD),
         NCD = ifelse(NCD == "Lymphomas and multiple myeloma", "AO: Lymphomas and multiple myeloma", NCD),
         NCD = ifelse(NCD == "Leukemia", "AP: Leukemia", NCD),
         NCD = ifelse(NCD == "Other malignant neoplasms", "AQ: Other malignant neoplasms", NCD),
         NCD = ifelse(NCD == "Other neoplasms", "BA: Other neoplasms", NCD),
         NCD = ifelse(NCD == "Diabetes mellitus", "CA: Diabetes mellitus", NCD),
         NCD = ifelse(NCD == "Endocrine disorders", "DA: Endocrine disorders", NCD),
         NCD = ifelse(NCD == "Unipolar depressive disorders", "EA: Unipolar depressive disorders", NCD),
         NCD = ifelse(NCD == "Bipolar affective disorders", "EB: Bipolar affective disorders", NCD),
         NCD = ifelse(NCD == "Schizophrenia", "EC: Schizophrenia", NCD),
         NCD = ifelse(NCD == "Epilepsi", "ED: Epilepsi", NCD),
         NCD = ifelse(NCD == "Alcohol use disorders", "EE: Alcohol use disorders", NCD),
         NCD = ifelse(NCD == "Alzheimer's disease and other dementias", "EF: Alzheimer's disease and other dementias", NCD),
         NCD = ifelse(NCD == "Parkinson's disease", "EG: Parkinson's disease", NCD),
         NCD = ifelse(NCD == "Multiple schlerosis", "EH: Multiple schlerosis", NCD),
         NCD = ifelse(NCD == "Drug use disorders", "EI: Drug use disorders", NCD),
         NCD = ifelse(NCD == "Reaction to severe stress, and adjustment disorders", "EJ: Reaction to severe stress", NCD),
         NCD = ifelse(NCD == "Obsessive-compulsive disorder", "EK: Obsessive-compulsive disorder", NCD),
         NCD = ifelse(NCD == "Anxiety disorders", "EL: Anxiety disorders", NCD),
         NCD = ifelse(NCD == "Insomnia (primary)", "EM: Insomnia (primary)", NCD),
         NCD = ifelse(NCD == "Migraine", "EN: Migraine", NCD),
         NCD = ifelse(NCD == "Other neuropsychiatric disorders", "EO: Other neuropsychiatric disorders", NCD),
         NCD = ifelse(NCD == "Glaucoma", "FA: Glaucoma", NCD),
         NCD = ifelse(NCD == "Cataracts", "FB: Cataracts", NCD),
         NCD = ifelse(NCD == "Hearing loss, adult onset", "FC: Hearing loss, adult onset", NCD),
         NCD = ifelse(NCD == "Other sense organ disorders", "FD: Other sense organ disorders", NCD),
         NCD = ifelse(NCD == "Rheumatic heart disease", "GA: Rheumatic heart disease", NCD),
         NCD = ifelse(NCD == "Hypertensive heart disease", "GB: Hypertensive heart disease", NCD),
         NCD = ifelse(NCD == "Ischemic heart disease", "GC: Ischemic heart disease", NCD),
         NCD = ifelse(NCD == "Cerebrovascular disease", "GD: Cerebrovascular disease", NCD),
         NCD = ifelse(NCD == "Inflammatory heart diseases", "GE: Inflammatory heart diseases", NCD),
         NCD = ifelse(NCD == "Other cardiovascular diseases", "GF: Other cardiovascular diseases", NCD),
         NCD = ifelse(NCD == "Chronic obstructive pulmonary disease", "HA: Chronic obstructive pulmonary disease", NCD),
         NCD = ifelse(NCD == "Asthma", "HB: Asthma", NCD),
         NCD = ifelse(NCD == "Other respiratory diseases", "HC: Other respiratory diseases", NCD),
         NCD = ifelse(NCD == "Peptic ulcer disease", "IA: Peptic ulcer disease", NCD),
         NCD = ifelse(NCD == "Cirrhosis of the liver", "IB: Cirrhosis of the liver", NCD),
         NCD = ifelse(NCD == "Appendicitis", "IC: Appendicitis", NCD),
         NCD = ifelse(NCD == "Other digestive diseases", "ID: Other digestive diseases", NCD),
         NCD = ifelse(NCD == "Nephritis and neophrosis", "JA: Nephritis and neophrosis", NCD),
         NCD = ifelse(NCD == "Benign prostatic hypertrophy", "JB: Benign prostatic hypertrophy", NCD),
         NCD = ifelse(NCD == "Other genitourinary system diseases", "JC: Other genitourinary system diseases", NCD),
         NCD = ifelse(NCD == "Skin diseases", "KA: Skin diseases", NCD),
         NCD = ifelse(NCD == "Rheumatoid arthritis", "LA: Rheumatoid arthritis", NCD),
         NCD = ifelse(NCD == "Osteoarthritis", "LB: Osteoarthritis", NCD),
         NCD = ifelse(NCD == "Gout", "LC: Gout", NCD),
         NCD = ifelse(NCD == "Low back pain", "LD: Low back pain", NCD),
         NCD = ifelse(NCD == "Other muscuskeletal disorders", "LE: Other muscuskeletal disorders", NCD),
         NCD = ifelse(NCD == "Dental caries", "MA: Dental caries", NCD),
         NCD = ifelse(NCD == "Periodontal disease", "MB: Periodontal disease", NCD),
         NCD = ifelse(NCD == "Other oral diseases", "MC: Other oral diseases", NCD)) %>% 
  mutate(GBD_cause_name = ifelse(NCD == "A1: Mouth and oropharynx cancers", "Malignant neoplasms", NA),
         GBD_cause_name = ifelse(NCD == "AB: Esophageal cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AC: Stomach cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AD: Colon and rectal cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AE: Liver cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AF: Pancreas cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AG: Trachea, bronchus, and lung cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AH: Melanoma and other skin cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AI: Breast cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AJ: Cervix uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AK: Corpus uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AL: Ovarian cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AM: Prostate cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AN: Bladder cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AO: Lymphomas and multiple myeloma cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AP: Leukemia", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AQ: Other malignant neoplasms", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "BA: Other neoplasms", "Other neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "CA: Diabetes mellitus", "Diabetes mellitus", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "DA: Endocrine disorders", "Endocrine disorders", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EA: Unipolar depressive disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EB: Bipolar affective disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EC: Schizophrenia", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "ED: Epilepsi", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EE: Alcohol use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EF: Alzheimer's disease and other dementias", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EG: Parkinson's disease", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EH: Multiple sclerosis", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EI: Drug use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EJ: Reaction to severe stress", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EK: Obsessive-compulsive disorder", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EL: Anxiety disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EM: Insomnia (primary)", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EN: Migraine", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EO: Other neuropsychiatric disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FA: Glaucoma", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FB: Cataracts", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FC: Hearing loss, adult onset", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FD: Other sense organ disorders", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GA: Rheumatic heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GB: Hypertensive heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GC: Ischemic heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GD: Cerebrovascular disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GE: Inflammatory heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GF: Other cardiovascular diseases", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HA: Chronic obstructive pulmonary disease", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HB: Asthma", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HC: Other respiratory diseases", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IA: Peptic ulcer disease", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IB: Cirrhosis of the liver", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IC: Appendicitis", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "ID: Other digestive diseases", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JA: Nephritis and nephrosis", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JB: Benign prostatic hypertrophy", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JC: Other genitourinary system diseases", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "KA: Skin diseases", "Skin diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LA: Rheumatoid arthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LB: Oesteoarthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LC: Gout", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LD: Low back pain", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LE: Other musculoskeletal disorders", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MA: Dental caries", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MB: Periodontal disease", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MC: Other oral diseases", "Oral conditions", GBD_cause_name)) %>% 
  mutate(order = ifelse(GBD_cause_name == "Malignant neoplasms", 1, NA),
         order = ifelse(GBD_cause_name == "Other neoplasms", 2, order),
         order = ifelse(GBD_cause_name == "Diabetes mellitus", 3, order),
         order = ifelse(GBD_cause_name == "Endocrine disorders", 4, order),
         order = ifelse(GBD_cause_name == "Neuropsychiatric conditions", 5, order),
         order = ifelse(GBD_cause_name == "Sense organ diseases", 6, order),
         order = ifelse(GBD_cause_name == "Cardiovascular diseases", 7, order),
         order = ifelse(GBD_cause_name == "Respiratory diseases", 8, order),
         order = ifelse(GBD_cause_name == "Digestive diseases", 9, order),
         order = ifelse(GBD_cause_name == "Genitourinary diseases", 10, order),
         order = ifelse(GBD_cause_name == "Skin diseases", 11, order),
         order = ifelse(GBD_cause_name == "Musculoskeletal diseases", 12, order),
         order = ifelse(GBD_cause_name == "Oral conditions", 13, order))


NCD_women_edu <- conf_NCD_women_edu %>% 
  select(-"...1") %>% 
  pivot_longer(c(5,11), names_to = "ESTIMATOR", values_to = "VALUE")%>% 
  pivot_longer(c(5,10), names_to = "CI_low_temp", values_to = "CI_LOW")%>% 
  pivot_longer(c(5,9), names_to = "CI_high_temp", values_to = "CI_HIGH")%>% 
  pivot_longer(c(5,8), names_to = "PVALUE_temp", values_to = "P_VALUE")%>% 
  pivot_longer(c(5,7), names_to = "QVALUE_temp", values_to = "QVALUE")%>% 
  pivot_longer(c(5,6), names_to = "BH_ACCEPT_temp", values_to = "BH_ACCEPT") %>% 
  filter(ESTIMATOR == "log_RR_udd1vs3" & CI_low_temp == "LOW_CI_udd1vs3" & CI_high_temp == "HIGH_CI_udd1vs3" & PVALUE_temp == "PVALUE_udd1vs3" & QVALUE_temp == "QVALUE_udd1vs3" & BH_ACCEPT_temp == "BH_accept_udd1vs3" |
           ESTIMATOR == "log_RR_udd2vs3" & CI_low_temp == "LOW_CI_udd2vs3" & CI_high_temp == "HIGH_CI_udd2vs3" & PVALUE_temp == "PVALUE_udd2vs3" & QVALUE_temp == "QVALUE_udd2vs3" & BH_ACCEPT_temp == "BH_accept_udd2vs3") %>% 
  select(NCD, GBD_cause_name, order, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT, FEMALE_edu_prop) 


###### data: income & wowomen

conf_NCD_women_inc <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/SupplementaryData24_NCD_income_women_RR.xlsx")

conf_NCD_women_inc <- left_join(conf_NCD_women_inc, N_NCD, by = "NCD") %>% 
  mutate(NCD = ifelse(NCD == "Mouth and oropharynx cancers", "A1: Mouth and oropharynx cancers", NCD),
         NCD = ifelse(NCD == "Esophageal cancer", "AB: Esophageal cancer", NCD),
         NCD = ifelse(NCD == "Stomach cancer", "AC: Stomach cancer", NCD),
         NCD = ifelse(NCD == "Colon and rectal cancers", "AD: Colon and rectal cancers", NCD),
         NCD = ifelse(NCD == "Liver cancer", "AE: Liver cancer", NCD),
         NCD = ifelse(NCD == "Pancreas cancer", "AF: Pancreas cancer", NCD),
         NCD = ifelse(NCD == "Trachea, bronchus, and lung cancers", "AG: Trachea, bronchus, and lung cancers", NCD),
         NCD = ifelse(NCD == "Melanoma and other skin cancers", "AH: Melanoma and other skin cancers", NCD),
         NCD = ifelse(NCD == "Breast cancer", "AI: Breast cancer", NCD),
         NCD = ifelse(NCD == "Cervix uteri cancer", "AJ: Cervix uteri cancer", NCD),
         NCD = ifelse(NCD == "Corpus uteri cancer", "AK: Corpus uteri cancer", NCD),
         NCD = ifelse(NCD == "Ovarian cancer", "AL: Ovarian cancer", NCD),
         NCD = ifelse(NCD == "Prostate cancer", "AM: Prostate cancer", NCD),
         NCD = ifelse(NCD == "Bladder cancer", "AN: Bladder cancer", NCD),
         NCD = ifelse(NCD == "Lymphomas and multiple myeloma", "AO: Lymphomas and multiple myeloma", NCD),
         NCD = ifelse(NCD == "Leukemia", "AP: Leukemia", NCD),
         NCD = ifelse(NCD == "Other malignant neoplasms", "AQ: Other malignant neoplasms", NCD),
         NCD = ifelse(NCD == "Other neoplasms", "BA: Other neoplasms", NCD),
         NCD = ifelse(NCD == "Diabetes mellitus", "CA: Diabetes mellitus", NCD),
         NCD = ifelse(NCD == "Endocrine disorders", "DA: Endocrine disorders", NCD),
         NCD = ifelse(NCD == "Unipolar depressive disorders", "EA: Unipolar depressive disorders", NCD),
         NCD = ifelse(NCD == "Bipolar affective disorders", "EB: Bipolar affective disorders", NCD),
         NCD = ifelse(NCD == "Schizophrenia", "EC: Schizophrenia", NCD),
         NCD = ifelse(NCD == "Epilepsi", "ED: Epilepsi", NCD),
         NCD = ifelse(NCD == "Alcohol use disorders", "EE: Alcohol use disorders", NCD),
         NCD = ifelse(NCD == "Alzheimer's disease and other dementias", "EF: Alzheimer's disease and other dementias", NCD),
         NCD = ifelse(NCD == "Parkinson's disease", "EG: Parkinson's disease", NCD),
         NCD = ifelse(NCD == "Multiple schlerosis", "EH: Multiple schlerosis", NCD),
         NCD = ifelse(NCD == "Drug use disorders", "EI: Drug use disorders", NCD),
         NCD = ifelse(NCD == "Reaction to severe stress, and adjustment disorders", "EJ: Reaction to severe stress", NCD),
         NCD = ifelse(NCD == "Obsessive-compulsive disorder", "EK: Obsessive-compulsive disorder", NCD),
         NCD = ifelse(NCD == "Anxiety disorders", "EL: Anxiety disorders", NCD),
         NCD = ifelse(NCD == "Insomnia (primary)", "EM: Insomnia (primary)", NCD),
         NCD = ifelse(NCD == "Migraine", "EN: Migraine", NCD),
         NCD = ifelse(NCD == "Other neuropsychiatric disorders", "EO: Other neuropsychiatric disorders", NCD),
         NCD = ifelse(NCD == "Glaucoma", "FA: Glaucoma", NCD),
         NCD = ifelse(NCD == "Cataracts", "FB: Cataracts", NCD),
         NCD = ifelse(NCD == "Hearing loss, adult onset", "FC: Hearing loss, adult onset", NCD),
         NCD = ifelse(NCD == "Other sense organ disorders", "FD: Other sense organ disorders", NCD),
         NCD = ifelse(NCD == "Rheumatic heart disease", "GA: Rheumatic heart disease", NCD),
         NCD = ifelse(NCD == "Hypertensive heart disease", "GB: Hypertensive heart disease", NCD),
         NCD = ifelse(NCD == "Ischemic heart disease", "GC: Ischemic heart disease", NCD),
         NCD = ifelse(NCD == "Cerebrovascular disease", "GD: Cerebrovascular disease", NCD),
         NCD = ifelse(NCD == "Inflammatory heart diseases", "GE: Inflammatory heart diseases", NCD),
         NCD = ifelse(NCD == "Other cardiovascular diseases", "GF: Other cardiovascular diseases", NCD),
         NCD = ifelse(NCD == "Chronic obstructive pulmonary disease", "HA: Chronic obstructive pulmonary disease", NCD),
         NCD = ifelse(NCD == "Asthma", "HB: Asthma", NCD),
         NCD = ifelse(NCD == "Other respiratory diseases", "HC: Other respiratory diseases", NCD),
         NCD = ifelse(NCD == "Peptic ulcer disease", "IA: Peptic ulcer disease", NCD),
         NCD = ifelse(NCD == "Cirrhosis of the liver", "IB: Cirrhosis of the liver", NCD),
         NCD = ifelse(NCD == "Appendicitis", "IC: Appendicitis", NCD),
         NCD = ifelse(NCD == "Other digestive diseases", "ID: Other digestive diseases", NCD),
         NCD = ifelse(NCD == "Nephritis and neophrosis", "JA: Nephritis and neophrosis", NCD),
         NCD = ifelse(NCD == "Benign prostatic hypertrophy", "JB: Benign prostatic hypertrophy", NCD),
         NCD = ifelse(NCD == "Other genitourinary system diseases", "JC: Other genitourinary system diseases", NCD),
         NCD = ifelse(NCD == "Skin diseases", "KA: Skin diseases", NCD),
         NCD = ifelse(NCD == "Rheumatoid arthritis", "LA: Rheumatoid arthritis", NCD),
         NCD = ifelse(NCD == "Osteoarthritis", "LB: Osteoarthritis", NCD),
         NCD = ifelse(NCD == "Gout", "LC: Gout", NCD),
         NCD = ifelse(NCD == "Low back pain", "LD: Low back pain", NCD),
         NCD = ifelse(NCD == "Other muscuskeletal disorders", "LE: Other muscuskeletal disorders", NCD),
         NCD = ifelse(NCD == "Dental caries", "MA: Dental caries", NCD),
         NCD = ifelse(NCD == "Periodontal disease", "MB: Periodontal disease", NCD),
         NCD = ifelse(NCD == "Other oral diseases", "MC: Other oral diseases", NCD)) %>% 
  mutate(GBD_cause_name = ifelse(NCD == "A1: Mouth and oropharynx cancers", "Malignant neoplasms", NA),
         GBD_cause_name = ifelse(NCD == "AB: Esophageal cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AC: Stomach cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AD: Colon and rectal cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AE: Liver cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AF: Pancreas cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AG: Trachea, bronchus, and lung cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AH: Melanoma and other skin cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AI: Breast cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AJ: Cervix uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AK: Corpus uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AL: Ovarian cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AM: Prostate cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AN: Bladder cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AO: Lymphomas and multiple myeloma cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AP: Leukemia", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AQ: Other malignant neoplasms", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "BA: Other neoplasms", "Other neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "CA: Diabetes mellitus", "Diabetes mellitus", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "DA: Endocrine disorders", "Endocrine disorders", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EA: Unipolar depressive disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EB: Bipolar affective disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EC: Schizophrenia", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "ED: Epilepsi", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EE: Alcohol use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EF: Alzheimer's disease and other dementias", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EG: Parkinson's disease", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EH: Multiple sclerosis", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EI: Drug use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EJ: Reaction to severe stress", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EK: Obsessive-compulsive disorder", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EL: Anxiety disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EM: Insomnia (primary)", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EN: Migraine", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EO: Other neuropsychiatric disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FA: Glaucoma", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FB: Cataracts", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FC: Hearing loss, adult onset", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FD: Other sense organ disorders", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GA: Rheumatic heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GB: Hypertensive heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GC: Ischemic heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GD: Cerebrovascular disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GE: Inflammatory heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GF: Other cardiovascular diseases", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HA: Chronic obstructive pulmonary disease", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HB: Asthma", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HC: Other respiratory diseases", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IA: Peptic ulcer disease", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IB: Cirrhosis of the liver", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IC: Appendicitis", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "ID: Other digestive diseases", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JA: Nephritis and nephrosis", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JB: Benign prostatic hypertrophy", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JC: Other genitourinary system diseases", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "KA: Skin diseases", "Skin diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LA: Rheumatoid arthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LB: Oesteoarthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LC: Gout", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LD: Low back pain", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LE: Other musculoskeletal disorders", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MA: Dental caries", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MB: Periodontal disease", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MC: Other oral diseases", "Oral conditions", GBD_cause_name)) %>% 
  mutate(order = ifelse(GBD_cause_name == "Malignant neoplasms", 1, NA),
         order = ifelse(GBD_cause_name == "Other neoplasms", 2, order),
         order = ifelse(GBD_cause_name == "Diabetes mellitus", 3, order),
         order = ifelse(GBD_cause_name == "Endocrine disorders", 4, order),
         order = ifelse(GBD_cause_name == "Neuropsychiatric conditions", 5, order),
         order = ifelse(GBD_cause_name == "Sense organ diseases", 6, order),
         order = ifelse(GBD_cause_name == "Cardiovascular diseases", 7, order),
         order = ifelse(GBD_cause_name == "Respiratory diseases", 8, order),
         order = ifelse(GBD_cause_name == "Digestive diseases", 9, order),
         order = ifelse(GBD_cause_name == "Genitourinary diseases", 10, order),
         order = ifelse(GBD_cause_name == "Skin diseases", 11, order),
         order = ifelse(GBD_cause_name == "Musculoskeletal diseases", 12, order),
         order = ifelse(GBD_cause_name == "Oral conditions", 13, order))


NCD_women_inc <- conf_NCD_women_inc %>% 
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
  select(NCD, GBD_cause_name, order, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT, FEMALE_inc_prop) 



# Copenhagen_plot data - education

Copenhagen_NCD_women_edu <- NCD_women_edu %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))




# Copenhagen_plot data - income

Copenhagen_NCD_women_inc <- NCD_women_inc %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))




# Making list of diagnoses

diag_women_edu <- Copenhagen_NCD_women_edu %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(NCD) %>% 
  unique() %>% 
  as.data.frame()


diag_women_inc <- Copenhagen_NCD_women_inc %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(NCD) %>% 
  unique() %>% 
  as.data.frame()

diag_list <- rbind(diag_women_edu, diag_women_inc) %>% 
  unique()


#### Finishing data

Copenhagen_NCD_women_edu <- left_join(diag_list, Copenhagen_NCD_women_edu, by = "NCD")

Copenhagen_NCD_women_inc <- left_join(diag_list, Copenhagen_NCD_women_inc, by = "NCD")



# plot : eduation
group_colors <- viridis::viridis(n=2, begin = 0, end = 0.8)


p <- ggplot(Copenhagen_NCD_women_edu, 
       aes(x = VALUE, y = as.factor(NCD), color = as.factor(ESTIMATOR), size = FEMALE_edu_prop)) +
  geom_point(position = position_dodge(0.5))+ 
  geom_errorbar(aes(xmin=CI_LOW, xmax=CI_HIGH), position = position_dodge(0.5), size = 0.5)+
  theme_minimal() +
  theme_classic() + 
  facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
             labeller = labeller(order = c("1" = "Malignant neoplasms",
                                           "2" = "Other neoplasms", 
                                           "3" = "Diabetes Mellitus",
                                           "4" = "Endocrine disorders",
                                           "5" = "Neuropsychiatric conditions",
                                           "6" = "Sense organ diseases", 
                                           "7" = "Cardiovascular diseases", 
                                           "8" = "Respiratory diseases",
                                           "9" = "Digestive diseases", 
                                           "10" = "Genitourinary diseases",
                                           "11" = "Skin diseases",
                                           "12" = "Musculoskeletal diseases", 
                                           "13" = "Oral conditions")))+
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  scale_color_manual(values = group_colors, name = "Education", labels = c("Low vs. high", "Medium vs. high")) +
  scale_size_continuous(name = "Percentage") +
  scale_alpha_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  scale_y_discrete(limits=rev, label = function(NCD){
    NCD %>% sub("....", "", .)
  }) +
  coord_cartesian(xlim = c(-1.5, 4))

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


p <- ggplot(Copenhagen_NCD_women_inc, 
       aes(x = VALUE, y = as.factor(NCD), color = as.factor(ESTIMATOR), size = FEMALE_inc_prop)) +
  geom_point(position = position_dodge(0.5))+ 
  geom_errorbar(aes(xmin=CI_LOW, xmax=CI_HIGH), position = position_dodge(0.5), size = 0.5)+
  theme_minimal() +
  theme_classic() + 
  facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
             labeller = labeller(order = c("1" = "Malignant neoplasms",
                                           "2" = "Other neoplasms", 
                                           "3" = "Diabetes Mellitus",
                                           "4" = "Endocrine disorders",
                                           "5" = "Neuropsychiatric conditions",
                                           "6" = "Sense organ diseases", 
                                           "7" = "Cardiovascular diseases", 
                                           "8" = "Respiratory diseases",
                                           "9" = "Digestive diseases", 
                                           "10" = "Genitourinary diseases",
                                           "11" = "Skin diseases",
                                           "12" = "Musculoskeletal diseases", 
                                           "13" = "Oral conditions")))+
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  scale_color_manual(name = "Income", labels = c("Q1 vs. Q4", "Q2 vs. Q4", "Q3 vs. Q4"), values = group_colors_inc)+
  scale_size_continuous(name = "Percentage") +
  scale_alpha_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  scale_y_discrete(limits=rev, label = function(NCD){
    NCD %>% sub("....", "", .)
  }) +
  coord_cartesian(xlim = c(-1.5, 4))

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

conf_NCD_men_edu <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/SupplementaryData18_NCD_education_men_RR.xlsx")

conf_NCD_men_edu <- left_join(conf_NCD_men_edu, N_NCD, by = "NCD") %>% 
  mutate(NCD = ifelse(NCD == "Mouth and oropharynx cancers", "A1: Mouth and oropharynx cancers", NCD),
         NCD = ifelse(NCD == "Esophageal cancer", "AB: Esophageal cancer", NCD),
         NCD = ifelse(NCD == "Stomach cancer", "AC: Stomach cancer", NCD),
         NCD = ifelse(NCD == "Colon and rectal cancers", "AD: Colon and rectal cancers", NCD),
         NCD = ifelse(NCD == "Liver cancer", "AE: Liver cancer", NCD),
         NCD = ifelse(NCD == "Pancreas cancer", "AF: Pancreas cancer", NCD),
         NCD = ifelse(NCD == "Trachea, bronchus, and lung cancers", "AG: Trachea, bronchus, and lung cancers", NCD),
         NCD = ifelse(NCD == "Melanoma and other skin cancers", "AH: Melanoma and other skin cancers", NCD),
         NCD = ifelse(NCD == "Breast cancer", "AI: Breast cancer", NCD),
         NCD = ifelse(NCD == "Cervix uteri cancer", "AJ: Cervix uteri cancer", NCD),
         NCD = ifelse(NCD == "Corpus uteri cancer", "AK: Corpus uteri cancer", NCD),
         NCD = ifelse(NCD == "Ovarian cancer", "AL: Ovarian cancer", NCD),
         NCD = ifelse(NCD == "Prostate cancer", "AM: Prostate cancer", NCD),
         NCD = ifelse(NCD == "Bladder cancer", "AN: Bladder cancer", NCD),
         NCD = ifelse(NCD == "Lymphomas and multiple myeloma", "AO: Lymphomas and multiple myeloma", NCD),
         NCD = ifelse(NCD == "Leukemia", "AP: Leukemia", NCD),
         NCD = ifelse(NCD == "Other malignant neoplasms", "AQ: Other malignant neoplasms", NCD),
         NCD = ifelse(NCD == "Other neoplasms", "BA: Other neoplasms", NCD),
         NCD = ifelse(NCD == "Diabetes mellitus", "CA: Diabetes mellitus", NCD),
         NCD = ifelse(NCD == "Endocrine disorders", "DA: Endocrine disorders", NCD),
         NCD = ifelse(NCD == "Unipolar depressive disorders", "EA: Unipolar depressive disorders", NCD),
         NCD = ifelse(NCD == "Bipolar affective disorders", "EB: Bipolar affective disorders", NCD),
         NCD = ifelse(NCD == "Schizophrenia", "EC: Schizophrenia", NCD),
         NCD = ifelse(NCD == "Epilepsi", "ED: Epilepsi", NCD),
         NCD = ifelse(NCD == "Alcohol use disorders", "EE: Alcohol use disorders", NCD),
         NCD = ifelse(NCD == "Alzheimer's disease and other dementias", "EF: Alzheimer's disease and other dementias", NCD),
         NCD = ifelse(NCD == "Parkinson's disease", "EG: Parkinson's disease", NCD),
         NCD = ifelse(NCD == "Multiple schlerosis", "EH: Multiple schlerosis", NCD),
         NCD = ifelse(NCD == "Drug use disorders", "EI: Drug use disorders", NCD),
         NCD = ifelse(NCD == "Reaction to severe stress, and adjustment disorders", "EJ: Reaction to severe stress", NCD),
         NCD = ifelse(NCD == "Obsessive-compulsive disorder", "EK: Obsessive-compulsive disorder", NCD),
         NCD = ifelse(NCD == "Anxiety disorders", "EL: Anxiety disorders", NCD),
         NCD = ifelse(NCD == "Insomnia (primary)", "EM: Insomnia (primary)", NCD),
         NCD = ifelse(NCD == "Migraine", "EN: Migraine", NCD),
         NCD = ifelse(NCD == "Other neuropsychiatric disorders", "EO: Other neuropsychiatric disorders", NCD),
         NCD = ifelse(NCD == "Glaucoma", "FA: Glaucoma", NCD),
         NCD = ifelse(NCD == "Cataracts", "FB: Cataracts", NCD),
         NCD = ifelse(NCD == "Hearing loss, adult onset", "FC: Hearing loss, adult onset", NCD),
         NCD = ifelse(NCD == "Other sense organ disorders", "FD: Other sense organ disorders", NCD),
         NCD = ifelse(NCD == "Rheumatic heart disease", "GA: Rheumatic heart disease", NCD),
         NCD = ifelse(NCD == "Hypertensive heart disease", "GB: Hypertensive heart disease", NCD),
         NCD = ifelse(NCD == "Ischemic heart disease", "GC: Ischemic heart disease", NCD),
         NCD = ifelse(NCD == "Cerebrovascular disease", "GD: Cerebrovascular disease", NCD),
         NCD = ifelse(NCD == "Inflammatory heart diseases", "GE: Inflammatory heart diseases", NCD),
         NCD = ifelse(NCD == "Other cardiovascular diseases", "GF: Other cardiovascular diseases", NCD),
         NCD = ifelse(NCD == "Chronic obstructive pulmonary disease", "HA: Chronic obstructive pulmonary disease", NCD),
         NCD = ifelse(NCD == "Asthma", "HB: Asthma", NCD),
         NCD = ifelse(NCD == "Other respiratory diseases", "HC: Other respiratory diseases", NCD),
         NCD = ifelse(NCD == "Peptic ulcer disease", "IA: Peptic ulcer disease", NCD),
         NCD = ifelse(NCD == "Cirrhosis of the liver", "IB: Cirrhosis of the liver", NCD),
         NCD = ifelse(NCD == "Appendicitis", "IC: Appendicitis", NCD),
         NCD = ifelse(NCD == "Other digestive diseases", "ID: Other digestive diseases", NCD),
         NCD = ifelse(NCD == "Nephritis and neophrosis", "JA: Nephritis and neophrosis", NCD),
         NCD = ifelse(NCD == "Benign prostatic hypertrophy", "JB: Benign prostatic hypertrophy", NCD),
         NCD = ifelse(NCD == "Other genitourinary system diseases", "JC: Other genitourinary system diseases", NCD),
         NCD = ifelse(NCD == "Skin diseases", "KA: Skin diseases", NCD),
         NCD = ifelse(NCD == "Rheumatoid arthritis", "LA: Rheumatoid arthritis", NCD),
         NCD = ifelse(NCD == "Osteoarthritis", "LB: Osteoarthritis", NCD),
         NCD = ifelse(NCD == "Gout", "LC: Gout", NCD),
         NCD = ifelse(NCD == "Low back pain", "LD: Low back pain", NCD),
         NCD = ifelse(NCD == "Other muscuskeletal disorders", "LE: Other muscuskeletal disorders", NCD),
         NCD = ifelse(NCD == "Dental caries", "MA: Dental caries", NCD),
         NCD = ifelse(NCD == "Periodontal disease", "MB: Periodontal disease", NCD),
         NCD = ifelse(NCD == "Other oral diseases", "MC: Other oral diseases", NCD)) %>% 
  mutate(GBD_cause_name = ifelse(NCD == "A1: Mouth and oropharynx cancers", "Malignant neoplasms", NA),
         GBD_cause_name = ifelse(NCD == "AB: Esophageal cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AC: Stomach cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AD: Colon and rectal cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AE: Liver cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AF: Pancreas cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AG: Trachea, bronchus, and lung cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AH: Melanoma and other skin cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AI: Breast cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AJ: Cervix uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AK: Corpus uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AL: Ovarian cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AM: Prostate cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AN: Bladder cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AO: Lymphomas and multiple myeloma cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AP: Leukemia", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AQ: Other malignant neoplasms", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "BA: Other neoplasms", "Other neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "CA: Diabetes mellitus", "Diabetes mellitus", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "DA: Endocrine disorders", "Endocrine disorders", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EA: Unipolar depressive disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EB: Bipolar affective disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EC: Schizophrenia", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "ED: Epilepsi", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EE: Alcohol use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EF: Alzheimer's disease and other dementias", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EG: Parkinson's disease", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EH: Multiple sclerosis", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EI: Drug use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EJ: Reaction to severe stress", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EK: Obsessive-compulsive disorder", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EL: Anxiety disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EM: Insomnia (primary)", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EN: Migraine", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EO: Other neuropsychiatric disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FA: Glaucoma", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FB: Cataracts", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FC: Hearing loss, adult onset", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FD: Other sense organ disorders", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GA: Rheumatic heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GB: Hypertensive heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GC: Ischemic heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GD: Cerebrovascular disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GE: Inflammatory heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GF: Other cardiovascular diseases", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HA: Chronic obstructive pulmonary disease", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HB: Asthma", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HC: Other respiratory diseases", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IA: Peptic ulcer disease", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IB: Cirrhosis of the liver", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IC: Appendicitis", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "ID: Other digestive diseases", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JA: Nephritis and nephrosis", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JB: Benign prostatic hypertrophy", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JC: Other genitourinary system diseases", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "KA: Skin diseases", "Skin diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LA: Rheumatoid arthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LB: Oesteoarthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LC: Gout", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LD: Low back pain", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LE: Other musculoskeletal disorders", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MA: Dental caries", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MB: Periodontal disease", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MC: Other oral diseases", "Oral conditions", GBD_cause_name)) %>% 
  mutate(order = ifelse(GBD_cause_name == "Malignant neoplasms", 1, NA),
         order = ifelse(GBD_cause_name == "Other neoplasms", 2, order),
         order = ifelse(GBD_cause_name == "Diabetes mellitus", 3, order),
         order = ifelse(GBD_cause_name == "Endocrine disorders", 4, order),
         order = ifelse(GBD_cause_name == "Neuropsychiatric conditions", 5, order),
         order = ifelse(GBD_cause_name == "Sense organ diseases", 6, order),
         order = ifelse(GBD_cause_name == "Cardiovascular diseases", 7, order),
         order = ifelse(GBD_cause_name == "Respiratory diseases", 8, order),
         order = ifelse(GBD_cause_name == "Digestive diseases", 9, order),
         order = ifelse(GBD_cause_name == "Genitourinary diseases", 10, order),
         order = ifelse(GBD_cause_name == "Skin diseases", 11, order),
         order = ifelse(GBD_cause_name == "Musculoskeletal diseases", 12, order),
         order = ifelse(GBD_cause_name == "Oral conditions", 13, order))


NCD_men_edu <- conf_NCD_men_edu %>% 
  select(-"...1") %>% 
  pivot_longer(c(5,11), names_to = "ESTIMATOR", values_to = "VALUE")%>% 
  pivot_longer(c(5,10), names_to = "CI_low_temp", values_to = "CI_LOW")%>% 
  pivot_longer(c(5,9), names_to = "CI_high_temp", values_to = "CI_HIGH")%>% 
  pivot_longer(c(5,8), names_to = "PVALUE_temp", values_to = "P_VALUE")%>% 
  pivot_longer(c(5,7), names_to = "QVALUE_temp", values_to = "QVALUE")%>% 
  pivot_longer(c(5,6), names_to = "BH_ACCEPT_temp", values_to = "BH_ACCEPT") %>% 
  filter(ESTIMATOR == "log_RR_udd1vs3" & CI_low_temp == "LOW_CI_udd1vs3" & CI_high_temp == "HIGH_CI_udd1vs3" & PVALUE_temp == "PVALUE_udd1vs3" & QVALUE_temp == "QVALUE_udd1vs3" & BH_ACCEPT_temp == "BH_accept_udd1vs3" |
           ESTIMATOR == "log_RR_udd2vs3" & CI_low_temp == "LOW_CI_udd2vs3" & CI_high_temp == "HIGH_CI_udd2vs3" & PVALUE_temp == "PVALUE_udd2vs3" & QVALUE_temp == "QVALUE_udd2vs3" & BH_ACCEPT_temp == "BH_accept_udd2vs3") %>% 
  select(NCD, GBD_cause_name, order, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT, MALE_edu_prop) 


###### data: income & women

conf_NCD_men_inc <- readxl::read_xlsx("H:/Phd/PhD artikler/Study1/Lancet Public Health/Review - substantial/Supplementary material/SupplementaryData22_NCD_income_men_RR.xlsx")

conf_NCD_men_inc <- left_join(conf_NCD_men_inc, N_NCD, by = "NCD") %>% 
  mutate(NCD = ifelse(NCD == "Mouth and oropharynx cancers", "A1: Mouth and oropharynx cancers", NCD),
         NCD = ifelse(NCD == "Esophageal cancer", "AB: Esophageal cancer", NCD),
         NCD = ifelse(NCD == "Stomach cancer", "AC: Stomach cancer", NCD),
         NCD = ifelse(NCD == "Colon and rectal cancers", "AD: Colon and rectal cancers", NCD),
         NCD = ifelse(NCD == "Liver cancer", "AE: Liver cancer", NCD),
         NCD = ifelse(NCD == "Pancreas cancer", "AF: Pancreas cancer", NCD),
         NCD = ifelse(NCD == "Trachea, bronchus, and lung cancers", "AG: Trachea, bronchus, and lung cancers", NCD),
         NCD = ifelse(NCD == "Melanoma and other skin cancers", "AH: Melanoma and other skin cancers", NCD),
         NCD = ifelse(NCD == "Breast cancer", "AI: Breast cancer", NCD),
         NCD = ifelse(NCD == "Cervix uteri cancer", "AJ: Cervix uteri cancer", NCD),
         NCD = ifelse(NCD == "Corpus uteri cancer", "AK: Corpus uteri cancer", NCD),
         NCD = ifelse(NCD == "Ovarian cancer", "AL: Ovarian cancer", NCD),
         NCD = ifelse(NCD == "Prostate cancer", "AM: Prostate cancer", NCD),
         NCD = ifelse(NCD == "Bladder cancer", "AN: Bladder cancer", NCD),
         NCD = ifelse(NCD == "Lymphomas and multiple myeloma", "AO: Lymphomas and multiple myeloma", NCD),
         NCD = ifelse(NCD == "Leukemia", "AP: Leukemia", NCD),
         NCD = ifelse(NCD == "Other malignant neoplasms", "AQ: Other malignant neoplasms", NCD),
         NCD = ifelse(NCD == "Other neoplasms", "BA: Other neoplasms", NCD),
         NCD = ifelse(NCD == "Diabetes mellitus", "CA: Diabetes mellitus", NCD),
         NCD = ifelse(NCD == "Endocrine disorders", "DA: Endocrine disorders", NCD),
         NCD = ifelse(NCD == "Unipolar depressive disorders", "EA: Unipolar depressive disorders", NCD),
         NCD = ifelse(NCD == "Bipolar affective disorders", "EB: Bipolar affective disorders", NCD),
         NCD = ifelse(NCD == "Schizophrenia", "EC: Schizophrenia", NCD),
         NCD = ifelse(NCD == "Epilepsi", "ED: Epilepsi", NCD),
         NCD = ifelse(NCD == "Alcohol use disorders", "EE: Alcohol use disorders", NCD),
         NCD = ifelse(NCD == "Alzheimer's disease and other dementias", "EF: Alzheimer's disease and other dementias", NCD),
         NCD = ifelse(NCD == "Parkinson's disease", "EG: Parkinson's disease", NCD),
         NCD = ifelse(NCD == "Multiple schlerosis", "EH: Multiple schlerosis", NCD),
         NCD = ifelse(NCD == "Drug use disorders", "EI: Drug use disorders", NCD),
         NCD = ifelse(NCD == "Reaction to severe stress, and adjustment disorders", "EJ: Reaction to severe stress", NCD),
         NCD = ifelse(NCD == "Obsessive-compulsive disorder", "EK: Obsessive-compulsive disorder", NCD),
         NCD = ifelse(NCD == "Anxiety disorders", "EL: Anxiety disorders", NCD),
         NCD = ifelse(NCD == "Insomnia (primary)", "EM: Insomnia (primary)", NCD),
         NCD = ifelse(NCD == "Migraine", "EN: Migraine", NCD),
         NCD = ifelse(NCD == "Other neuropsychiatric disorders", "EO: Other neuropsychiatric disorders", NCD),
         NCD = ifelse(NCD == "Glaucoma", "FA: Glaucoma", NCD),
         NCD = ifelse(NCD == "Cataracts", "FB: Cataracts", NCD),
         NCD = ifelse(NCD == "Hearing loss, adult onset", "FC: Hearing loss, adult onset", NCD),
         NCD = ifelse(NCD == "Other sense organ disorders", "FD: Other sense organ disorders", NCD),
         NCD = ifelse(NCD == "Rheumatic heart disease", "GA: Rheumatic heart disease", NCD),
         NCD = ifelse(NCD == "Hypertensive heart disease", "GB: Hypertensive heart disease", NCD),
         NCD = ifelse(NCD == "Ischemic heart disease", "GC: Ischemic heart disease", NCD),
         NCD = ifelse(NCD == "Cerebrovascular disease", "GD: Cerebrovascular disease", NCD),
         NCD = ifelse(NCD == "Inflammatory heart diseases", "GE: Inflammatory heart diseases", NCD),
         NCD = ifelse(NCD == "Other cardiovascular diseases", "GF: Other cardiovascular diseases", NCD),
         NCD = ifelse(NCD == "Chronic obstructive pulmonary disease", "HA: Chronic obstructive pulmonary disease", NCD),
         NCD = ifelse(NCD == "Asthma", "HB: Asthma", NCD),
         NCD = ifelse(NCD == "Other respiratory diseases", "HC: Other respiratory diseases", NCD),
         NCD = ifelse(NCD == "Peptic ulcer disease", "IA: Peptic ulcer disease", NCD),
         NCD = ifelse(NCD == "Cirrhosis of the liver", "IB: Cirrhosis of the liver", NCD),
         NCD = ifelse(NCD == "Appendicitis", "IC: Appendicitis", NCD),
         NCD = ifelse(NCD == "Other digestive diseases", "ID: Other digestive diseases", NCD),
         NCD = ifelse(NCD == "Nephritis and neophrosis", "JA: Nephritis and neophrosis", NCD),
         NCD = ifelse(NCD == "Benign prostatic hypertrophy", "JB: Benign prostatic hypertrophy", NCD),
         NCD = ifelse(NCD == "Other genitourinary system diseases", "JC: Other genitourinary system diseases", NCD),
         NCD = ifelse(NCD == "Skin diseases", "KA: Skin diseases", NCD),
         NCD = ifelse(NCD == "Rheumatoid arthritis", "LA: Rheumatoid arthritis", NCD),
         NCD = ifelse(NCD == "Osteoarthritis", "LB: Osteoarthritis", NCD),
         NCD = ifelse(NCD == "Gout", "LC: Gout", NCD),
         NCD = ifelse(NCD == "Low back pain", "LD: Low back pain", NCD),
         NCD = ifelse(NCD == "Other muscuskeletal disorders", "LE: Other muscuskeletal disorders", NCD),
         NCD = ifelse(NCD == "Dental caries", "MA: Dental caries", NCD),
         NCD = ifelse(NCD == "Periodontal disease", "MB: Periodontal disease", NCD),
         NCD = ifelse(NCD == "Other oral diseases", "MC: Other oral diseases", NCD)) %>% 
  mutate(GBD_cause_name = ifelse(NCD == "A1: Mouth and oropharynx cancers", "Malignant neoplasms", NA),
         GBD_cause_name = ifelse(NCD == "AB: Esophageal cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AC: Stomach cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AD: Colon and rectal cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AE: Liver cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AF: Pancreas cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AG: Trachea, bronchus, and lung cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AH: Melanoma and other skin cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AI: Breast cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AJ: Cervix uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AK: Corpus uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AL: Ovarian cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AM: Prostate cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AN: Bladder cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AO: Lymphomas and multiple myeloma cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AP: Leukemia", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "AQ: Other malignant neoplasms", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "BA: Other neoplasms", "Other neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "CA: Diabetes mellitus", "Diabetes mellitus", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "DA: Endocrine disorders", "Endocrine disorders", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EA: Unipolar depressive disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EB: Bipolar affective disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EC: Schizophrenia", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "ED: Epilepsi", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EE: Alcohol use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EF: Alzheimer's disease and other dementias", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EG: Parkinson's disease", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EH: Multiple sclerosis", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EI: Drug use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EJ: Reaction to severe stress", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EK: Obsessive-compulsive disorder", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EL: Anxiety disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EM: Insomnia (primary)", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EN: Migraine", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "EO: Other neuropsychiatric disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FA: Glaucoma", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FB: Cataracts", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FC: Hearing loss, adult onset", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "FD: Other sense organ disorders", "Sense organ diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GA: Rheumatic heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GB: Hypertensive heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GC: Ischemic heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GD: Cerebrovascular disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GE: Inflammatory heart disease", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "GF: Other cardiovascular diseases", "Cardiovascular diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HA: Chronic obstructive pulmonary disease", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HB: Asthma", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "HC: Other respiratory diseases", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IA: Peptic ulcer disease", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IB: Cirrhosis of the liver", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "IC: Appendicitis", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "ID: Other digestive diseases", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JA: Nephritis and nephrosis", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JB: Benign prostatic hypertrophy", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "JC: Other genitourinary system diseases", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "KA: Skin diseases", "Skin diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LA: Rheumatoid arthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LB: Oesteoarthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LC: Gout", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LD: Low back pain", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "LE: Other musculoskeletal disorders", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MA: Dental caries", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MB: Periodontal disease", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "MC: Other oral diseases", "Oral conditions", GBD_cause_name)) %>% 
  mutate(order = ifelse(GBD_cause_name == "Malignant neoplasms", 1, NA),
         order = ifelse(GBD_cause_name == "Other neoplasms", 2, order),
         order = ifelse(GBD_cause_name == "Diabetes mellitus", 3, order),
         order = ifelse(GBD_cause_name == "Endocrine disorders", 4, order),
         order = ifelse(GBD_cause_name == "Neuropsychiatric conditions", 5, order),
         order = ifelse(GBD_cause_name == "Sense organ diseases", 6, order),
         order = ifelse(GBD_cause_name == "Cardiovascular diseases", 7, order),
         order = ifelse(GBD_cause_name == "Respiratory diseases", 8, order),
         order = ifelse(GBD_cause_name == "Digestive diseases", 9, order),
         order = ifelse(GBD_cause_name == "Genitourinary diseases", 10, order),
         order = ifelse(GBD_cause_name == "Skin diseases", 11, order),
         order = ifelse(GBD_cause_name == "Musculoskeletal diseases", 12, order),
         order = ifelse(GBD_cause_name == "Oral conditions", 13, order))


NCD_men_inc <- conf_NCD_men_inc %>% 
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
  select(NCD, GBD_cause_name, order, N, PERSON_YEARS, CASES, ESTIMATOR, VALUE, CI_LOW, CI_HIGH, P_VALUE, QVALUE, BH_ACCEPT, MALE_inc_prop) 



# Copenhagen_plot data - education

Copenhagen_NCD_men_edu <- NCD_men_edu %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))




# Copenhagen_plot data - income

Copenhagen_NCD_men_inc <- NCD_men_inc %>% 
  mutate(significant = ifelse(CI_LOW < 0 & CI_HIGH > 0, "NO", "YES"),
         extremevalues = ifelse(VALUE > -5 & VALUE < 5, "NO", "YES"),
         ESTIMATOR = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, ESTIMATOR),
         VALUE = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, VALUE),
         CI_LOW = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_LOW),
         CI_HIGH = ifelse(BH_ACCEPT != TRUE | significant == "NO"  | extremevalues == "YES", NA, CI_HIGH))




# Making list of diagnoses

diag_men_edu <- Copenhagen_NCD_men_edu %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(NCD) %>% 
  unique() %>% 
  as.data.frame()


diag_men_inc <- Copenhagen_NCD_men_inc %>% 
  filter(!is.na(ESTIMATOR)) %>% 
  select(NCD) %>% 
  unique() %>% 
  as.data.frame()

diag_list <- rbind(diag_men_edu, diag_men_inc) %>% 
  unique()


#### Finishing data

Copenhagen_NCD_men_edu <- left_join(diag_list, Copenhagen_NCD_men_edu, by = "NCD")

Copenhagen_NCD_men_inc <- left_join(diag_list, Copenhagen_NCD_men_inc, by = "NCD")



# plot : eduation
group_colors <- viridis::viridis(n=2, begin = 0, end = 0.8)


p <- ggplot(Copenhagen_NCD_men_edu, 
       aes(x = VALUE, y = as.factor(NCD), color = as.factor(ESTIMATOR), size = MALE_edu_prop)) +
  geom_point(position = position_dodge(0.5))+ 
  geom_errorbar(aes(xmin=CI_LOW, xmax=CI_HIGH), position = position_dodge(0.5), size = 0.5)+
  theme_minimal() +
  theme_classic() + 
  facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
             labeller = labeller(order = c("1" = "Malignant neoplasms",
                                           "2" = "Other neoplasms", 
                                           "3" = "Diabetes Mellitus",
                                           "4" = "Endocrine disorders",
                                           "5" = "Neuropsychiatric conditions",
                                           "6" = "Sense organ diseases", 
                                           "7" = "Cardiovascular diseases", 
                                           "8" = "Respiratory diseases",
                                           "9" = "Digestive diseases", 
                                           "10" = "Genitourinary diseases",
                                           "11" = "Skin diseases",
                                           "12" = "Musculoskeletal diseases", 
                                           "13" = "Oral conditions")))+
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  scale_color_manual(values = group_colors, name = "Education", labels = c("Low vs. high", "Medium vs. high")) +
  scale_size_continuous(name = "Percentage") +
  scale_alpha_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  scale_y_discrete(limits=rev, label = function(NCD){
    NCD %>% sub("....", "", .)
  }) +
  coord_cartesian(xlim = c(-1.5, 4))

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


p <- ggplot(Copenhagen_NCD_men_inc, 
       aes(x = VALUE, y = as.factor(NCD), color = as.factor(ESTIMATOR), size = MALE_inc_prop)) +
  geom_point(position = position_dodge(0.5))+ 
  geom_errorbar(aes(xmin=CI_LOW, xmax=CI_HIGH), position = position_dodge(0.5), size = 0.5)+
  theme_minimal() +
  theme_classic() + 
  facet_grid(vars(order), switch = "y", scales = "free_y", space = "free_y",
             labeller = labeller(order = c("1" = "Malignant neoplasms",
                                           "2" = "Other neoplasms", 
                                           "3" = "Diabetes Mellitus",
                                           "4" = "Endocrine disorders",
                                           "5" = "Neuropsychiatric conditions",
                                           "6" = "Sense organ diseases", 
                                           "7" = "Cardiovascular diseases", 
                                           "8" = "Respiratory diseases",
                                           "9" = "Digestive diseases", 
                                           "10" = "Genitourinary diseases",
                                           "11" = "Skin diseases",
                                           "12" = "Musculoskeletal diseases", 
                                           "13" = "Oral conditions")))+
  theme(strip.text.y.left = element_text(angle = 0, face = "bold", hjust=1),
        strip.placement = "outside",
        strip.background = element_blank()) +
  scale_color_manual(name = "Income", labels = c("Q1 vs. Q4", "Q2 vs. Q4", "Q3 vs. Q4"), values = group_colors_inc)+
  scale_size_continuous(name = "Percentage") +
  scale_alpha_continuous(name = "Percentage") +
  geom_vline(xintercept = 0, col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  scale_y_discrete(limits=rev, label = function(NCD){
    NCD %>% sub("....", "", .)
  }) +
  coord_cartesian(xlim = c(-1.5, 4))


# Making a line for each GBD_cause_name to indicate length/size of group
library(grid)
q <- ggplotGrob(p)
lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                gp=gpar(col="black", lwd=3))

for (k in grep("strip-l",q$layout$name)) {
  q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
}

grid.draw(q)

