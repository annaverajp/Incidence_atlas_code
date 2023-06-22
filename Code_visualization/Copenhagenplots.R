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

## Adgang til data
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")

ASIR_all <- dbReadTable(conn_inci, "ASIR_all")
ASIR_all <- ASIR_all %>% 
  left_join(dplyr::select(names, DIAG, KAP), by = "DIAG") %>% 
  unique() %>% 
  mutate(KAP = ifelse(DIAG == "DA97", 1, KAP),
         KAP = ifelse(DIAG == "DC99", 2, KAP))

N_all <- dbReadTable(conn_inci, "N_all") %>% 
  left_join(dplyr::select(names, DIAG, KAP), by = "DIAG") %>% 
  unique() %>% 
  mutate(KAP = ifelse(DIAG == "DA97", 1, KAP),
         KAP = ifelse(DIAG == "DC99", 2, KAP)) 



### Copenhagen_plot for all diseases

Copenhagen_all <- ASIR_all %>% 
  mutate(udd = recode(udd, "1" = "udd1",
                      "2" = "udd2",
                      "3" = "udd3")) %>% 
  pivot_wider(names_from = udd, values_from = ASIR) %>% 
  mutate(RR1vs2 = udd1/udd2,
         RR3vs2 = udd3/udd2,
         RR3vs1 = udd3/udd1)%>% 
  dplyr::select(DIAG, RR1vs2, RR3vs2, KAP) %>% 
  pivot_longer(2:3, names_to = "type", values_to = "RR")  %>% 
  left_join(N_all, by = c("DIAG", "KAP")) 



atlas <- ggplot(Copenhagen_all, aes(x = log(RR), y = as.factor(KAP), color = as.factor(type), size = prop)) +
  geom_jitter(width = 0, height = 0.25) + 
  theme_minimal() +
  theme_classic() + 
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_rect(color="black", fill=NA),
        strip.background = element_rect(fill=NA, color="black")) +
  scale_y_discrete(limits=rev, labels = c('Chap. 19: Injury, poisoning and certain other external causes',
                                          'Chap. 18: Symptoms, signs and abnormal clinical and laboratory findings',
                                          'Chap. 14: Diseases of the genitourinary system', 
                                          'Chap. 13: Diseases of the musculoskeletal system and connective tissue',
                                          'Chap. 12: Diseases of the skin and subcutaneous tissue',
                                          'Chap. 11: Diseases of the digestive system',
                                          'Chap. 10: Diseases of the respiratory system',
                                          'Chap. 9: Diseases of the circulatory system',
                                          'Chap. 8: Diseases of the ear and mastoid process',
                                          'Chap. 7: Diseases of the eye and adnexa',
                                          'Chap. 6: Diseases of the nervous system', 
                                          'Chap. 5: Mental and behavioural disorders',
                                          'Chap. 4: Endocrine, nutritional and metabolic diseases',
                                          'Chap. 3: Diseases of the blood and blood-forming organs',
                                          'Chap. 2: Neoplasms',
                                          'Chap. 1: Infectious and parasitic diseases'))  +
  scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium")) +
  scale_size_continuous(name = "Proportion") +
  geom_vline(xintercept = log(1), col = "Black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'ICD-10 chapter')+
  ggtitle("Age-standardised incidence rate ratios for 2012-2021 presented on a log scale") +
  xlim(-2,2)

#atlas
#ggsave(filename = "CopenhagenPlot_all.tiff", path = "H:/KT22SVN/X30/PhD_Study1/Results_plots", units = "px", width = 4400, height = 2400, device='tiff', dpi=400)



#### Copenhagen plot with chapter 2

Chap2 <- Copenhagen_all %>% 
  filter(KAP == 2) %>% 
  mutate(KAP_NAME = ifelse(KAP == 2, "Chapter 2: Neoplasms", NA),
         DIAG = substr(str_trim(DIAG), 2, 4)) %>% 
  group_by(DIAG) %>% 
  arrange(-RR)


#chapter 2
chap2 <- ggplot(Chap2[Chap2$KAP==2,], aes(x = log(RR), y = DIAG, group = DIAG, color = type)) +
  geom_point(aes(size = prop))+
  geom_path(aes(group = DIAG), alpha = 0.4) +
  theme_minimal() +
  theme_classic() + 
  theme(panel.border = element_rect(color="black", fill=NA)) +
  scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium", "")) +
  scale_size_continuous(name = "Proportion") +
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 6))+
  geom_vline(xintercept=log(1), linetype="dashed", 
             color = "black")+
  labs(y = 'Diagnosis', x = 'log(Incidence rate ratio)',
       title = "Age-standardised incidence rate ratios for 2012-2021 presented on a log scale",
       subtitle ="Chapter 2: Neoplasms") +
  scale_y_discrete(limits=rev) +
  xlim(-1.3,1.3)

#chap2
#ggsave(filename = "CopenhagenPlot_chap2.tiff", path = "H:/KT22SVN/X30/PhD_Study1/Results_plots", units = "px", width = 4000, height = 6300, device='tiff', dpi=400)



### Copenhagen plot for most prevalent diseases/NCDs

ASIR_NCD <- dbReadTable(conn_inci, "ASIR_NCD")


N_NCD <- N_all %>% 
  mutate(NCD = DIAG, 
         NCD = ifelse(NCD >= "DC00" & NCD <= "DC14", "A1: Mouth and oropharynx cancers", NCD),
         NCD = ifelse(NCD == "DC15", "AB: Esophageal cancer", NCD),
         NCD = ifelse(NCD == "DC16", "AC: Stomach cancer", NCD),
         NCD = ifelse(NCD >= "DC18" & NCD <= "DC21", "AD: Colon and rectal cancers", NCD),
         NCD = ifelse(NCD == "DC22", "AE: Liver cancer", NCD),
         NCD = ifelse(NCD == "DC25", "AF: Pancreas cancer", NCD),
         NCD = ifelse(NCD == "DC33" | NCD == "DC34", "AG: Trachea, bronchus, and lung cancers", NCD),
         NCD = ifelse(NCD == "DC43" | NCD == "DC44", "AH: Melanoma and other skin cancers", NCD),
         NCD = ifelse(NCD == "DC50", "AI: Breast cancer", NCD),
         NCD = ifelse(NCD == "DC53", "AJ: Cervix uteri cancer", NCD),
         NCD = ifelse(NCD == "DC54" | NCD == "DC55", "AK: Corpus uteri cancer", NCD),
         NCD = ifelse(NCD == "DC56", "AL: Ovarian cancer", NCD),
         NCD = ifelse(NCD == "DC61", "AM: Prostate cancer", NCD),
         NCD = ifelse(NCD == "DC67", "AN: Bladder cancer", NCD),
         NCD = ifelse(NCD >= "DC81" & NCD <= "DC90" | NCD == "DC96", "AO: Lymphomas and multiple myeloma", NCD),
         NCD = ifelse(NCD >= "DC91" & NCD <= "DC95", "AP: Leukemia", NCD),
         NCD = ifelse(NCD == "DC17" | NCD >= "DC23" & NCD <= "DC32" | NCD >= "DC37" & NCD <= "DC41" | NCD >= "DC45" & NCD <= "DC49" | 
                        NCD == "DC51" | NCD == "DC52" | NCD >= "DC57" & NCD <= "DC60" | NCD >= "DC62" & NCD <= "DC66" | 
                        NCD >= "DC68" & NCD <= "DC80" | NCD == "DC97", "AQ: Other malignant neoplasms", NCD),
         NCD = ifelse(NCD >= "DD00" & NCD <= "DD48", "BA: Other neoplasms", NCD),
         NCD = ifelse(NCD >= "DE10" & NCD <= "DE14", "CA: Diabetes mellitus", NCD),
         NCD = ifelse(NCD >= "DD55" & NCD <= "DD89" | NCD >= "DE03" & NCD <= "DE07" | NCD == "DE15" | NCD == "DE16" | 
                        NCD >= "DE20" & NCD <= "DE34" | NCD >= "DE65" & NCD <= "DE88", "DA: Endocrine disorders", NCD),
         NCD = ifelse(NCD == "DF32" | NCD == "DF33", "EA: Unipolar depressive disorders", NCD),
         NCD = ifelse(NCD == "DF30" | NCD == "DF31", "EB: Bipolar affective disorders", NCD),
         NCD = ifelse(NCD >= "DF20" & NCD <= "DF29", "EC: Schizophrenia", NCD),
         NCD = ifelse(NCD == "DG40" | NCD == "DG41", "ED: Epilepsi", NCD),
         NCD = ifelse(NCD == "DF10", "EE: Alcohol use disorders", NCD),
         NCD = ifelse(NCD == "DF01" | NCD == "DF03" | NCD == "DG30" | NCD == "DG31", "EF: Alzheimer's disease and other dementias", NCD),
         NCD = ifelse(NCD == "DG20" | NCD == "DG21", "EG: Parkinson's disease", NCD),
         NCD = ifelse(NCD == "DG35", "EH: Multiple schlerosis", NCD),
         NCD = ifelse(NCD >= "DF11" & NCD <= "DF16" | NCD == "DF18" | NCD == "DF19", "EI: Drug use disorders", NCD),
         NCD = ifelse(NCD == "DF43", "EJ: Reaction to severe stress", NCD),
         NCD = ifelse(NCD == "DF42", "EK: Obsessive-compulsive disorder", NCD),
         NCD = ifelse(NCD == "DF40" | NCD == "DF41", "EL: Anxiety disorders", NCD),
         NCD = ifelse(NCD == "DF51", "EM: Insomnia (primary)", NCD),
         NCD = ifelse(NCD == "DG43", "EN: Migraine", NCD),
         NCD = ifelse(NCD >= "DF04" & NCD == "DF09" | NCD == "DF17" | NCD >= "DF34" & NCD <= "DF39" | NCD >= "DF44" & NCD <= "DF50" | 
                        NCD <= "DF52" & NCD >= "DF69" | NCD >= "DF80" & NCD <= "DF99" | NCD >= "DG06" & NCD <= "DG12" | NCD >= "DG23" & NCD <= "DG25" | 
                        NCD == "DG36" | NCD == "DG37" | NCD >= "DG44" & NCD <= "DG98", "EO: Other neuropsychiatric disorders", NCD),
         NCD = ifelse(NCD == "DH40", "FA: Glaucoma", NCD),
         NCD = ifelse(NCD == "DH25" | NCD == "DH26", "FB: Cataracts", NCD),
         NCD = ifelse(NCD == "DH90" | NCD == "DH91", "FC: Hearing loss, adult onset", NCD),
         NCD = ifelse(NCD >= "DH00" & NCD <= "DH21" | NCD >= "DH27" & NCD <= "DH35" | NCD >= "DH43" & NCD <= "DH61" | NCD >= "H68" & NCD <= "DH83" |
                        NCD == "DH92" | NCD == "DH93", "FD: Other sense organ disorders", NCD),
         NCD = ifelse(NCD >= "DI01" & NCD <= "DI09", "GA: Rheumatic heart disease", NCD),
         NCD = ifelse(NCD >= "DI10" & NCD <= "DI13", "GB: Hypertensive heart disease", NCD),
         NCD = ifelse(NCD >= "DI20" & NCD <= "DI25", "GC: Ischemic heart disease", NCD),
         NCD = ifelse(NCD >= "DI60" & NCD <= "DI69", "GD: Cerebrovascular disease", NCD),
         NCD = ifelse(NCD >= "DI30" & NCD <= "DI33" | NCD == "DI38" | NCD == "DI40" | NCD == "DI42", "GE: Inflammatory heart diseases", NCD),
         NCD = ifelse(NCD == "DI00" | NCD >= "DI26" & NCD <= "DI28" | NCD >= "DI34" & NCD <= "DI37" | NCD >= "DI44" & NCD <= "DI51" | 
                        NCD >= "DI70" & NCD <= "DI99", "GF: Other cardiovascular diseases", NCD),
         NCD = ifelse(NCD >= "DJ40" & NCD <= "DJ44", "HA: Chronic obstructive pulmonary disease", NCD),
         NCD = ifelse(NCD >= "DJ45" & NCD <= "DJ46", "HB: Asthma", NCD),
         NCD = ifelse(NCD >= "DJ30" & NCD <= "DJ39" | NCD >= "DJ47" & NCD <= "DJ98", "HC: Other respiratory diseases", NCD),
         NCD = ifelse(NCD >= "DK25" & NCD <= "DK27", "IA: Peptic ulcer disease", NCD),
         NCD = ifelse(NCD == "DK70" | NCD == "DK74", "IB: Cirrhosis of the liver", NCD),
         NCD = ifelse(NCD >= "DK35" & NCD <= "DK37", "IC: Appendicitis", NCD),
         NCD = ifelse(NCD >= "DK20" & NCD <= "DK22" | NCD >= "DK28" & NCD <= "DK31" | NCD == "DK38" | NCD >= "DK40" & NCD <= "DK66" | 
                        NCD >= "DK71" & NCD <= "DK73" | NCD >= "DK75" & NCD <= "DK92", "ID: Other digestive diseases", NCD),
         NCD = ifelse(NCD >= "DN00" & NCD <= "DN19", "JA: Nephritis and neophrosis", NCD),
         NCD = ifelse(NCD == "DN40", "JB: Benign prostatic hypertrophy", NCD),
         NCD = ifelse(NCD >= "DN20" & NCD <= "DN39" | NCD >= "DN41" & NCD <= "DN64" | NCD >= "DN75" & NCD <= "DN98", "JC: Other genitourinary system diseases", NCD),
         NCD = ifelse(NCD >= "DL00" & NCD <= "DL98", "KA: Skin diseases", NCD),
         NCD = ifelse(NCD >= "DM05" & NCD <= "DM06", "LA: Rheumatoid arthritis", NCD),
         NCD = ifelse(NCD >= "DM15" & NCD <= "DM19", "LB: Osteoarthritis", NCD),
         NCD = ifelse(NCD == "DM10", "LC: Gout", NCD),
         NCD = ifelse(NCD >= "DM45" & NCD <= "DM48" | NCD == "DM54", "LD: Low back pain", NCD),
         NCD = ifelse(NCD >= "DM00" & NCD <= "DM02" | NCD == "DM08" | NCD >= "DM11" & NCD <= "DM13" | NCD >= "DM20" & NCD <= "DM43" | 
                        NCD >= "DM50" & NCD <= "DM53" | NCD >= "DM55" & NCD <= "DM99", "LE: Other muscuskeletal disorders", NCD),
         NCD = ifelse(NCD == "DK02", "MA: Dental caries", NCD),
         NCD = ifelse(NCD == "DK05", "MB: Periodontal disease", NCD),
         NCD = ifelse(NCD >= "DK00" & NCD <= "DK04" | NCD >= "DK06" & NCD <= "DK14", "MC: Other oral diseases", NCD)) %>% 
  group_by(NCD) %>% 
  summarise(prop = sum(prop)) %>% 
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
  filter(!is.na(GBD_cause_name))


ASIR_NCD <- ASIR_NCD %>% 
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
  left_join(N_NCD, by = "NCD") %>% 
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
 

ASIR_NCD <- ASIR_NCD %>% 
  filter(!is.na(GBD_cause_name))


Copenhagen_NCD <- ASIR_NCD %>% 
  mutate(udd = recode(udd, "1" = "udd1",
                      "2" = "udd2",
                      "3" = "udd3")) %>% 
  pivot_wider(names_from = udd, values_from = ASIR) %>% 
  mutate(RR1vs2 = udd1/udd2,
         RR3vs2 = udd3/udd2,
         RR3vs1 = udd3/udd1)%>% 
  dplyr::select(NCD, RR1vs2, RR3vs2, GBD_cause_name, order, prop) %>% 
  pivot_longer(2:3, names_to = "type", values_to = "RR") %>% 
  arrange(NCD) %>% 
  arrange(-RR)


# make variable indicating highest RR
test <- Copenhagen_NCD %>% 
  group_by(NCD) %>% 
  filter(RR == max(RR)) %>% 
  mutate(balance = type) %>% 
  select(NCD, balance)

Copenhagen_NCD <- left_join(Copenhagen_NCD, test, by = "NCD")

#plot

p <- ggplot(Copenhagen_NCD, aes(x = log(RR), y = NCD, color = as.factor(type), size = prop)) +
  geom_point(aes(size = prop))+ 
  geom_line(aes(group = NCD, color = balance), linewidth=1, alpha = 0.4) +
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
  scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium")) +
  scale_size_continuous(name = "Proportion") +
  scale_alpha_continuous(name = "Proportion") +
  geom_vline(xintercept = log(1), col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Noncommunicable diseases') +
  ggtitle("Age-standardised incidence rate ratios for NCDs in 2012-2021 presented on a log scale") +
  scale_y_discrete(limits=rev, label = function(NCD){
    NCD %>% sub("....", "", .)
  })+
  xlim(-1.5,1.5)

# Making a line for each GBD_cause_name to indicate length/size of group
library(grid)
q <- ggplotGrob(p)
lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                gp=gpar(col="black", lwd=3))

for (k in grep("strip-l",q$layout$name)) {
  q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
}

grid.draw(q)
  
#ggsave(filename = "CopenhagenPlot_NCD.tiff", path = "H:/KT22SVN/X30/PhD_Study1/Results_plots", units = "px", width = 4400, height = 5000, device='tiff', dpi=500, q)
  
    

### Copenhagen plot for most prevalent diseases/NCDs - GBD

ASIR_CD_GBD <- dbReadTable(conn_inci, "ASIR_CD_GBD")
N_level4 <- dbReadTable(conn_inci, "N_level4")

N_CD <- N_level4 %>% 
  mutate(CD = ifelse(DIAG >= "DA15" & DIAG <= "DA19" | DIAG == "DB90", "Tuberculosis", NA),
         CD = ifelse(DIAG >= "DA50" & DIAG <= "DA53", "Syphilis", CD),
         CD = ifelse(DIAG >= "DA55" & DIAG <= "DA56", "Chlamydia", CD),
         CD = ifelse(DIAG == "DA54", "Gonorrhea", CD),
         CD = ifelse(DIAG >= "DA57" & DIAG <= "DA64" | DIAG >= "DN70" & DIAG <= "DN73", "Other sexually transmitted diseases", CD),
         CD = ifelse(DIAG >= "DB20" & DIAG <= "DB24", "HIV/AIDS", CD),
         CD = ifelse(DIAG >= "DA00" & DIAG <= "DA01" | DIAG >= "DA03" & DIAG <= "DA04" | DIAG >= "DA06" & DIAG <= "DA09", "Diarrheal diseases", CD),
         CD = ifelse(DIAG == "DA37", "Pertussis", CD),
         CD = ifelse(DIAG == "DA80" | DIAG == "DB91", "Poliomyelitis", CD),
         CD = ifelse(DIAG == "DA36", "Diphtheria", CD),
         CD = ifelse(DIAG == "DB05", "Measles", CD),
         CD = ifelse(DIAG >= "DA33" & DIAG <= "DA35", "Tetanus", CD),
         CD = ifelse(DIAG == "DA39" | DIAG == "DG00" | DIAG == "DG03", "Meningitis", CD),
         CD = ifelse(DIAG == "DB15", "Hepatitis A", CD),
         CD = ifelse(DIAG == "DB16" | DIAG4 == "DB170" | DIAG4 == "DB180" | DIAG4 == "DB181" | DIAG == "DB19", "Hepatitis B", CD),
         CD = ifelse(DIAG4 == "DB171" | DIAG4 == "DB182", "Hepatitis C", CD),
         CD = ifelse(DIAG >= "DB50" & DIAG <= "DB54", "Malaria", CD),
         CD = ifelse(DIAG == "DB56", "Trypanosomiasis", CD),
         CD = ifelse(DIAG == "DB57", "Chagas' disease", CD),
         CD = ifelse(DIAG == "DB65", "Schistosomiasis", CD),
         CD = ifelse(DIAG == "DB55", "Leishmaniasis", CD),
         CD = ifelse(DIAG4 >= "DB740" & DIAG4 <= "DB742", "Lymphatic filariasis", CD),
         CD = ifelse(DIAG == "DB73", "Onchocerciasis", CD),
         CD = ifelse(DIAG == "DA30", "Leprosy", CD),
         CD = ifelse(DIAG >= "DA90" & CD <= "DA91", "Dengue", CD),
         CD = ifelse(DIAG4 == "DA830", "Japanese encephalitis", CD),
         CD = ifelse(DIAG == "DA71", "Trachoma", CD),
         CD = ifelse(DIAG == "DB77", "Ascariasis", CD),
         CD = ifelse(DIAG == "DB79", "Trichuriasis", CD),
         CD = ifelse(DIAG == "DB76", "Hookworm disease", CD),
         CD = ifelse(DIAG == "DB78" | DIAG == "DB80" | DIAG == "DB81", "Other intestinal infections", CD),
         CD = ifelse(DIAG == "DA02" | DIAG == "DA05" | DIAG >= "DA20" & DIAG <= "DA28" | DIAG == "DA31" | DIAG == "DA32" | DIAG == "DA38" | 
                       DIAG <= "DA40" & DIAG >= "DA49" | DIAG >= "DA65" & DIAG <= "DA70" | DIAG >= "DA74" & DIAG <= "DA79" | DIAG >= "DA81" & DIAG <= "DA82" | 
                       DIAG4 >= "DA831" & DIAG4 <= "DA839" | DIAG >= "DA84" & DIAG <= "DA89" | DIAG >= "DA92" & DIAG <= "DA99" | DIAG >= "DB00" & DIAG <= "DB04" |
                       DIAG >= "DB06" & DIAG <= "DB15" | DIAG >= "DB25" & DIAG <= "DB49" | DIAG >= "DB58" & DIAG <= "DB60" | DIAG == "DB64" | DIAG >= "DB66" & DIAG <= "DB72" |
                       DIAG4 >= "DB743" & DIAG4 <= "DB749" | DIAG == "DB75" | DIAG >= "DB82" & DIAG <= "DB89" | DIAG >= "DB92" & DIAG <= "DB99" | DIAG == "DG04", "Other infectious diseases", CD),
         CD = ifelse(DIAG >= "DJ10" & DIAG <= "DJ18" | DIAG >= "DJ20" & DIAG <= "DJ22", "Lower respiratory infections", CD),
         CD = ifelse(DIAG >= "DJ00" & DIAG <= "DJ06", "Upper respiratory infections", CD),
         CD = ifelse(DIAG >= "DH65" & DIAG <= "DH66", "Otitis media", CD)) %>% 
  group_by(CD) %>% 
  summarise(prop = sum(prop),
            N = sum(N))%>% 
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
  filter(!is.na(CD))

ASIR_CD_GBD <- ASIR_CD_GBD %>% 
  left_join(N_CD, by = "CD") %>% 
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
         CD = ifelse(CD == "Otitis media", "RC: Otitis media", CD)) %>% 
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
         order = ifelse(GBD_cause_name == "Respiratory infections", 18, order)) 


ASIR_CD_GBD <- ASIR_CD_GBD %>% 
  filter(!is.na(GBD_cause_name))


Copenhagen_CD_GBD <- ASIR_CD_GBD %>% 
  mutate(udd = recode(udd, "1" = "udd1",
                      "2" = "udd2",
                      "3" = "udd3")) %>% 
  pivot_wider(names_from = udd, values_from = ASIR) %>% 
  mutate(RR1vs2 = udd1/udd2,
         RR3vs2 = udd3/udd2,
         RR3vs1 = udd3/udd1)%>% 
  dplyr::select(CD, RR1vs2, RR3vs2, GBD_cause_name, order, prop) %>% 
  pivot_longer(2:3, names_to = "type", values_to = "RR") %>% 
  arrange(CD) %>% 
  arrange(-RR)


# make variable indicating highest RR
test <- Copenhagen_CD_GBD %>% 
  group_by(CD) %>% 
  filter(RR == max(RR)) %>% 
  mutate(balance = type) %>% 
  select(CD, balance)

Copenhagen_CD_GBD <- left_join(Copenhagen_CD_GBD, test, by = "CD")


#plot
p <- ggplot(Copenhagen_CD_GBD, aes(x = log(RR), y = CD, color = as.factor(type), size = prop)) +
  geom_point(aes(size = prop))+ 
  geom_line(aes(group = CD, color = balance, size = 0.0001), alpha = 0.4) +
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
  scale_color_discrete(name = "Education", labels = c("Low vs. Medium", "High vs. Medium")) +
  scale_size_continuous(name = "Proportion") +
  scale_alpha_continuous(name = "Proportion") +
  geom_vline(xintercept = log(1), col = "black", linetype="dashed") +
  labs(x = 'log(Incidence rate ratio)', y = 'Communicable diseases') +
  ggtitle("Age-standardised incidence rate ratios for CDs in 2012-2021 presented on a log scale") +
  scale_y_discrete(limits=rev, label = function(CD){
    CD %>% sub("....", "", .)
  })+
  xlim(-1.8,1.8)



# Making a line for each GBD_cause_name to indicate length/size of group
library(grid)
q <- ggplotGrob(p)
lg <- linesGrob(x=unit(c(1,1),"npc"), y=unit(c(1,0),"npc"), 
                gp=gpar(col="black", lwd=3))

for (k in grep("strip-l",q$layout$name)) {
  q$grobs[[k]]$grobs[[1]]$children[[1]] <- lg
}

grid.draw(q)

#ggsave(filename = "CopenhagenPlot_CD_GBD.tiff", path = "H:/KT22SVN/X30/PhD_Study1/Results_plots", units = "px", width = 4400, height = 1500, device='tiff', dpi=500)




