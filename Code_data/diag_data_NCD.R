library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)

###############################################################################
# The following script creates a dataset with all first time diagnoses of NCDs#                                                               
###############################################################################

########## get diag data ############

conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
diags_all_level3 <- DBI::dbReadTable(conn = conn_inci, name = "diags_all_level3")


########## group diags into GBD categories of NCDs ############

NCD <- diags_all_level3 %>% 
  dplyr::select(DIAG) %>% 
  unique() 


NCD <- NCD %>% 
  mutate(NCD = DIAG, 
         NCD = ifelse(NCD >= "DC00" & NCD <= "DC14", "Mouth and oropharynx cancers", NCD),
         NCD = ifelse(NCD == "DC15", "Esophageal cancer", NCD),
         NCD = ifelse(NCD == "DC16", "Stomach cancer", NCD),
         NCD = ifelse(NCD >= "DC18" & NCD <= "DC21", "Colon and rectal cancers", NCD),
         NCD = ifelse(NCD == "DC22", "Liver cancer", NCD),
         NCD = ifelse(NCD == "DC25", "Pancreas cancer", NCD),
         NCD = ifelse(NCD == "DC33" | NCD == "DC34", "Trachea, bronchus, and lung cancers", NCD),
         NCD = ifelse(NCD == "DC43" | NCD == "DC44", "Melanoma and other skin cancers", NCD),
         NCD = ifelse(NCD == "DC50", "Breast cancer", NCD),
         NCD = ifelse(NCD == "DC53", "Cervix uteri cancer", NCD),
         NCD = ifelse(NCD == "DC54" | NCD == "DC55", "Corpus uteri cancer", NCD),
         NCD = ifelse(NCD == "DC56", "Ovarian cancer", NCD),
         NCD = ifelse(NCD == "DC61", "Prostate cancer", NCD),
         NCD = ifelse(NCD == "DC67", "Bladder cancer", NCD),
         NCD = ifelse(NCD >= "DC81" & NCD <= "DC90" | NCD == "DC96", "Lymphomas and multiple myeloma", NCD),
         NCD = ifelse(NCD >= "DC91" & NCD <= "DC95", "Leukemia", NCD),
         NCD = ifelse(NCD == "DC17" | NCD >= "DC23" & NCD <= "DC32" | NCD >= "DC37" & NCD <= "DC41" | NCD >= "DC45" & NCD <= "DC49" | 
                        NCD == "DC51" | NCD == "DC52" | NCD >= "DC57" & NCD <= "DC60" | NCD >= "DC62" & NCD <= "DC66" | 
                        NCD >= "DC68" & NCD <= "DC80" | NCD == "DC97", "Other malignant neoplasms", NCD),
         NCD = ifelse(NCD >= "DD00" & NCD <= "DD48", "Other neoplasms", NCD),
         NCD = ifelse(NCD >= "DE10" & NCD <= "DE14", "Diabetes mellitus", NCD),
         NCD = ifelse(NCD >= "DD55" & NCD <= "DD89" | NCD >= "DE03" & NCD <= "DE07" | NCD == "DE15" | NCD == "DE16" | 
                        NCD >= "DE20" & NCD <= "DE34" | NCD >= "DE65" & NCD <= "DE88", "Endocrine disorders", NCD),
         NCD = ifelse(NCD == "DF32" | NCD == "DF33", "Unipolar depressive disorders", NCD),
         NCD = ifelse(NCD == "DF30" | NCD == "DF31", "Bipolar affective disorders", NCD),
         NCD = ifelse(NCD >= "DF20" & NCD <= "DF29", "Schizophrenia", NCD),
         NCD = ifelse(NCD == "DG40" | NCD == "DG41", "Epilepsi", NCD),
         NCD = ifelse(NCD == "DF10", "Alcohol use disorders", NCD),
         NCD = ifelse(NCD == "DF01" | NCD == "DF03" | NCD == "DG30" | NCD == "DG31", "Alzheimer's disease and other dementias", NCD),
         NCD = ifelse(NCD == "DG20" | NCD == "DG21", "Parkinson's disease", NCD),
         NCD = ifelse(NCD == "DG35", "Multiple schlerosis", NCD),
         NCD = ifelse(NCD >= "DF11" & NCD <= "DF16" | NCD == "DF18" | NCD == "DF19", "Drug use disorders", NCD),
         NCD = ifelse(NCD == "DF43", "Reaction to severe stress, and adjustment disorders", NCD),
         NCD = ifelse(NCD == "DF42", "Obsessive-compulsive disorder", NCD),
         NCD = ifelse(NCD == "DF40" | NCD == "DF41", "Anxiety disorders", NCD),
         NCD = ifelse(NCD == "DF51", "Insomnia (primary)", NCD),
         NCD = ifelse(NCD == "DG43", "Migraine", NCD),
         NCD = ifelse(NCD >= "DF04" & NCD <= "DF09" | NCD == "DF17" | NCD >= "DF34" & NCD <= "DF39" | NCD >= "DF44" & NCD <= "DF50" | 
                        NCD <= "DF52" & NCD >= "DF69" | NCD >= "DF80" & NCD <= "DF99" | NCD >= "DG06" & NCD <= "DG12" | NCD >= "DG23" & NCD <= "DG25" | 
                        NCD == "DG36" | NCD == "DG37" | NCD >= "DG44" & NCD <= "DG98", "Other neuropsychiatric disorders", NCD),
         NCD = ifelse(NCD == "DH40", "Glaucoma", NCD),
         NCD = ifelse(NCD == "DH25" | NCD == "DH26", "Cataracts", NCD),
         NCD = ifelse(NCD == "DH90" | NCD == "DH91", "Hearing loss, adult onset", NCD),
         NCD = ifelse(NCD >= "DH00" & NCD <= "DH21" | NCD >= "DH27" & NCD <= "DH35" | NCD >= "DH43" & NCD <= "DH61" | NCD >= "H68" & NCD <= "DH83" |
                        NCD == "DH92" | NCD == "DH93", "Other sense organ disorders", NCD),
         NCD = ifelse(NCD >= "DI01" & NCD <= "DI09", "Rheumatic heart disease", NCD),
         NCD = ifelse(NCD >= "DI10" & NCD <= "DI13", "Hypertensive heart disease", NCD),
         NCD = ifelse(NCD >= "DI20" & NCD <= "DI25", "Ischemic heart disease", NCD),
         NCD = ifelse(NCD >= "DI60" & NCD <= "DI69", "Cerebrovascular disease", NCD),
         NCD = ifelse(NCD >= "DI30" & NCD <= "DI33" | NCD == "DI38" | NCD == "DI40" | NCD == "DI42", "Inflammatory heart diseases", NCD),
         NCD = ifelse(NCD == "DI00" | NCD >= "DI26" & NCD <= "DI28" | NCD >= "DI34" & NCD <= "DI37" | NCD >= "DI44" & NCD <= "DI51" | 
                        NCD >= "DI70" & NCD <= "DI99", "Other cardiovascular diseases", NCD),
         NCD = ifelse(NCD >= "DJ40" & NCD <= "DJ44", "Chronic obstructive pulmonary disease", NCD),
         NCD = ifelse(NCD >= "DJ45" & NCD <= "DJ46", "Asthma", NCD),
         NCD = ifelse(NCD >= "DJ30" & NCD <= "DJ39" | NCD >= "DJ47" & NCD <= "DJ98", "Other respiratory diseases", NCD),
         NCD = ifelse(NCD >= "DK25" & NCD <= "DK27", "Peptic ulcer disease", NCD),
         NCD = ifelse(NCD == "DK70" | NCD == "DK74", "Cirrhosis of the liver", NCD),
         NCD = ifelse(NCD >= "DK35" & NCD <= "DK37", "Appendicitis", NCD),
         NCD = ifelse(NCD >= "DK20" & NCD <= "DK22" | NCD >= "DK28" & NCD <= "DK31" | NCD == "DK38" | NCD >= "DK40" & NCD <= "DK66" | 
                        NCD >= "DK71" & NCD <= "DK73" | NCD >= "DK75" & NCD <= "DK92", "Other digestive diseases", NCD),
         NCD = ifelse(NCD >= "DN00" & NCD <= "DN19", "Nephritis and neophrosis", NCD),
         NCD = ifelse(NCD == "DN40", "Benign prostatic hypertrophy", NCD),
         NCD = ifelse(NCD >= "DN20" & NCD <= "DN39" | NCD >= "DN41" & NCD <= "DN64" | NCD >= "DN75" & NCD <= "DN98", "Other genitourinary system diseases", NCD),
         NCD = ifelse(NCD >= "DL00" & NCD <= "DL98", "Skin diseases", NCD),
         NCD = ifelse(NCD >= "DM05" & NCD <= "DM06", "Rheumatoid arthritis", NCD),
         NCD = ifelse(NCD >= "DM15" & NCD <= "DM19", "Osteoarthritis", NCD),
         NCD = ifelse(NCD == "DM10", "Gout", NCD),
         NCD = ifelse(NCD >= "DM45" & NCD <= "DM48" | NCD == "DM54", "Low back pain", NCD),
         NCD = ifelse(NCD >= "DM00" & NCD <= "DM02" | NCD == "DM08" | NCD >= "DM11" & NCD <= "DM13" | NCD >= "DM20" & NCD <= "DM43" | 
                        NCD >= "DM50" & NCD <= "DM53" | NCD >= "DM55" & NCD <= "DM99", "Other muscuskeletal disorders", NCD),
         NCD = ifelse(NCD == "DK02", "Dental caries", NCD),
         NCD = ifelse(NCD == "DK05", "Periodontal disease", NCD),
         NCD = ifelse(NCD >= "DK00" & NCD <= "DK04" | NCD >= "DK06" & NCD <= "DK14", "Other oral diseases", NCD))


## Group NCDs
NCD <- NCD %>% 
  mutate(GBD_cause_name = ifelse(NCD == "Mouth and oropharynx cancers", "Malignant neoplasms", NA),
         GBD_cause_name = ifelse(NCD == "Esophageal cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Stomach cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Colon and rectal cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Liver cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Pancreas cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Trachea, bronchus, and lung cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Melanoma and other skin cancers", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Breast cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Cervix uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Corpus uteri cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Ovarian cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Prostate cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Bladder cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Lymphomas and multiple myeloma cancer", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Leukemia", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other malignant neoplasms", "Malignant neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other neoplasms", "Other neoplasms", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Diabetes mellitus", "Diabetes mellitus", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Endocrine disorders", "Endocrine disorders", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Unipolar depressive disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Bipolar affective disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Schizophrenia", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Epilepsi", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Alcohol use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Alzheimer's disease and other dementias", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Parkinson's disease", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Multiple sclerosis", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Drug use disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Reaction to severe stress, and adjustment disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Obsessive-compulsive disorder", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Anxiety disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Insomnia (primary)", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Migraine", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other neuropsychiatric disorders", "Neuropsychiatric conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Glaucoma", "Sense organ disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Cataracts", "Sense organ disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Hearing loss, adult onset", "Sense organ disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other sense organ disorders", "Sense organ disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Rheumatic heart disease", "Cardiovascular disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Hypertensive heart disease", "Cardiovascular disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Ischemic heart disease", "Cardiovascular disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Cerebrovascular disease", "Cardiovascular disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Inflammatory heart disease", "Cardiovascular disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other cardiovascular diseases", "Cardiovascular disease", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Chronic obstructive pulmonary disease", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Asthma", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other respiratory diseases", "Respiratory diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Peptic ulcer disease", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Cirrhosis of the liver", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Appendicitis", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other digestive diseases", "Digestive diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Nephritis and nephrosis", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Benign prostatic hypertrophy", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other genitourinary system diseases", "Genitourinary diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Skin diseases", "Skin diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Rheumatoid arthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Oesteoarthritis", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Gout", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Low back pain", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other musculoskeletal disorders", "Musculoskeletal diseases", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Dental caries", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Periodontal disease", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Edentulism", "Oral conditions", GBD_cause_name),
         GBD_cause_name = ifelse(NCD == "Other oral diseases", "Oral conditions", GBD_cause_name))


########## left_join NCDs to diags_all_level3 to make df only with NCDs ############
diags_NCD <- diags_all_level3 %>% 
  left_join(NCD, by = "DIAG") %>% 
  filter(!is.na(GBD_cause_name)) %>% 
  dplyr::select(PERSON_ID, D_INDDTO, DIAG, NCD, GBD_cause_name) %>% 
  unique()
  

########## save data ############

dbWriteTable(conn_inci, "diags_NCD", diags_NCD, overwrite = TRUE)



