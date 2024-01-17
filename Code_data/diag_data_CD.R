library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)

###############################################################################
# The following script creates a dataset with all first time diagnoses of CDs.#
# CD are defined according to GBD.                                           #                                                               
###############################################################################

########## get diag data ############

conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
diags_all_level4 <- DBI::dbReadTable(conn = conn_inci, name = "diags_all_level4")


########## group diags into ECDCs categories of CDs ############

CD <- diags_all_level4 %>% 
  dplyr::select(DIAG, DIAG4) %>% 
  unique() 


CD <- CD %>% 
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
         CD = ifelse(DIAG >= "DH65" & DIAG <= "DH66", "Otitis media", CD))


## Group CDs
CD <- CD %>% 
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
         GBD_cause_name = ifelse(CD == "Otitis media", "Respiratory infections", GBD_cause_name))

         
########## left_join CDs to diags_CD to make df only with CDs ############
# While doing this, we also make sure, that only the first time diagnosis is used.
# Hence, for CDs where the third level is used (e.g. DA15 for tuberculosis) the first
# time diagnosis at this level is found and for CDs where the fourth level is used 
# (e.g. DB181 for Hepatitis B) the first time diagnosis at this level is found. 

diags_CD <- diags_all_level4 %>% 
  left_join(select(CD, DIAG4, CD, GBD_cause_name), by = "DIAG4") %>% 
  filter(!is.na(GBD_cause_name)) %>% 
  mutate(diag_type = ifelse(CD == "Hepatitis C" | CD == "Lymphatic filariasis" | CD == "Japanese encephalitis" | 
                              DIAG4 >= "DA831" & DIAG4 <= "DA839" | DIAG4 >= "DB743" & DIAG4 <= "DB749" | 
                              DIAG4 == "DB170" | DIAG4 == "DB180" | DIAG4 == "DB181", 0, 1))


diags_CD_check0 <- diags_CD_test %>% 
  filter(diag_type == 0) %>% 
  group_by(PERSON_ID, DIAG4) %>% 
  filter(D_INDDTO == min(D_INDDTO)) %>% 
  unique()
  
diags_CD_check1 <- diags_CD_test %>% 
  filter(diag_type == 1) %>% 
  group_by(PERSON_ID, DIAG) %>% 
  filter(D_INDDTO == min(D_INDDTO)) %>% 
  unique()

diags_CD <- rbind(diags_CD_check0, diags_CD_check1)

         
########## save data ############
         
dbWriteTable(conn_inci, "diags_CD", diags_CD, overwrite = TRUE)
         
         
         
         