library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)

###############################################################################
# The following script creates a dataset with all first time diagnoses of CDs.#
# CD are defined according to the definitions used in ECDC's annual           #
# epidemiological report.                                                     #                                                               
###############################################################################

########## get diag data ############

conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
diags_all_level4 <- DBI::dbReadTable(conn = conn_inci, name = "diags_all_level4")



########## group diags into ECDCs categories of CDs ############

CD <- diags_all_level4 %>% 
  dplyr::select(DIAG, DIAG4) %>% 
  unique() 

CD <- CD %>% 
  mutate(CD = ifelse(DIAG == "DA22", "Anthrax", NA),
         CD = ifelse(DIAG4 == "DA051", "Botulism", CD), 
         CD = ifelse(DIAG == "DA23", "Brucellosis", CD),
         CD = ifelse(DIAG4 == "DA045", "Campylobacteriosis", CD), 
         CD = ifelse(DIAG4 == "DA920", "Chikungunya", CD), 
         CD = ifelse(DIAG >= "DA55" & DIAG <= "DA56", "Clamydia infection", CD), 
         CD = ifelse(DIAG == "DA00", "Cholera", CD),
         CD = ifelse(DIAG4 == "DA810", "Creutzfeldt_Jakob disease", CD),
         CD = ifelse(DIAG4 == "DA980", "Crimean-Congo haemorrhagic fever", CD), 
         CD = ifelse(DIAG4 == "DA072", "Cryptosporidiosis", CD), 
         CD = ifelse(DIAG == "DA90" | DIAG == "DA91", "Dengue", CD), 
         CD = ifelse(DIAG == "DA36", "Diphtheria", CD), 
         CD = ifelse(DIAG4 == "DA984", "Ebola and Marburg virus diseases", CD), 
         CD = ifelse(DIAG == "DB67", "Echinococcosis", CD), 
         CD = ifelse(DIAG4 == "DA071", "Giardiasis (lambliasis)", CD), 
         CD = ifelse(DIAG == "DA54", "Gonorrhea", CD), 
         CD = ifelse(DIAG4 == "DA492" | DIAG4 == "DB963", "Haemophilus influenzae infection", CD), 
         CD = ifelse(DIAG4 == "DB334", "Hantavirus infection", CD),
         CD = ifelse(DIAG4 == "DA047", "Healthcare-associated infections: Clostridium difficile infections", CD), 
         CD = ifelse(DIAG == "DB15", "Hepatitis A", CD), 
         CD = ifelse(DIAG == "DB16" | DIAG4 == "DB170" | DIAG4 == "DB180" | DIAG4 == "DB181" | DIAG4 == "DB191", "Hepatitis B", CD), 
         CD = ifelse(DIAG4 == "DB171" | DIAG4 == "DB182" | DIAG4 == "DB192", "Hepatitis C", CD), 
         CD = ifelse(DIAG >= "DB20" & DIAG <= "DB24", "HIV/AIDS", CD), 
         CD = ifelse(DIAG >= "DJ09" & DIAG <= "DJ11", "Influenza", CD),
         CD = ifelse(DIAG4 == "DA962", "Lassa fever", CD), 
         CD = ifelse(DIAG4 == "DA481", "Legionnaires' disease", CD), 
         CD = ifelse(DIAG == "A27", "Leptospirosis", CD), 
         CD = ifelse(DIAG == "DA32", "Listeriosis", CD), 
         CD = ifelse(DIAG >= "DB50" & DIAG <= "DB54", "Malaria", CD), 
         CD = ifelse(DIAG == "DA39", "Meningococcal infection", CD), 
         CD = ifelse(DIAG == "DB05", "Measles", CD), 
         CD = ifelse(DIAG == "DB36", "Mumps", CD), 
         CD = ifelse(DIAG == "DA37", "Pertussis", CD), 
         CD = ifelse(DIAG == "DA20", "Plague", CD), 
         CD = ifelse(DIAG == "DA80" | DIAG == "DB91", "Poliomyelitis", CD), 
         CD = ifelse(DIAG == "DA78", "Q fever", CD), 
         CD = ifelse(DIAG == "DA82", "Rabies", CD), 
         CD = ifelse(DIAG4 == "DA924", "Rift Valley fever", CD), 
         CD = ifelse(DIAG == "DB06", "Rubella", CD), 
         CD = ifelse(DIAG == "DA02", "Salmonellosis", CD), 
         CD = ifelse(DIAG4 == "DB962", "Escherichia coli (E. coli) infection", CD), 
         CD = ifelse(DIAG == "DA03", "Shigellosis", CD), 
         CD = ifelse(DIAG == "DB03", "Smallpox", CD), 
         CD = ifelse(DIAG >= "DA50" & DIAG <= "DA53", "Syphilis", CD), 
         CD = ifelse(DIAG >= "DA33" & DIAG <= "DA35", "Tetanus", CD), 
         CD = ifelse(DIAG == "DA84", "Tick-borne ecephalitis", CD), 
         CD = ifelse(DIAG == "DB75", "Trichinellosis", CD), 
         CD = ifelse(DIAG >= "DA15" & DIAG <= "DA19" | DIAG == "DB90", "Tuberculosis", CD),
         CD = ifelse(DIAG == "DA21", "Tularaemia", CD), 
         CD = ifelse(DIAG == "DA01", "Typhoid and paratyphoid", CD), 
         CD = ifelse(DIAG4 == "DA923", "West Nile virus infection", CD), 
         CD = ifelse(DIAG == "DA95", "Yellow fever", CD), 
         CD = ifelse(DIAG4 == "DA282" | DIAG4 == "DA046", "Yersiniosis", CD), 
         CD = ifelse(DIAG4 == "DA925", "Zika virus disease", CD), 
         CD = ifelse(DIAG >= "DJ00" & DIAG <= "DJ06", "Upper respiratory infections", CD), 
         CD = ifelse(DIAG >= "DJ12" & DIAG <= "DJ22", "Lower respiratory infections", CD), 
         CD = ifelse(DIAG >= "DH65" & DIAG <= "DH66", "Otitis media", CD))


########## left_join CDs to diags_CD to make df only with CDs ############
# While doing this, we also make sure, that only the first time diagnosis is used.
# Hence, for CDs where the third level is used (e.g. DA15 for tuberculosis) the first
# time diagnosis at this level is found and for CDs where the fourth level is used 
# (e.g. DB181 for Hepatitis B) the first time diagnosis at this level is found. 

diags_CD <- diags_all_level4 %>% 
  left_join(select(CD, DIAG4, CD), by = "DIAG4") %>% 
  filter(!is.na(CD)) %>% 
  mutate(diag_type = ifelse(CD == "Botulism" | CD == "Campylobacteriosis" | CD == "Chikungunya" | 
                              CD == "Creutzfeldt_Jakob disease" | CD == "Crimean-Congo haemorrhagic fever" | CD == "Cryptosporidiosis" | 
                              CD == "Ebola and Marburg virus diseases" | CD == "Giardiasis (lambliasis)" | CD == "Haemophilus influenzae infection" | 
                              CD == "Hantavirus infection" | CD == "Healthcare-associated infections: Clostridium difficile infections" | CD == "Hepatitis C" | 
                              CD == "Lassa fever" | CD == "Legionnaires' disease" | CD == "Rift Valley fever" | 
                              CD == "Escherichia coli (E. coli) infection" | CD == "West Nile virus infection" | CD == "Yersiniosis" | 
                              CD == "Zika virus disease" | 
                              DIAG4 == "DB170" | DIAG4 == "DB180" | DIAG4 == "DB181" | DIAG4 == "DB191", 0, 1))


diags_CD_check0 <- diags_CD %>% 
  filter(diag_type == 0)

diags_CD_check1 <- diags_CD %>% 
  filter(diag_type == 1) %>% 
  group_by(PERSON_ID, DIAG) %>% 
  filter(D_INDDTO == min(D_INDDTO)) %>% 
  unique()

diags_CD <- rbind(diags_CD_check0, diags_CD_check1)


########## save data ############

dbWriteTable(conn_inci, "diags_CD_ECDC", diags_CD, overwrite = TRUE)



