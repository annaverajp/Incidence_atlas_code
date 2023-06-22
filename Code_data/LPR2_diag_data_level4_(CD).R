###### Copy of Elisabeths code
# Loads and saves the diagnosis data for the project of incidence of everything
# Input: MERGED_LINES @ D220005, 
#        LPRADM, LPRDIAG, 
#        PRIVADM, PRIVDIAG, 
#        UAFADM, UAFDIAG, 
#        PSYKADM,  PSYKDIAG @ D222008
#        ICD_MAPPING_CLEANED @ D222010
# Output: DIAG_DATA OR DIAG_DATA_ONLY_A @ D222010
t0 <- Sys.time()

# Initialization ----------------------------------------------------------

library(tidyverse)
library(data.table)
library(DSTora)
library(DBI)
library(ROracle)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 
setDTthreads(8) # Do no exceed maximum allowed threads
source('H:/KT22SVN/Challenge/Familial aggregation/Data pre-processing/LPR_functions.R')

study_end <- as.Date('2021-12-31')

only_A_diag <- FALSE # Only save A-diagnoses if TRUE. Otherwise use A and B



# Functions used in script ------------------------------------------------

# Find entries with correct diagnosis codes
get_icd10_diagnosis_data <- function(icd10_codes, pop_id, only_A_diag = FALSE){
  t0 <- Sys.time()
  diags <- list()
  
  if(only_A_diag == FALSE){
    AB_selection <- "(C_DIAGTYPE = 'A' OR C_DIAGTYPE = 'B')"
  }
  
  icd10_codes <- sort(icd10_codes)
  
  # Get batch of 100 diagnoses (otherwise the database complains) 
  n_batches <- ceiling(length(icd10_codes)/100)
  for(i in 1:n_batches){
    cat('batch', i, '/', n_batches, '\n')
    

    idx_lwr = (i-1)*100+1 
    idx_upr = min(i*100, length(icd10_codes))
    icd10_codes_batch = icd10_codes[idx_lwr:idx_upr]
    
    # Get codes
    diags[[i]] <- trawlDiagnosisData(
      codes = icd10_codes_batch, 
      only_A_diag = only_A_diag, 
      exact = FALSE, 
      psyk = TRUE,
      priv = TRUE,
      uaf = TRUE,
      print_sql = FALSE, 
      where_statement = AB_selection
    ) %>%
      rename_all(tolower) %>% 
      filter(person_id %in% pop_id) %>% 
      rename(diag = any_of(c('c_diag', 'c_adiag'))) %>% 
      mutate(diag = substr(str_trim(diag), 1, 5)) %>% 
      get_first_diag(.) %>% 
      filter(diag %in% icd10_codes)
    
  }
  diags <- bind_rows(diags) %>% select(person_id, diag, d_inddto, d_uddto, source)
  t1 <- Sys.time()
  cat('Time to get icd10-data:', difftime(t1, t0, units = c('mins')), '\n')
  return(diags)
}

# Gets first instance of an diagnosis for a person
get_first_diag <- function(diags){
  diags <- setDT(diags)
  diags <- unique(diags[order(d_inddto)], by = c("person_id", "diag"))
  diags <- setDF(diags)
}

# Find entries with icd8 codes corresponding to the icd10_codes
get_icd8_diagnosis_data <- function(icd10_codes, icd_map, pop_id, only_A_diag = FALSE){
  t0 <- Sys.time()
  
  if(only_A_diag == FALSE){
    AB_selection <- "(C_DIAGTYPE = 'A' OR C_DIAGTYPE = 'B')"
  }
  
  # Loading diagnoses with correct icd8-code
  icd8_codes <- icd_map %>% 
    filter(diag %in% icd10_codes) %>% # Only codes mapping to correct icd10
    arrange(icd8_code) %>%
    pull(icd8_code) %>% 
    unique()
  
  diags_icd8 <- list()
  n_batches <- ceiling(length(icd8_codes)/1000)
  for(i in 1:n_batches){
    cat('batch', i, '/', n_batches, '\n')
    
    idx_lwr = (i-1)*1000+1 
    idx_upr = min(i*1000, length(icd8_codes))
    icd8_codes_batch = icd8_codes[idx_lwr:idx_upr]
    
    diags_icd8[[i]] <- trawlDiagnosisData(
      codes = icd8_codes_batch, 
      only_A_diag = only_A_diag, 
      exact = TRUE, 
      psyk = TRUE,
      priv = TRUE, 
      uaf = TRUE,
      print_sql = FALSE,
      where_statement = AB_selection)
    
  }
  diags_icd8 <- bind_rows(diags_icd8) %>% rename_all(tolower)

  
  # Mapping from icd8 to icd10 + getting first time diagnosis was given
  diags_icd8 <- diags_icd8 %>% 
    rename(icd8_diag = any_of(c('c_diag', 'c_adiag'))) %>%
    filter(person_id %in% pop_id) %>% 
    left_join(.,icd_map, by = c('icd8_diag' = 'icd8_code')) %>% 
    # Only map diagnoses where we know when diagnosis was given and valid map exists
    filter(from_date <= d_uddto & d_uddto <= to_date & !is.na(diag) & !is.na(d_uddto)) %>% 
    # Keep only codes that we're interested in
    filter(diag %in% icd10_codes) %>% 
    get_first_diag(.) %>% 
    select(person_id, diag, d_inddto, d_uddto, source)
  
  t1 <- Sys.time()
  cat('Time to get icd8-data:', difftime(t1, t0, units = c('mins')), '\n')
  
  return(diags_icd8)
  
}


# Get population ids ------------------------------------------------------
# Only interested in diagnoses for individuals in incidence data
conn_LL <- OraGenvej(dbname = "STATPROD", dbuser = "[D220005]")

pop_id <- DBI::dbReadTable(conn = conn_LL, name = "MERGED_LINES") %>% 
  pull(PERSON_ID) %>% 
  unique()


# Making list of all abbreviated icd10 codes ------------------------------
conn_lpr <- DSTora::OraGenvej('STATPROD', '[D222008]')

sql <- glue::glue_sql(
  "SELECT DISTINCT(C_DIAG) AS ICD10_CODE FROM D222008.{`table_name`} WHERE C_DIAG LIKE 'D%'", 
  .con = conn_lpr,
  table_name = c('LPRDIAG', 'PRIVDIAG', 'UAFDIAG', 'PSYKDIAG'))

icd10_codes_lpr <- DBI::dbGetQuery(conn_lpr,  sql[1])
icd10_codes_priv <- DBI::dbGetQuery(conn_lpr,  sql[2])
icd10_codes_uaf <- DBI::dbGetQuery(conn_lpr,  sql[3])
icd10_codes_psyk <- DBI::dbGetQuery(conn_lpr,  sql[4])

icd10_codes <- bind_rows(icd10_codes_lpr, icd10_codes_priv, icd10_codes_uaf, icd10_codes_psyk) %>% 
  mutate(DIAG = str_sub(str_trim(ICD10_CODE), 1, 5)) %>% 
  distinct(DIAG) %>%
  # Only need ICD10 chapters 1-19 which ends with DT983
  filter(DIAG <= 'DT983') %>% 
  # Only codes in pattern of two letters followed by two digits (to avoid searching for typos)
  filter(str_detect(DIAG, '[:alpha:]{2}[:digit:]{2}')) %>% 
  pull(DIAG)
length(icd10_codes) # 10133 codes

DBI::dbDisconnect(conn_lpr)


# Prepare mapping between icd8 and icd10 ----------------------------------

conn_famagg <- DSTora::OraGenvej('STATPROD', '[D222010]')

icd_map <- DBI::dbReadTable(conn_famagg, 'ICD_MAPPING_CLEANED') %>% 
  rename_all(tolower)

# Gathering data ----------------------------------------------------------
# 78 minutes in total, only A diags -> 20 minutes total 
#icd10_codes_sample <- c(sample(icd10_codes, 10), 'DE10')
diags_icd10 <- get_icd10_diagnosis_data(icd10_codes, pop_id, only_A_diag = only_A_diag) 
diags_icd8 <- get_icd8_diagnosis_data(icd10_codes, icd_map, pop_id, only_A_diag = only_A_diag)

diags <- bind_rows(diags_icd10, diags_icd8)
rm(diags_icd10, diags_icd8)

diags <- diags %>% 
  filter(d_inddto <= study_end) %>% 
  get_first_diag(.)


# Writing data ------------------------------------------------------------
colnames(diags) <- c("PERSON_ID", "DIAG", "D_INDDTO", "D_UDDTO", "SOURCE")

conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
DBI::dbWriteTable(conn_inci, 'diags_LPR2_level4', diags, overwrite = TRUE)

dbListTables(conn_inci)  


