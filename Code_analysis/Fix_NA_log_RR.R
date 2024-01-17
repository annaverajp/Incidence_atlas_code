library(tidyverse)
library(readr)
options(scipen=100000)
basepath <- "K:\\Metode\\Challenge\\7_FKC\\age_cohort\\"




load_in <- function(base_path,filename_before_i,i_seq, filename_after_i){
  
  for (i in i_seq){ 
    if(i==1){out <- read_delim(paste0(base_path, filename_before_i,i,filename_after_i), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)
    }else{out <- rbind(out, read_delim(paste0(base_path, filename_before_i,i,filename_after_i), 
                               delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE))}}
    return(out)
}


diags_all <- load_in(base_path = basepath, filename_before_i = "booted_all_i", i_seq = 1:2,filename_after_i="_diags.csv")
diags_edu <- load_in(base_path = basepath, filename_before_i = "booted_edu_i", i_seq = 1:2,filename_after_i="_diags.csv")
diags_inc <- load_in(base_path = basepath, filename_before_i = "booted_inc_i", i_seq = 1:2,filename_after_i="_diags.csv")

diags_original_estimates <- read_delim(paste0(basepath,"original_estimates_diags_all.csv"), 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

CD_all <- load_in(base_path = basepath, filename_before_i = "booted_all_i", i_seq = 1:3,filename_after_i="_diags_CDonly.csv")
CD_edu <- load_in(base_path = basepath, filename_before_i = "booted_edu_i", i_seq = 1:3,filename_after_i="_diags_CDonly.csv")
CD_inc <- load_in(base_path = basepath, filename_before_i = "booted_inc_i", i_seq = 1:3,filename_after_i="_diags_CDonly.csv")

CD_original_estimates <- read_delim(paste0(basepath,"original_estimates_diags_CDonly.csv"), 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

NCD_all <- load_in(base_path = basepath, filename_before_i = "booted_all_i", i_seq = 1:3,filename_after_i="_diags_NCDonly.csv")
NCD_edu <- load_in(base_path = basepath, filename_before_i = "booted_edu_i", i_seq = 1:3,filename_after_i="_diags_NCDonly.csv")
NCD_inc <- load_in(base_path = basepath, filename_before_i = "booted_inc_i", i_seq = 1:3,filename_after_i="_diags_NCDonly.csv")

NCD_original_estimates <- read_delim(paste0(basepath,"original_estimates_diags_NCDonly.csv"), 
                     delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE)

cleanup <- function(tibble){
  # Ensure tibble has enough rows
  if (nrow(tibble) < 23) {
    stop("Tibble does not have enough rows to perform the operation.")
  }
  
  tibble <- tibble %>% 
    mutate(estimator2 = case_when(
      estimator == "log_RR_RR1v4" ~ "log_RR_RR1vs4",
      TRUE ~ estimator
    )) %>% 
    mutate(ID = row_number()) %>% 
    # Create lagged columns for the required offsets
    mutate(value_plus_19 = lead(value, 19),
           value_plus_20 = lead(value, 20),
           value_plus_22 = lead(value, 22),
           value_plus_23 = lead(value, 23)) %>%
    # Calculate new value column
    mutate(value = case_when(
      ID %% 36 == 10 ~ log(value_plus_19 / value_plus_22),
      ID %% 36 == 13 ~ log(value_plus_20 / value_plus_23),
      TRUE ~ value
    )) %>%
    select(-c(ID, value_plus_19, value_plus_20, value_plus_22, value_plus_23))
  
  return(tibble)
}

diags_original_estimates <- cleanup(diags_original_estimates)
write_csv2(diags_original_estimates, paste0(basepath,"original_estimates_diags_all.csv"))

diags_all <- cleanup(diags_all)
write_csv2(diags_all, paste0(basepath,"diags_all_agg.csv"))

diags_edu <- cleanup(diags_edu)
write_csv2(diags_edu, paste0(basepath,"diags_edu_agg.csv"))

diags_inc <- cleanup(diags_inc)
write_csv2(diags_inc, paste0(basepath,"diags_inc_agg.csv"))


CD_all <- cleanup(CD_all)
CD_edu <- cleanup(CD_edu)
CD_inc <- cleanup(CD_inc)
CD_original_estimates < cleanup(CD_original_estimates)


write_csv2(CD_all, paste0(basepath,"CD_all_agg.csv"))
write_csv2(CD_edu, paste0(basepath,"CD_edu_agg.csv"))
write_csv2(CD_inc, paste0(basepath,"CD_inc_agg.csv"))
write_csv2(CD_original_estimates, paste0(basepath,"CD_original_estimates_agg.csv"))


NCD_all <- cleanup(NCD_all)
NCD_edu <- cleanup(NCD_edu)
NCD_inc <- cleanup(NCD_inc)
NCD_original_estimates < cleanup(NCD_original_estimates)


write_csv2(NCD_all, paste0(basepath,"NCD_all_agg.csv"))
write_csv2(NCD_edu, paste0(basepath,"NCD_edu_agg.csv"))
write_csv2(NCD_inc, paste0(basepath,"NCD_inc_agg.csv"))
write_csv2(NCD_original_estimates, paste0(basepath,"NCD_original_estimates_agg.csv"))

