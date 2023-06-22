library(tidyverse)
names_raw <- read.csv('K:/Metode/Challenge/Familial aggregation/Data/Raw/SKS 2013 navne diagnoser.txt', header = FALSE)
# Downloaded from and documentation here: 
#https://sundhedsdatastyrelsen.dk/da/rammer-og-retningslinjer/om-klassifikationer/sks-klassifikationer/download-sks

names <- names_raw %>% mutate(
  type = str_sub(V1, 1, 3)
) %>% filter(type == 'dia') %>% 
  mutate(
    diag = str_trim(str_sub(V1, 4, 23)), 
    dates = str_sub(V1, 24, 47), 
    diag_name = str_sub(V1, 48, 167)
  ) %>% select(-V1)

head(names)

# Kapitel info added manually from https://medinfo.dk/sks/brows.php?s_nod=6203
# Appended 9 to end to get all 
kapitel <- tribble(
  ~kap, ~start, ~end,
  1, 'DA00', 'DB999',
  2, 'DC00', 'DD489',
  3, 'DD50', 'DD899',
  4, 'DE00', 'DE909',
  5, 'DF00', 'DF999',
  6, 'DG00', 'DG999',
  7, 'DH00', 'DH599',
  8, 'DH60', 'DH959',
  9, 'DI00', 'DI999',
  10, 'DJ00', 'DJ999',
  11, 'DK00', 'DK939',
  12, 'DL00', 'DL999',
  13, 'DM00', 'DM999',
  14, 'DN00', 'DN999',
  15, 'DO00', 'DO999',
  16, 'DP00', 'DP969',
  17, 'DQ00', 'DQ999',
  18, 'DR00', 'DR999',
  19, 'DS00', 'DT989',
  20, 'DX60', 'DY099',
  21, 'DZ00', 'DZ999'
  ) %>% 
  mutate(kap_roman = as.roman(kap))

# Add kapitel information
names <- names %>% 
  mutate(kap = case_when(
    (diag >= kapitel$start[1]) & (diag <= kapitel$end[1]) ~ 1,
    (diag >= kapitel$start[2]) & (diag <= kapitel$end[2]) ~ 2,
    (diag >= kapitel$start[3]) & (diag <= kapitel$end[3]) ~ 3,
    (diag >= kapitel$start[4]) & (diag <= kapitel$end[4]) ~ 4,
    (diag >= kapitel$start[5]) & (diag <= kapitel$end[5]) ~ 5,
    (diag >= kapitel$start[6]) & (diag <= kapitel$end[6]) ~ 6,
    (diag >= kapitel$start[7]) & (diag <= kapitel$end[7]) ~ 7,
    (diag >= kapitel$start[8]) & (diag <= kapitel$end[8]) ~ 8,
    (diag >= kapitel$start[9]) & (diag <= kapitel$end[9]) ~ 9,
    (diag >= kapitel$start[10]) & (diag <= kapitel$end[10]) ~ 10,
    (diag >= kapitel$start[11]) & (diag <= kapitel$end[11]) ~ 11,
    (diag >= kapitel$start[12]) & (diag <= kapitel$end[12]) ~ 12,
    (diag >= kapitel$start[13]) & (diag <= kapitel$end[13]) ~ 13,
    (diag >= kapitel$start[14]) & (diag <= kapitel$end[14]) ~ 14,
    (diag >= kapitel$start[15]) & (diag <= kapitel$end[15]) ~ 15,
    (diag >= kapitel$start[16]) & (diag <= kapitel$end[16]) ~ 16,
    (diag >= kapitel$start[17]) & (diag <= kapitel$end[17]) ~ 17,
    (diag >= kapitel$start[18]) & (diag <= kapitel$end[18]) ~ 18,
    (diag >= kapitel$start[19]) & (diag <= kapitel$end[19]) ~ 19,
    (diag >= kapitel$start[20]) & (diag <= kapitel$end[20]) ~ 20,
    (diag >= kapitel$start[21]) & (diag <= kapitel$end[21]) ~ 21
  ), 
  kap_roman = as.character(as.roman(kap)))

# Fixing æ,ø,å before uploading
names <- names %>% 
  mutate(diag_name = str_replace(diag_name, 'æ', 'ae'),
         diag_name = str_replace(diag_name, 'å', 'aa'),
         diag_name = str_replace(diag_name, 'ø', 'oe'))

names <- names %>% rename_all(toupper)


