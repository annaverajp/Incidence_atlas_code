library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
library(Epi)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

library(flextable)
library(table1)
library(officer)
library(rvg)

# Data needed
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")

Population <- dbReadTable(conn = conn_inci, name = "Population_2013_2022")


# Getting all demographic info ready for table 1
Population <- Population %>% 
   mutate(AGE_CAT = floor(AGE),
          AGE_CAT = cut(AGE_CAT, breaks = c(-Inf, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                        labels = c("30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                                   "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90+ years")),
          UDD = recode(UDD,
                       "1" = "Low education",
                       "2" = "Medium education",
                       "3" = "High education")) %>% 
   mutate(KOEN = ifelse(KOEN == 1, "Male", "Female"))


#### Table 1 for education

data <- Population %>% filter(UDD %in% c("Low education", "Medium education", "High education"))


# bruger table1 til at lave en tabel, smider i en flextable
label(data$BIRTH_COHORT) <- "Birth cohort"
label(data$UDD) <- "Education"
label(data$AGE_CAT) <- "Age"
label(data$KOEN) <- "Sex"
label(data$INCOMEQ) <- "Indkomst"


tab1 <- table1( ~KOEN + AGE_CAT + BIRTH_COHORT + INCOMEQ| UDD , data=data ) %>% t1flex()


# Smider ud i et worddokument
read_docx() %>% # Her bør man kunne hente et template ind
   body_add_flextable( tab1 %>% 
                          bold( part="header") %>% 
                          set_caption("Tabel 1. Population characteristics") %>% 
                          font(fontname = "Times New Roman") %>% fontsize( size=10) %>% 
                          padding(padding = 0, part = "all") %>% 
                          set_table_properties(layout = "autofit")) %>% 
   body_add_break() %>%  
   print(target = "Table1_study1_revised.docx")



#### Table 1 for income

data <- Population %>% filter(INCOMEQ %in% c("Q1", "Q2", "Q3", "Q4"))


# bruger table1 til at lave en tabel, smider i en flextable
label(data$BIRTH_COHORT) <- "Birth cohort"
label(data$UDD) <- "Education"
label(data$AGE_CAT) <- "Age"
label(data$KOEN) <- "Sex"
label(data$INCOMEQ) <- "Indkomst"


tab1 <- table1( ~ AGE_CAT + BIRTH_COHORT + UDD| INCOMEQ + KOEN, data=data ) %>% t1flex()


# Smider ud i et worddokument
read_docx() %>% # Her bør man kunne hente et template ind
  body_add_flextable( tab1 %>% 
                        bold( part="header") %>% 
                        set_caption("Tabel 1. Population characteristics") %>% 
                        font(fontname = "Times New Roman") %>% fontsize( size=10) %>% 
                        padding(padding = 0, part = "all") %>% 
                        set_table_properties(layout = "autofit")) %>% 
  body_add_break() %>%  
  print(target = "Table1_study1_revised_income.docx")



