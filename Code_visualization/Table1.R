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

Population_2012_2021 <- dbReadTable(conn = conn_inci, name = "Population_2012_2021")


# Getting all demographic info ready for table 1
Population_2012_2021 <- Population_2012_2021 %>% 
   mutate(BIRTHDAY = as.Date(ymd_hms(BIRTHDAY)),
          STUDY_ENTRANCE = as.Date(ymd_hms(STUDY_ENTRANCE)),
          STUDY_EXIT = as.Date(ymd_hms(STUDY_EXIT)))%>% 
   mutate(AGE_CAT = floor(AGE),
          AGE_CAT = ifelse(AGE_CAT == 29, 30, AGE_CAT),
          AGE_CAT = cut(AGE_CAT, breaks = c(-Inf, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, 89, Inf),
                        labels = c("30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years",
                                   "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90+ years")),
          cohort = cut(floor(cal.yr(BIRTHDAY)), breaks = c(-Inf, 1929, 1939, 1949, 1959, 1969, 1979, Inf),
                                  labels = c("1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1990")),
          udd = recode(udd,
                       "1" = "Low education",
                       "2" = "Medium education",
                       "3" = "High education")) %>% 
   mutate(KOEN = ifelse(KOEN == 1, "Male", "Female"),
          death = ifelse(EVENT_END == "Doed", 1, 0))



data <- Population_2012_2021 %>% filter(udd %in% c("Low education", "Medium education", "High education"))


# bruger table1 til at lave en tabel, smider i en flextable
label(data$cohort) <- "Birth cohort"
label(data$udd) <- "Education"
label(data$AGE_CAT) <- "Age"
label(data$KOEN) <- "Sex"
label(data$death) <- "Deaths"


tab1 <- table1( ~KOEN + AGE_CAT + cohort + death| udd , data=data ) %>% t1flex()


# Smider ud i et worddokument
read_docx() %>% # Her bør man kunne hente et template ind
   body_add_flextable( tab1 %>% 
                          bold( part="header") %>% 
                          set_caption("Tabel 1. Population characteristics") %>% 
                          font(fontname = "Times New Roman") %>% fontsize( size=10) %>% 
                          padding(padding = 0, part = "all") %>% 
                          set_table_properties(layout = "autofit")) %>% 
   body_add_break() %>%  
   print(target = "Table1_study1.docx")


