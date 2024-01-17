library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

##############################################################################
# The following script creates the study populations.                        #
##############################################################################

####### ------------ Data access ------------ ######## 

conn_LL <- OraGenvej(dbname = "STATPROD", dbuser = "X30")
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")
conn_PSD <- OraGenvej("DB_PSD", "X30")

HFAUDD <- DBI::dbGetQuery(conn_PSD, 'SELECT DISTINCT PERSON_ID, HFAUDD, HF_KILDE, HF_VFRA, HF_VTIL, REFERENCETID, VERSION FROM D400600. "PSD_HOEJSTE_FULDFOERT_UDD"')

KOEN <- dbGetQuery(conn_LL, ("select * from D221911.LIFELINES_KOEN"))

LL <- dbGetQuery(conn_LL, ("select * from D221911.LIFELINES"))
head(LL)


####### ------------ Defining study population ------------ ######## 

Population <- LL %>% 
  filter(EVENT_START_DATE <= as.Date("2022-12-31"),
         EVENT_FINAL_DATE >= as.Date("2013-01-01")) %>%
  mutate(STUDY_ENTRANCE = ifelse(EVENT_START_DATE <= as.Date("2013-01-01"), as.Date("2013-01-01"), as.Date(EVENT_START_DATE)+1),
         STUDY_ENTRANCE = as.Date(STUDY_ENTRANCE, origin = "1970-01-01"),
         STUDY_END = ifelse(EVENT_FINAL_DATE >= as.Date("2022-12-31"), as.Date("2022-12-31"), as.Date(EVENT_FINAL_DATE)+1),
         STUDY_END = as.Date(STUDY_END, origin = "1970-01-01"),
         AGE = (as.numeric(difftime(as.Date(STUDY_ENTRANCE), as.Date(BIRTHDAY, origin = "1970-01-01")))-1)/365.25,
         EVENT_START = ifelse(EVENT_START_DATE < as.Date("2013-01-01"), "Beginning of follow-up", EVENT_CAUSE_START),
         EVENT_START = ifelse(EVENT_START == "Bopael", "Beginning of follow-up", EVENT_CAUSE_START),
         EVENT_END = ifelse(EVENT_CAUSE_FINAL == "I befolkningen", "End of follow-up", EVENT_CAUSE_FINAL)) 


Population_over30 <- Population %>% 
  filter(AGE >= 30) %>% 
  group_by(PERSON_ID) %>% 
  filter(STUDY_ENTRANCE == min(STUDY_ENTRANCE)) 

Population_under30 <- Population %>% 
  filter(AGE < 30) %>% 
  mutate(BIRTHDAY30 = as.Date(BIRTHDAY) + years(30)+1) %>% 
  filter(BIRTHDAY30 <= as.Date("2022-12-31"),
         BIRTHDAY30 >= as.Date("2013-01-01")) %>%
  filter(BIRTHDAY30 <= EVENT_FINAL_DATE & BIRTHDAY30 >= EVENT_START_DATE) %>% 
  group_by(PERSON_ID) %>% 
  filter(STUDY_ENTRANCE == min(STUDY_ENTRANCE)) %>% 
  mutate(STUDY_ENTRANCE = BIRTHDAY30,
         EVENT_START = ifelse(BIRTHDAY30 <= as.Date("2022-12-31") & BIRTHDAY30 >= as.Date("2013-01-01"), "Turn 30 years", EVENT_START)) %>% 
  select(-BIRTHDAY30)


Population <- rbind(Population_over30, Population_under30) %>% 
  filter(STUDY_ENTRANCE == min(STUDY_ENTRANCE)) %>% 
  mutate(AGE = ifelse(AGE < 30, 30, AGE))



####### ------------ Getting sex ------------ ######## 

Population_KOEN <- left_join(Population, KOEN, by = "PERSON_ID")

KOEN2 <- Population_KOEN %>% 
  group_by(PERSON_ID) %>% 
  summarise(n = n()) %>% 
  filter(n >= 2) %>% 
  left_join(KOEN, by = "PERSON_ID") %>% 
  filter(VTIL == max(VTIL)) %>% 
  filter(RTIL == max(RTIL))

KOEN1 <- Population_KOEN %>% 
  group_by(PERSON_ID) %>% 
  summarise(n = n()) %>% 
  filter(n == 1) %>% 
  left_join(KOEN, by = "PERSON_ID")

KOEN_final <- rbind(KOEN1, KOEN2) %>% 
  select(PERSON_ID, KOEN)


# Link to population
Population <- left_join(Population, KOEN_final, by = "PERSON_ID")



####### ------------ Getting covariates from HFAUDD ------------ ######## 


isced_table <- DSTcolectica::NewestClassificationBySerieName("DISCED15_AUDD") |> as_tibble()

Population_edu <- left_join(Population, HFAUDD, by = "PERSON_ID") %>% 
  dplyr::select(PERSON_ID, STUDY_ENTRANCE, HFAUDD, HF_VFRA, HF_VTIL, REFERENCETID, VERSION) %>% 
  group_by(PERSON_ID) %>% 
  filter(STUDY_ENTRANCE >= HF_VFRA) %>% 
  filter(HF_VTIL == max(HF_VTIL)) %>% 
  filter(REFERENCETID == max(REFERENCETID)) %>% 
  filter(VERSION == max(VERSION))

Population_edu <- Population_edu |> 
  left_join( isced_table |> mutate( CODE5 = as.numeric(CODE5))
             , by = c("HFAUDD"="CODE5")) %>% 
  dplyr::select(PERSON_ID, CODE1, TITLE1)


# Link to Population
Population <- left_join(Population, Population_edu, by = "PERSON_ID")


### Checking missing information

Population <- Population %>% 
  mutate(BIRTH_COHORT = cut(floor(cal.yr(BIRTHDAY)), breaks = c(-Inf, 1919, 1929, 1939, 1949, 1959, 1969, 1979, Inf),
                            labels = c("-1920","1920-1929", "1930-1939", "1940-1949", "1950-1959", "1960-1969", "1970-1979", "1980-1992")))

table(Population$BIRTH_COHORT, Population$TITLE1, useNA = "always")


# Delete individuals born before 1920 due to very limited information on education

Population <- Population %>% 
  filter(BIRTH_COHORT != "-1920")

# Delete NAs
Population <- Population %>% 
  filter(!is.na(CODE1))


# Make educational categories

Population <- Population %>% 
  mutate(UDD = CODE1, 
         UDD = recode(UDD, 
                      "10" = 1,
                      "15" = 1,
                      "20" = 2,
                      "30" = 2,
                      "35" = 2,
                      "40" = 3,
                      "50" = 3,
                      "60" = 3,
                      "70" = 3,
                      "80" = 3)) 



##### Select variables

Population <- Population %>% 
  dplyr::select(PERSON_ID, BIRTHDAY, EVENT_START_DATE, EVENT_CAUSE_START, EVENT_FINAL_DATE, EVENT_CAUSE_FINAL, 
         STUDY_ENTRANCE, EVENT_START, STUDY_END, EVENT_END, AGE, BIRTH_COHORT, KOEN, UDD)


##### Check that STUDY_END is after STUDY_ENTRANCE
test <- Population %>% 
  mutate(check = as.Date(STUDY_END) < as.Date(STUDY_ENTRANCE)) ## Three observations show the opposit. They are excluded

Population <- test %>% 
  filter(check == FALSE)


#### Add income to the population
INDKOMST <- DBI::dbGetQuery(conn_PSD, 'SELECT DISTINCT PERSON_ID, AEKVIVADISP_13, REFERENCETID, VERSION FROM D460202."PSD_PERS_INDKOMST_DEF13"')


# Select three most resent REFERENCETID relative to STUDY_ENTRANCE

INCOME2 <- left_join(select(Population, PERSON_ID, STUDY_ENTRANCE), INDKOMST, by = "PERSON_ID") %>% 
  group_by(PERSON_ID, REFERENCETID) %>% 
  filter(VERSION == max(VERSION)) %>% 
  ungroup()  %>% 
  mutate(STUDY_ENT = as.numeric(format(as.Date(STUDY_ENTRANCE)+1, "%Y")),
         REFERENCET = as.numeric(format(as.Date(REFERENCETID), "%Y"))) %>% 
  filter(REFERENCET == STUDY_ENT | REFERENCET == STUDY_ENT-1 | REFERENCET == STUDY_ENT-2) %>% 
  filter(!is.na(AEKVIVADISP_13)) %>% 
  group_by(PERSON_ID) %>% 
  summarise(n = n(),
            mean_income = mean(AEKVIVADISP_13)) %>% 
  filter(mean_income >= 0) %>% 
  mutate(INCOMEQ = cut(mean_income, breaks = quantile(mean_income, probs = c(0, 0.25, 0.5, 0.75, 1)), labels = c("Q1", "Q2", "Q3", "Q4"), include.lowest = TRUE))

INCOME2 <- INCOME2 %>% 
  select(-n)


# add income information to the population
Population <- left_join(Population, INCOME2, by = "PERSON_ID") %>% 
  select(-check)


#Save the data
dbWriteTable(conn_inci, "Population_2013_2022", Population, overwrite = T)


