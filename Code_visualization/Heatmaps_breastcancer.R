library(DSTora)
library(DBI)
library(ROracle)
library(tidyverse)
library(lubridate)
devtools::load_all('H:/KT22SVN/Challenge/pipeline/dslpiper') 

##############################################################################
# The following script plots age and period specific incidence rates on a    #
# Lexis surface. Incidence rates are estimating using GAM and estimating a   #
# tensor product (te). Age standardisation is applied.                       #
# Comparing observed incidence rates with estimated incidence rates.         #
##############################################################################

## Adgang til Incidence af alt
conn_inci <- OraGenvej(dbname = "STATUDV", dbuser = "[D222107]")


####### ------------ Access the data ------------ ######## 

diags_mainanalysis <- dbReadTable(conn = conn_inci, name = "diags_mainanalysis")

Population <- dbReadTable(conn = conn_inci, name = "Population_2013_2022")


######## ------------ Defining standard population ------------- ###########

# European standard population 2013 (age groups from "30-34 years" to "90+ years")
sp_prop1 <- data.frame(AGE = c(30:99),
                       sp = rep(c(0.01954887, 0.02105264, 0.02105264, 0.02105264, 0.02105264, 0.01954887, 0.01804511, 0.01654135, 0.01503759, 0.01203008, 0.007518796, 0.004511278, 0.002406016, 0.00037594), each = 5)) 

sp_prop2 <- data.frame(AGE = c(100,101,102),
                       sp = 0.00037594)

sp_prop <- rbind(sp_prop1, sp_prop2)


######## ------------ Estimating incidence rates for breast cancer ------------- ###########

breast_cancer <- diags_mainanalysis %>% 
  filter(DIAG == "DC50")


#### Prepare data

Population <- Population %>% 
  mutate(STUDY_ENTRANCE = cal.yr(STUDY_ENTRANCE),
         STUDY_END = cal.yr(STUDY_END),
         BIRTHDAY = cal.yr(BIRTHDAY))

diags_mainanalysis <- diags_mainanalysis %>% 
  mutate(D_INDDTO = cal.yr(as.Date(D_INDDTO)))

EligiblePop <- left_join(Population, breast_cancer, by = "PERSON_ID") %>% 
  mutate(prior = D_INDDTO < STUDY_ENTRANCE,
         prior = ifelse(is.na(prior), FALSE, prior)) %>% 
  filter(prior == FALSE | is.na(prior)) %>% 
  mutate(check = D_INDDTO <= STUDY_END,
         EVENT_END = ifelse(check == FALSE | is.na(check), EVENT_END, "Diag"),
         STUDY_END = ifelse(EVENT_END == "Diag", D_INDDTO, STUDY_END),
         STUDY_END = ifelse(STUDY_ENTRANCE == STUDY_END, STUDY_END + 0.001368925, STUDY_END),
         event = ifelse(EVENT_END == "Diag", 1, 0)) %>% 
  dplyr::select(PERSON_ID, BIRTHDAY, STUDY_ENTRANCE, STUDY_END, EVENT_START, EVENT_END, AGE, KOEN, UDD, BIRTH_COHORT, INCOMEQ, event) 


# Split data by age
library(Epi)
library(popEpi)
library(mgcv)

split <- lexpand(EligiblePop, birth = BIRTHDAY, entry = STUDY_ENTRANCE, exit = STUDY_END,
                 status = event, entry.status = EVENT_START, breaks = list(age = c(29:103), per = c(2012:2023)))

split_udd <- split %>% 
  mutate(event = ifelse(lex.Xst == "1", 1,0),
         AGE = floor(age),
         AGE = ifelse(AGE == 29, 30, AGE),
         PERIOD = floor(per),
         PERIOD = ifelse(PERIOD == 2012, 2013, PERIOD)) %>% 
  group_by(AGE, UDD, KOEN, PERIOD) %>% 
  summarise(lex.dur = sum(lex.dur),
            event = sum(event),
            IR = event/lex.dur) %>% 
  left_join(sp_prop, by = "AGE") %>% 
  mutate(ASIR = (IR * sp)*100000)


### Running tests

testingASIR_OBSERVED <- split_udd %>% 
  group_by(PERIOD) %>% 
  summarise(ASIR_sum = sum(ASIR))

testing_events <- split_test %>% 
  ungroup() %>% 
  dplyr::select(PERIOD, event, AGE, lex.dur) %>% 
  group_by(PERIOD) %>% 
  summarize(n = sum(event),
            py = sum(lex.dur))



##### Run model

### GAM with tensor product
model_u1k1 <- gam(event ~ offset(log(lex.dur)) +te(AGE,PERIOD), data = split_udd[split_udd$UDD == 1 & split_udd$KOEN == 1,], family = "poisson")
model_u1k2 <- gam(event ~ offset(log(lex.dur)) +te(AGE,PERIOD), data = split_udd[split_udd$UDD == 1 & split_udd$KOEN == 2,], family = "poisson")
model_u2k1 <- gam(event ~ offset(log(lex.dur)) +te(AGE,PERIOD), data = split_udd[split_udd$UDD == 2 & split_udd$KOEN == 1,], family = "poisson")
model_u2k2 <- gam(event ~ offset(log(lex.dur)) +te(AGE,PERIOD), data = split_udd[split_udd$UDD == 2 & split_udd$KOEN == 2,], family = "poisson")
model_u3k1 <- gam(event ~ offset(log(lex.dur)) +te(AGE,PERIOD), data = split_udd[split_udd$UDD == 3 & split_udd$KOEN == 1,], family = "poisson")
model_u3k2 <- gam(event ~ offset(log(lex.dur)) +te(AGE,PERIOD), data = split_udd[split_udd$UDD == 3 & split_udd$KOEN == 2,], family = "poisson")


## Plotting for udd = 1 & men

### Predict

df_predict_u1k1 <- split_udd %>% 
  ungroup() %>% 
  dplyr::select(AGE, PERIOD) %>% 
  unique() %>% 
  mutate(lex.dur = 1)

predict_u1k1 <- predict(model_u1k1, df_predict_u1k1, type = "response")

grid_data_u1k1 <- cbind(df_predict_u1k1, predictions = predict_u1k1) %>% 
  left_join(sp_prop, by = "AGE") %>% 
  mutate(ASIR = (predictions * sp)*100000)


### Testing estimated ASIR

testingASIR_GAM <- grid_data_u1k1 %>% 
  group_by(PERIOD) %>% 
  summarise(ASIR_sum = sum(ASIR))


ggplot(data = grid_data_u1k1, aes(x = PERIOD, y = AGE, fill = ASIR))+
  geom_raster(alpha = 0.8)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,0.2))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), expand = c(0,0))+
  scale_y_continuous(breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100), expand = c(0,0),
                     sec.axis = sec_axis(~ (.- 2022)*-1, name = "COHORT", breaks = c(1992, 1987, 1982, 1977, 1972, 1967, 1962, 1957, 1952, 1947, 1942, 1937, 1932, 1927, 1922))) +
  geom_abline(intercept = seq(20, 200, by = 5) - 2022.5, slope = 1, linetype = 2, color = "black", alpha = 0.4)


## Plotting for udd = 2 & men

### Predict

df_predict_u2k1 <- split_udd %>% 
  ungroup() %>% 
  dplyr::select(AGE, PERIOD) %>% 
  unique() %>% 
  mutate(lex.dur = 1)

predict_u2k1 <- predict(model_u2k1, df_predict_u2k1, type = "response")

grid_data_u2k1 <- cbind(df_predict_u2k1, predictions = predict_u2k1) %>% 
  left_join(sp_prop, by = "AGE") %>% 
  mutate(ASIR = (predictions * sp)*100000)


### Testing estimated ASIR

testingASIR_GAM <- grid_data_u2k1 %>% 
  group_by(PERIOD) %>% 
  summarise(ASIR_sum = sum(ASIR))


ggplot(data = grid_data_u2k1, aes(x = PERIOD, y = AGE, fill = ASIR))+
  geom_raster(alpha = 0.8)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,0.2))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), expand = c(0,0))+
  scale_y_continuous(breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100), expand = c(0,0),
                     sec.axis = sec_axis(~ (.- 2022)*-1, name = "COHORT", breaks = c(1992, 1987, 1982, 1977, 1972, 1967, 1962, 1957, 1952, 1947, 1942, 1937, 1932, 1927, 1922))) +
  geom_abline(intercept = seq(20, 200, by = 5) - 2022.5, slope = 1, linetype = 2, color = "black", alpha = 0.4)


## Plotting for udd = 3 & men

### Predict

df_predict_u3k1 <- split_udd %>% 
  ungroup() %>% 
  dplyr::select(AGE, PERIOD) %>% 
  unique() %>% 
  mutate(lex.dur = 1)

predict_u3k1 <- predict(model_u3k1, df_predict_u3k1, type = "response")

grid_data_u3k1 <- cbind(df_predict_u3k1, predictions = predict_u3k1) %>% 
  left_join(sp_prop, by = "AGE") %>% 
  mutate(ASIR = (predictions * sp)*100000)


### Testing estimated ASIR

testingASIR_GAM <- grid_data_u3k1 %>% 
  group_by(PERIOD) %>% 
  summarise(ASIR_sum = sum(ASIR))


ggplot(data = grid_data_u3k1, aes(x = PERIOD, y = AGE, fill = ASIR))+
  geom_raster(alpha = 0.8)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,0.2))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), expand = c(0,0))+
  scale_y_continuous(breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100), expand = c(0,0),
                     sec.axis = sec_axis(~ (.- 2022)*-1, name = "COHORT", breaks = c(1992, 1987, 1982, 1977, 1972, 1967, 1962, 1957, 1952, 1947, 1942, 1937, 1932, 1927, 1922))) +
  geom_abline(intercept = seq(20, 200, by = 5) - 2022.5, slope = 1, linetype = 2, color = "black", alpha = 0.4)



## Plotting for udd = 1 & women

### Predict

df_predict_u1k2 <- split_udd %>% 
  ungroup() %>% 
  dplyr::select(AGE, PERIOD) %>% 
  unique() %>% 
  mutate(lex.dur = 1)

predict_u1k2 <- predict(model_u1k2, df_predict_u1k2, type = "response")

grid_data_u1k2 <- cbind(df_predict_u1k2, predictions = predict_u1k2) %>% 
  left_join(sp_prop, by = "AGE") %>% 
  mutate(ASIR = (predictions * sp)*100000)


### Testing estimated ASIR

testingASIR_GAM <- grid_data_u1k2 %>% 
  group_by(PERIOD) %>% 
  summarise(ASIR_sum = sum(ASIR))


ggplot(data = grid_data_u1k2, aes(x = PERIOD, y = AGE, fill = ASIR))+
  geom_raster(alpha = 0.8)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,8))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), expand = c(0,0))+
  scale_y_continuous(breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100), expand = c(0,0),
                     sec.axis = sec_axis(~ (.- 2022)*-1, name = "COHORT", breaks = c(1992, 1987, 1982, 1977, 1972, 1967, 1962, 1957, 1952, 1947, 1942, 1937, 1932, 1927, 1922))) +
  geom_abline(intercept = seq(20, 200, by = 5) - 2022.5, slope = 1, linetype = 2, color = "black", alpha = 0.4)


## Plotting for udd = 2 & women

### Predict

df_predict_u2k2 <- split_udd %>% 
  ungroup() %>% 
  dplyr::select(AGE, PERIOD) %>% 
  unique() %>% 
  mutate(lex.dur = 1)

predict_u2k2 <- predict(model_u2k2, df_predict_u2k2, type = "response")

grid_data_u2k2 <- cbind(df_predict_u2k2, predictions = predict_u2k2) %>% 
  left_join(sp_prop, by = "AGE") %>% 
  mutate(ASIR = (predictions * sp)*100000)


### Testing estimated ASIR

testingASIR_GAM <- grid_data_u2k2 %>% 
  group_by(PERIOD) %>% 
  summarise(ASIR_sum = sum(ASIR))


ggplot(data = grid_data_u2k2, aes(x = PERIOD, y = AGE, fill = ASIR))+
  geom_raster(alpha = 0.8)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,8))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), expand = c(0,0))+
  scale_y_continuous(breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100), expand = c(0,0),
                     sec.axis = sec_axis(~ (.- 2022)*-1, name = "COHORT", breaks = c(1992, 1987, 1982, 1977, 1972, 1967, 1962, 1957, 1952, 1947, 1942, 1937, 1932, 1927, 1922))) +
  geom_abline(intercept = seq(20, 200, by = 5) - 2022.5, slope = 1, linetype = 2, color = "black", alpha = 0.4)


## Plotting for udd = 3 & women

### Predict

df_predict_u3k2 <- split_udd %>% 
  ungroup() %>% 
  dplyr::select(AGE, PERIOD) %>% 
  unique() %>% 
  mutate(lex.dur = 1)

predict_u3k2 <- predict(model_u3k2, df_predict_u3k2, type = "response")

grid_data_u3k2 <- cbind(df_predict_u3k2, predictions = predict_u3k2) %>% 
  left_join(sp_prop, by = "AGE") %>% 
  mutate(ASIR = (predictions * sp)*100000)


### Testing estimated ASIR

testingASIR_GAM <- grid_data_u3k2 %>% 
  group_by(PERIOD) %>% 
  summarise(ASIR_sum = sum(ASIR))


ggplot(data = grid_data_u3k2, aes(x = PERIOD, y = AGE, fill = ASIR))+
  geom_raster(alpha = 0.8)+
  scale_fill_gradient(low = "blue", high = "red", limits = c(0,8))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), expand = c(0,0))+
  scale_y_continuous(breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100), expand = c(0,0),
                     sec.axis = sec_axis(~ (.- 2022)*-1, name = "COHORT", breaks = c(1992, 1987, 1982, 1977, 1972, 1967, 1962, 1957, 1952, 1947, 1942, 1937, 1932, 1927, 1922))) +
  geom_abline(intercept = seq(20, 200, by = 5) - 2022.5, slope = 1, linetype = 2, color = "black", alpha = 0.4)








#### Plotting observed incidence rate

ggplot(data = split_udd, aes(x = PERIOD, y = AGE, fill = ASIR))+
  geom_raster(alpha = 0.8)+
  scale_fill_gradient(low = "blue", high = "red")+
  facet_grid(UDD ~ KOEN, labeller = labeller(KOEN = c('1' = "Men", '2' = "Women"), 
                                             UDD = c('1' = "Low education", '2' = "Medium education", '3'="High education")))+
  theme_minimal()+
  theme(panel.grid.major.y = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_line(color = "black", size = 0.5, linetype = 2),
        panel.grid.minor.y = element_blank()) +
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), expand = c(0,0))+
  scale_y_continuous(breaks = c(30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100), expand = c(0,0),
                     sec.axis = sec_axis(~ (.- 2022)*-1, name = "COHORT", breaks = c(1992, 1987, 1982, 1977, 1972, 1967, 1962, 1957, 1952, 1947, 1942, 1937, 1932, 1927, 1922))) +
  geom_abline(intercept = seq(20, 200, by = 5) - 2022.5, slope = 1, linetype = 2, color = "black", alpha = 0.4)




