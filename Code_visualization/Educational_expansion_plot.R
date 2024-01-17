####### ----------- Plotting distribution of age across cohorts --------- ########
library(dplyr)
library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(viridis)

barplot <- Population %>% 
  group_by(BIRTH_COHORT, UDD) %>% 
  summarise(n = n())

ggplot(barplot, aes(fill = as.factor(UDD), y = n, x = BIRTH_COHORT)) +
  geom_bar(position = "fill", stat = "identity", alpha = .6, color = "black") +
  scale_fill_manual(values =   c("#00bfc4", "#8494ff", "#ff68a1"),
                    name = "Level of education", 
                    labels = c("Low", "Medium", "High")) +
  xlab("Birth cohorts") +
  ylab("Percentage") +
  theme_bw()+
  theme(text = element_text(size = 12, family = "Times New Roman"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Approved
c("#00bfc4", "#8494ff", "#ff68a1")
c("#cd9600", "#e68613", "#f8766d")
c("#8494ff", "#00bfc4", "#00c19a")
c("#8494ff", "#00bfc4", "#00be67")
c("#f8766d", "#00c19a", "#8494ff")



barplot_income <- Population %>% 
  group_by(BIRTH_COHORT, INCOMEQ) %>% 
  filter(!is.na(INCOMEQ)) %>% 
  summarise(n = n())

ggplot(barplot_income, aes(fill = as.factor(INCOMEQ), y = n, x = BIRTH_COHORT)) +
  geom_bar(position = "fill", stat = "identity", alpha = .6, color = "black") +
  scale_fill_manual(values =   c("#00bfc4", "#8494ff", "#ff68a1", "#00c19a"),
                    name = "Level of education", 
                    labels = c("Q1", "Q2", "Q3", "Q4")) +
  xlab("Birth cohorts") +
  ylab("Percentage") +
  theme_bw()+
  theme(text = element_text(size = 12, family = "Times New Roman"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
