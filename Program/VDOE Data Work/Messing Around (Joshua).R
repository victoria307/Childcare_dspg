rm(list=ls())
cat("\014")

library(readxl)
library(stringr)
library(tidyr)
library(dplyr)
library(fixest)

data <- read_excel("Data/VDOE/CE by locality month_2025-07-03_VA Tech.xlsx")

data <- data %>% 
  separate(Locality, into = c("FIPS", "Locality"), sep = " - ") %>% 
  mutate(FIPS = paste0("51",FIPS),
         Post = case_when(
           year > 2022 ~ 1,
           year == 2022 & month >= 7 ~ 1,
           TRUE ~ 0
         )) %>% 
  rename(TotalCount = `Total Count`,
         TotalExpenditures = `Total Expenditures`) %>% 
  mutate(TotalCount = as.numeric(TotalCount))

model1 <- feols(TotalCount ~ Post|FIPS + year + month,
                data = data)

summary(model1)

model2 <- feols(TotalExpenditures ~ Post + TotalCount|FIPS + year + month,
                          data = data)
summary(model2)

sum(is.na(data$TotalExpenditures))

check <- data %>% 
  group_by(Locality) %>% 
  summarise(NAs = sum(is.na(TotalCount)),
            Total = n()) 

check <- data %>% 
  group_by(Locality) %>% 
  summarise(NAs = sum(is.na(TotalCount)),
            Total = n()) 


summary(check$NAs)