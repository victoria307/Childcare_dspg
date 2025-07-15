rm(list=ls())
cat("\014")


#Packages-------
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
library(fixest)
library(car)

VAdata <- read.csv("Data/VAdataparticipation.csv") %>% 
  rename(fips = COUNTY_FIPS_CODE,
         year = Year)  
  

VAdata$Year <- as.integer(as.character(VAdata$year))
VDOEdata <- read.csv("Data/VDOE/VDOEpolicydummy.csv")

merged <- VDOEdata %>% 
  left_join(VAdata, by = c("fips", "year")) %>% 
  mutate(Total_Count = as.numeric(Total_Count)) %>% 
  filter(!is.na(Total_Count))

VAdata22 <- VAdata %>%
  filter(Year == 2022)

VAdataTS <- VAdata %>%
  filter(Year >= 2014 & Year <= 2019)

check <- merged %>% 
  filter(code == 3)
check2 <- VAdata %>% 
  filter(fips == 51003)

write.csv(merged, "Data/VDOE/WB+VDOE.csv")


#Modeling
model <- lm(Expenditures ~
               MR70per + EL85LFW + NoWaitListPeriod + NoCopayPeriod+
               MR70per:MCINFANT + EL85LFW:MCINFANT + NoWaitListPeriod:MCINFANT +
               NoCopayPeriod:MCINFANT + #median family income
               H_UNDER6_SINGLEM + #number of households under 6 with single mothers
               MCINFANT + MCTODDLER + MCPRESCHOOL + MFCCINFANT + MFCCTODDLER +
               MFCCPRESCHOOL + Urban + ONERACE_W + ONERACE_B +
               ONERACE_I + ONERACE_A + ONERACE_H +
               HISPANIC,
             data = merged)
summary(model)

alias(model)  

model2 <- lm(formula = Total_Count ~ MR70per + EL85LFW + NoWaitListPeriod + 
     NoCopayPeriod + MFI + H_UNDER6_SINGLEM + Urban + ONERACE_W + 
     ONERACE_B + ONERACE_I + ONERACE_A + ONERACE_H + ONERACE_OTHER + 
     TWORACES + HISPANIC, data = merged)
summary(model2)



summary(is.na(merged$Total_Count))
summary(is.nan(merged$Total_Count))
summary(is.infinite(merged$Total_Count))

str(merged$Total_Count)





















