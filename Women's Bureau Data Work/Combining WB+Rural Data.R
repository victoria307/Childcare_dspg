
### Combine Rural and Urban Information (Complete)


cat("\014")
rm(list=ls())

library(readxl)
library(dplyr)
library(fixest)

EMPdata <- read.csv("Data/VA_data.csv")
RuralData <- read_xlsx("Data/Census County Classification.xlsx")
VAChar <- read.csv("Data/VACountyCharacteristics.csv")

unique(EMPdata$STUDYYEAR)

RuralData <- RuralData %>% 
  filter(STATE_NAME == "Virginia") %>% 
  mutate(COUNTY_FIPS_CODE = as.numeric(paste0(STATE,COUNTY))) %>% 
  select(COUNTY_FIPS_CODE, POPPCT_URB) %>% 
  mutate(Urban = if_else(POPPCT_URB > .5,1,0))

WBNameChange <- EMPdata %>% 
  mutate(State = STATE_NAME,
         County = COUNTY_NAME,
         FIPSCode = COUNTY_FIPS_CODE,
         Year = STUDYYEAR,
         Poverty = PR_F,
         Income = MFI,
         Population = TOTALPOP,
         White = ONERACE_W,
         Black = ONERACE_B,
         AmericanIndian = ONERACE_I,
         Asian = ONERACE_A,
         Hawaiian = ONERACE_H,
         NumberHouseholds = HOUSEHOLDS,
         SingleMother = H_UNDER6_SINGLEM,
         InfantCostC = MCINFANT,
         ToddlerCostC = MCTODDLER,
         PreschoolCostC = MCPRESCHOOL,
         InfantCostF = MFCCINFANT,
         ToddlerCostF = MFCCTODDLER,
         PreschoolCostF = MFCCPRESCHOOL
         ) %>% 
  select(State, County, FIPSCode, Year, EMR_20to64, FEMR_20to64, UNR_20to64, FUNR_20to64, 
         FLFPR_20to64, FLFPR_20to64_UNDER6, FLFPR_20to64_6to17, FLFPR_20to64_UNDER6_6to17,STATE_NAME,  
         COUNTY_NAME,  COUNTY_FIPS_CODE,  STUDYYEAR,  PR_F,  MFI,  TOTALPOP,  ONERACE_W, ONERACE_B,  
         ONERACE_I,  ONERACE_A,  ONERACE_H,  HOUSEHOLDS,  H_UNDER6_SINGLEM,  MCINFANT,  MCTODDLER,  
         MCPRESCHOOL,  MFCCINFANT,  MFCCTODDLER,  MFCCPRESCHOOL) %>% 
  filter(Year >= 2010)



Merged <- left_join(EMPdata, RuralData, by = "COUNTY_FIPS_CODE")



VAChar <- VAChar %>% 
  filter(YEAR != 1) %>% 
  mutate(COUNTY_FIPS_CODE = 51000 + COUNTY,
         STUDYYEAR = case_when(
           YEAR == 2 ~ 2020,
           YEAR == 3 ~ 2021,
           YEAR == 4 ~ 2022,
           YEAR == 5 ~ 2023
         ))


RuralData <- RuralData %>% 
  mutate(COUNTY_FIPS_CODE = as.numeric(paste0(STATE,COUNTY))) %>% 
  filter(STATE == 51)

Merged <- left_join(EMPdata, RuralData, by = "COUNTY_FIPS_CODE")
ModelData <- left_join(WBNameChange, RuralData, by = "COUNTY_FIPS_CODE")
#write.csv(Merged,"Data/EMPandRural.csv")

