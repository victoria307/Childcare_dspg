cat("\014")
rm(list=ls())

library(readxl)
library(dplyr)

EMPdata <- read.csv("Data/VA_data.csv")
RuralData <- read_xlsx("Data/Census County Classification.xlsx")
VAChar <- read.csv("Data/VACountyCharacteristics.csv")

unique(EMPdata$STUDYYEAR)

RuralData <- RuralData %>% 
  filter(STATE == 51) %>% 
  mutate(COUNTY_FIPS_CODE = as.numeric(paste0(STATE,COUNTY))) %>% 
  select(COUNTY_FIPS_CODE, POPPCT_URB) %>% 
  mutate(Urban = if_else(POPPCT_URB > .5,1,0))


# Redundant Not Used ------------------------------------------------------


#EMPdata <- EMPdata %>% 
  #select(STATE_NAME, STATE_ABBREVIATION, COUNTY_NAME, COUNTY_FIPS_CODE, STUDYYEAR, 
         #EMR_20to64, FEMR_20to64, UNR_20to64, FUNR_20to64, FLFPR_20to64, FLFPR_20to64_UNDER6,
         #FLFPR_20to64_6to17, FLFPR_20to64_UNDER6_6to17, PR_F, PR_P, MFI, MFI_2022, 
         #FME, TOTALPOP, ONERACE_W, ONERACE_B, ONERACE_I, ONERACE_A, ONERACE_H,
         #HOUSEHOLDS, H_UNDER6_BOTHWORK, H_UNDER6_FWORK, H_UNDER6_MWORK, H_UNDER6_SINGLEM_STATE,
         #MCBto5, MC6to11, MC12to17, MC18to23, MC24to29, MC30to35, MC36to41, MC42to47,
         #MC48to53, MC54toSA, MCSA, MFCCBto5, MFCC6to11, MFCC12to17, MFCC18to23, MFCC24to29,
         #MFCC30to35, MFCC36to41, MFCC42to47, MFCC48to53, MFCC54toSA, MFCCSA, MCINFANT, 
         #MCTODDLER,MCPRESCHOOL, MFCCINFANT, MFCCTODDLER, MFCCPRESCHOOL, H_UNDER6_SINGLEM         )

#EMPdata <- EMPdata %>% 
  #filter(STUDYYEAR >= 2010) %>% 
  #select(STATE_NAME, STATE_ABBREVIATION, COUNTY_NAME, COUNTY_FIPS_CODE, STUDYYEAR, 
         #EMR_20to64, FEMR_20to64, UNR_20to64, FUNR_20to64, FLFPR_20to64, FLFPR_20to64_UNDER6,
         #FLFPR_20to64_6to17, FLFPR_20to64_UNDER6_6to17, PR_F, PR_P, MFI, MFI_2022, 
         #FME, TOTALPOP, ONERACE_W, ONERACE_B, ONERACE_I, ONERACE_A, ONERACE_H,
         #HOUSEHOLDS, H_UNDER6_BOTHWORK, H_UNDER6_FWORK, H_UNDER6_MWORK, H_UNDER6_SINGLEM_STATE,
         #MCINFANT, MCTODDLER,MCPRESCHOOL, MFCCINFANT, MFCCTODDLER, MFCCPRESCHOOL,
         #H_UNDER6_SINGLEM)

# Continue ----------------------------------------------------------------



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

#write.csv(Merged,"Data/EMPandRural.csv")

Vars <- c("State")
data <- Merged %>% 
  select(c(STATE_NAME.x, COUNTY_NAME.x, ))
data <- Merged %>% 
  select()