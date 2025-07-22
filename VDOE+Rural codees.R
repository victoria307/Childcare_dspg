
### Combine Rural and Urban Information (Complete)


cat("\014")
rm(list=ls())

library(readxl)
library(dplyr)
library(fixest)

VDOEdata <- read.csv("shiny_app/Data/WB+VDOE copy.csv")
RuralData <- read_xlsx("Data/Census Data/Census County Classification.xlsx")

unique(VDOEdata$STUDYYEAR)

RuralData <- RuralData %>% 
  filter(STATE_NAME == "Virginia") %>% 
  mutate(COUNTY_FIPS_CODE = as.numeric(paste0(STATE,COUNTY))) %>% 
  select(COUNTY_FIPS_CODE, POPPCT_URB) %>% 
  mutate(Urban = if_else(POPPCT_URB > .5,1,0))

RuralData <- RuralData %>%
  rename(fips = COUNTY_FIPS_CODE)

Merged <- left_join(EMPdata, RuralData, by = "fips")

write.csv(Merged,"Data/VDOE+WB+Rural.csv")
