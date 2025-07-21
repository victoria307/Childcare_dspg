#Setting up File
rm(list = ls())
cat("\014")

#Getting Packages
library(dplyr)
library(fixest)
library(modelsummary)
library(lubridate)

#Getting Data
setwd("C:/Users/joshu/Desktop/DSPG/Childcare Project")
data <- read.csv("Data/SIPP Output/20-23VASIPPData.csv")

#Creating Household Size variable
data <- data %>%
  group_by(HouseholdID) %>%
  mutate(hh_size = n_distinct(PersonalNumber) + as.numeric(as.character(first(KidsUnder5)))) %>%
  ungroup()

#Creating Subsidy Variable
#2019 Subsidy Cutoff (page 80)
# https://www.dss.virginia.gov/files/division/cc/state_plans/State_Child_Care_Plan.pdf
# 2022 Cutoff became 85% of SMI  (page 82)
# https://www.childcare.virginia.gov/home/showpublisheddocument/269/638035972496930000

data <- data %>% 
  mutate(Subsidy = case_when(
    hh_size == 2 & Income >= 3430 & Income <= 5041 ~ "Newly_Eligible",
    hh_size == 3 & Income >= 4330 & Income <= 6226 ~ "Newly_Eligible",
    hh_size == 4 & Income >= 5230 & Income <= 7412 ~ "Newly_Eligible",
    hh_size == 5 & Income >= 6130 & Income <= 8599 ~ "Newly_Eligible",
    
    hh_size == 2 & Income < 3430 ~ "Always_Eligible",
    hh_size == 3 & Income < 4330  ~"Always_Eligible",
    hh_size == 4 & Income < 5230 ~ "Always_Eligible",
    hh_size == 5 & Income < 6130 ~ "Always_Eligible",
    
    hh_size == 2 & Income > 5041 ~ "Never_Eligible",
    hh_size == 3 & Income > 6226 ~ "Never_Eligible",
    hh_size == 4 & Income > 7412 ~ "Never_Eligible",
    hh_size == 5 & Income > 8599 ~ "Never_Eligible",
    TRUE ~ NA
  ))

#Making that if eligible for one period, then eligible for whole year
data <- data %>%
  mutate(Subsidy = factor(Subsidy, 
                          levels = c("Newly_Eligible", "Always_Eligible", "Never_Eligible"))) %>%
  group_by(HouseholdID, Year) %>%
  mutate(Subsidy = levels(Subsidy)[min(as.integer(Subsidy), na.rm = TRUE)]) %>%
  ungroup()




#Indicator if after treatment
data <- data %>% 
  mutate(PostTreat = if_else(Year > 2022 | (Year == 2022 & Month >= 7),1,0))


#Make White the reference group
data$Race <- relevel(factor(data$Race), ref = "White")


#Filter high income individuals
#Making the limit 100% SMI or 1.25 SMI not big difference
data <- data %>% 
  group_by(PID) %>% 
  filter(
         !any(
           (hh_size == 2 & Income > 5930)|
           (hh_size == 3 & Income > 7325)|
           (hh_size == 4 & Income > 8721)|
           (hh_size == 5 & Income >  10116)
  )) %>% 
  ungroup()

#  10.5% of data is treated
sum(data$PostTreat == 1 & data$Subsidy == "Newly_Eligible")/nrow(data)




#Creating filtered Datasets ---

#Time Lost Dataset (Will not work)
#Only one household lost work due to childcare
TimeData <- data %>% 
  group_by(HouseholdID) %>% 
  filter(any(TimeLost > 0)) %>% 
  ungroup()

#Hrs Worked Dataset
HRSData <- data %>% 
  group_by(HouseholdID) %>% 
  filter(any(HRSWorked > 0)) %>% 
  ungroup()




#Model


#Cool Model!
EMP1 <- lm(EMP ~ factor(Subsidy) + PostTreat + factor(Subsidy):PostTreat+
                  NonMetro + Female + SingleFamily +
                  Education + ParentAge + factor(Race), data = data,
                weights =   Weight)
summary(EMP1)

modelsummary(EMP1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")




modelsummary(EMP1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")

#TimeLost Model just does not work

#Most of the stuff is not significant
HRS1 <- lm(HRSWorked ~ Subsidy + PostTreat + Subsidy:PostTreat+
             NonMetro + Female + SingleFamily + 
             Education + ParentAge + factor(Race), data = HRSData,
           weights = Weight)
summary(HRS1)

### New IV Model ----

dataIV <- data %>% 
  filter(ChildCare == 1)








