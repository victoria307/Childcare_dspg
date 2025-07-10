#Setting up File
rm(list = ls())
cat("\014")

#Getting Packages
library(dplyr)
library(fixest)
library(modelsummary)

#Getting Data
data <- read.csv("../Childcare Project/Data/SIPP Output/20-23VASIPPData.csv")

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
    hh_size == 2 & Income >= 3430 & Income <= 5041 ~ 1,
    hh_size == 3 & Income >= 4330 & Income <=6226 ~ 1,
    hh_size == 4 & Income >= 5230 & Income <=7412 ~ 1,
    hh_size == 5 & Income >= 6130 & Income <=8599 ~ 1,
    TRUE ~ 0
  ))

#Making that if eligible for one period, then eligible for whole year
data <- data %>%
  group_by(HouseholdID, Year) %>%
  mutate(Subsidy = as.integer(any(Subsidy == 1))) %>%
  ungroup()

#Indicator if after treatment
data <- data %>% 
  mutate(PostTreat = if_else(Year > 2022 | (Year == 2022 & Month >= 7),1,0))

# Issue, only 8% of data is treated
sum(data$PostTreat ==1 & data$Subsidy == 1)/nrow(data)

#Make White the reference group
data$Race <- relevel(factor(data$Race), ref = "White")



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



EMP1 <- lm(EMP ~ Subsidy + PostTreat + Subsidy:PostTreat+
                  NonMetro + Female + SingleFamily + 
                  Education + ParentAge + factor(Race), data = data,
                weights = Weight)
summary(EMP1)

Time1 <- lm(TimeLost ~ Subsidy + PostTreat + Subsidy:PostTreat+
              NonMetro + Female + SingleFamily + 
              Education + ParentAge + factor(Race), data = data,
            weights = Weight)
summary(Time1)

HRS1 <- lm(HRSWorked ~ Subsidy + PostTreat + Subsidy:PostTreat+
                      NonMetro + Female + SingleFamily + 
                      Education + ParentAge + factor(Race), data = HRSData,
                    weights = Weight)
summary(HRS1)





