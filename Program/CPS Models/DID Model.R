## File Setup ----
# Cleaning Enviorment ----
rm(list=ls())
cat("\014")

# Getting Necessary Packages ----
library("data.table")
library("bit64")
library("dplyr")
library("tidyr")
library("forcats")
library(stringr)
library(httr)
library(R.utils)
library(fixest)
library(caret)
library(fastDummies)
library(viridis)
library(modelsummary)

# Loading in data ----
#gunzip("Data/cps_00006.csv.gz")

data <- read.csv("Data/cps_00006.csv")

#Setting up data
data$CPSID <- as.character(data$CPSID)
data$CPSIDP <- as.character(data$CPSIDP)

data <- data %>% 
  dplyr::select(-c(ASECFLAG, ASECWTH, HWTFINL, WTFINL, ASECWT, UHRSWORK1))

# Data Cleaning ----
data <- data %>% 
  filter(NCHLT5 > 0) %>% 
  mutate(
    Education = case_when(
      EDUC <= 72 ~ "No HS",
      EDUC == 73 ~ "HS",
      EDUC > 73 & EDUC <= 110 ~ "Some College",
      EDUC == 111 ~ "Bachelors",
      EDUC > 111 ~ "Graduate/Professional Degree",
      TRUE ~ NA
    ),
    
    EMP = case_when(
      EMPSTAT == 10 | EMPSTAT == 12 ~ 1,
      TRUE ~ 0
    ),
    
    EMPSPOUSE = case_when(
      EMPSTAT_SP == 10 | EMPSTAT_SP == 12 ~ 1,
      TRUE ~ 0
    ),
    
    Married = if_else(MARST == 1,1,0),
    
    Female = if_else(SEX ==2,1,0),
    
    Post = if_else(YEAR > 2022 | (YEAR == 2022 & MONTH >= 7),1,0),
    
    Subsidy = case_when(
      FAMSIZE == 2 & FAMINC >= 730 & FAMINC <= 820 ~ "Newly_Eligible",
      FAMSIZE == 3 & FAMINC >= 740 & FAMINC <= 830 ~ "Newly_Eligible",
      FAMSIZE == 4 & FAMINC >= 820 & FAMINC <= 841 ~ "Newly_Eligible",
      FAMSIZE == 5 & FAMINC >= 830 & FAMINC <= 841 ~ "Newly_Eligible",
      
      FAMSIZE == 2 & FAMINC < 730 ~ "Always_Eligible",
      FAMSIZE == 3 & FAMINC < 740  ~"Always_Eligible",
      FAMSIZE == 4 & FAMINC < 820 ~ "Always_Eligible",
      FAMSIZE == 5 & FAMINC < 830 ~ "Always_Eligible",
      
      FAMSIZE == 2 & FAMINC > 820 ~ "Never_Eligible",
      FAMSIZE == 3 & FAMINC > 830 ~ "Never_Eligible",
      FAMSIZE == 4 & FAMINC > 841 ~ "Never_Eligible",
      FAMSIZE == 5 & FAMINC > 841 ~ "Never_Eligible",
      TRUE ~ NA
      
    ),
    Race = case_when(
      RACE == 100 ~ "White",
      RACE == 200 ~ "Black",
      RACE == 300 ~ "American Indian",
      RACE == 600 | RACE == 651  | RACE == 652 ~ "Asian or Pacific Islander",
      RACE == 999 ~ NA,
      RACE >= 800 ~ "2+ Race",
      TRUE ~ NA
    ),
    
    Rural = if_else(METRO == 3,1,0)
    
    
  )

#Making sure we remove all NA observation bc it ruins ML
data <- data %>%
  filter(if_all(everything(), ~ !is.na(.)),
         !is.na(RACE)) %>% 
  mutate(across(where(is.character), as.factor))


# Get rid of superfulous variables
data2 <- data %>% 
  dplyr::select(-c(SERIAL, CPSID, STATEFIP,
                   METRO, PERNUM, CPSIDP, CPSIDV, SEX, RACE,
                   MARST, EMPSTAT, EDUC, FAMINC))


#Add the necessary Dummies (ML does not like factors)
dummies <- dummyVars(" ~ .", data = data2, fullRank = TRUE)

#Load in the ML Model
fit.rf <- readRDS("Program/CPS Models/fit_rf_model.rds")


#Creating Final Dataset (With ML Predictions)----
finalData <- data
x_full <- predict(dummies, newdata = finalData) %>% as.data.frame()

predicted_class <- predict(fit.rf, newdata = x_full)

finalData$predicted_subsidy <- predicted_class


#Modeling ----



summary(finalData$predicted_subsidy)


EMP3 <- feols(EMP ~ predicted_subsidy + Post + predicted_subsidy:Post
              + AGE + NCHLT5 + Married + Female + Race 
                + Rural + Education|CPSID + YEAR + MONTH, data = finalData)
summary(EMP3)


modelsummary(EMP3, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")


summary(EMP3)
wald(EMP3)


finalData <- finalData %>%
  mutate(EventTime = 12 * (YEAR - 2022) + (MONTH - 6))  # if policy in July 2022

TestData <- finalData %>% 
  filter(predicted_subsidy %in% c("Newly_Eligible", "Always_Eligible"))
         

EMP_event <- feols(EMP ~ i(EventTime, predicted_subsidy == "Newly_Eligible", ref = -1) +
                     AGE + NCHLT5 + Married + Female + Race + Rural + Education |
                     CPSID + YEAR + MONTH, data = TestData)

iplot(EMP_event,
      main = "Event Study: Policy Impact on Employment",
      xlab = "Months Relative to Policy",
      ref.line = 0)


