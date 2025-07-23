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
  filter(COUNTY != 0,
         FAMSIZE != 0,
         NCHILD != 0,
  ) %>% 
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
      EMPSTAT == 10 | EMPSTAT_SP == 12 ~ 1,
      TRUE ~ 0
    ),
    Married = if_else(MARST == 1,1,0),
    Post = case_when(
      YEAR > 2022 ~ 1,
      YEAR == 2022 & MONTH >= 7 ~ 1,
      TRUE ~0
    ),
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
    Urban = if_else(METRO == 3,1,0)
    
    
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
                   MARST, EMPSTAT, EDUC, FAMINC,-EMPSTAT_SP))


#Add the necessary Dummies (ML does not like factors)
dummies <- dummyVars(" ~ .", data = data2, fullRank = TRUE)

#Load in the ML Model
fit.rf <- readRDS("fit_rf_model.rds")


#Creating Final Dataset (With ML Predictions)----
finalData <- data
x_full <- predict(dummies, newdata = finalData) %>% as.data.frame()

predicted_class <- predict(fit.rf, newdata = x_full)

finalData$predicted_subsidy <- predicted_class


#Modeling ----

finalData <- finalData %>% filter(NCHLT5 > 0)

summary(finalData$predicted_subsidy)


EMP3 <- feols(EMP ~ predicted_subsidy + Post + predicted_subsidy:Post
                + AGE + NCHLT5 + Married + Female + Race +
                + Urban + Education|CPSID + YEAR + MONTH, data = finalData)
summary(EMP3)








