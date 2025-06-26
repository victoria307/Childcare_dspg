

# File Setup --------------------------------------------------------------


cat("\014")
rm(list = ls())


library("data.table")
library("bit64")
library("dplyr")
library("stargazer")
library(forcats)
library(broom)
library(glmnet)
library(fixest)
library(modelsummary)



# Data Collection + Cleaning ----------------------------------------------


data <- read.csv("Data/SIPPcleaneddata.csv")


#Making Factors
factor_cols <- c("age_group1",
                 "age_group2",
                 "age_group3",
                 "age_group4",
                 "age_group5",
                 "age_group6")
factor_cols2 <- c("age_group2",
                  "age_group3",
                  "age_group4",
                  "age_group5",
                  "age_group6")
data[factor_cols] <- lapply(data[factor_cols], factor)
data <- data %>%
  mutate(across(all_of(factor_cols2), ~ fct_relevel(., "No Child")))

data$Race <- as.factor(data$Race)
data$Race <- factor(data$Race,
                    levels = c("Residual", "White", "Black", "Asian"))

HRsData <- data %>% 
  filter(HRSWorked > 0)



# Model Creation ----------------------------------------------------------

## Basic Models -------

TimeLost1 <- lm(
  TimeLost ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6,
  data, weights = Weight
)
summary(TimeLost1)
modelsummary(TimeLost1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")


EMP1 <- lm(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6,
  data, weights = Weight
)
summary(EMP1)
modelsummary(EMP1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")


HRSWorked1 <- lm(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6,
  HRsData, weights = Weight
)
summary(HRSWorked1)
modelsummary(HRSWorked1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")

## Fixed Effects Model ----

TimeLostFE <- feols(
  TimeLost ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6 |PersonalNumber + TimeID,
  data, weights = ~Weight
)
summary(TimeLostFE)
modelsummary(TimeLostFE, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")

EMPFE <- feols(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6 | PersonalNumber + TimeID,
  data, weights = ~Weight
)
summary(EMPFE)
modelsummary(EMPFE, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")

HRSWorkedFE <- feols(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6 | PersonalNumber + TimeID,
  HRsData, weights = ~Weight
)
summary(HRSWorkedFE)
modelsummary(HRSWorkedFE, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")

## Interaction Models ----

TimeLost2 <- lm(
  TimeLost ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6 +
    age_group1 + Kid2:age_group2 + Kid3:age_group3 + Kid4:age_group4 + Kid5:age_group5 +
    Kid6:age_group6,
  data
)
summary(TimeLost2)


EMP2 <- lm(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6 +
    age_group1 + Kid2:age_group2 + Kid3:age_group3 + Kid4:age_group4 + Kid5:age_group5 +
    Kid6:age_group6,
  data
)
summary(EMP2)


HRSWorked2 <- lm(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6 +
    age_group1 + Kid2:age_group2 + Kid3:age_group3 + Kid4:age_group4 + Kid5:age_group5 +
    Kid6:age_group6,
  data
)
summary(HRSWorked2)


## Hurdle Model ----
### Data Adjustments
data2 <- data %>% 
  mutate(TimeLostFactor = if_else(TimeLost>0,1,0))

### Hurdle Part 1 ----
Hurdle1<- lm(
  TimeLostFactor ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6,
  data2,
  weights = Weight
)
summary(Hurdle1)


### Hurdle Part 2 ----
data3 <- data2 %>% 
  filter(TimeLostFactor == 1)

Hurdle2<- lm(
  TimeLost ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6,
  data3, weights = Weight
)

summary(Hurdle2)

stargazer(Hurdle1,Hurdle2)

# LASSO ----


LASSOData <- data %>% 
  select(c(-1,-2,-3,-4, -11,-13, -15, -17, -19, -21))

EMP <- data$EMP
TimeLost <- data$TimeLost
HoursWorked <- data$HRSWorked
LASSOData <- data.matrix(LASSOData)


## Employment ----

EMPModel <- cv.glmnet(LASSOData, EMP, alpha = 1)




