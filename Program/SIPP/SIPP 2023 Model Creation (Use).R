

# File Setup --------------------------------------------------------------


cat("\014")
rm(list = ls())



library("dplyr")
library(fixest)
library(modelsummary)
library(stargazer)
library(AER)
library(jtools)
library(viridis)
library(ggplot2)

# Data Collection + Cleaning ----------------------------------------------

setwd("C:/Users/joshu/Desktop/DSPG/Childcare Project")
data <- read.csv("Data/Sipp Output/20-23SIPPData.csv") %>% 
  mutate(EMP = if_else(EMP == 100,1,0))
data$Race <- relevel(factor(data$Race), ref = "White")



#Making Necessary Datasets

#HRS Worked/Hours Lost model, Only for workers
HRsData <- data %>% 
  filter(HRSWorked > 0)





# Model Creation ----------------------------------------------------------

## Basic Models -------
#Use all the Basic Models
TimeLost1 <- lm(
  TimeLost ~ NonMetro + Female + ChildCare + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + SingleFamily+
    factor(KidsUnder5)  + WelfareorSS:ChildCare,
  HRsData, weights = Weight
)
summary(TimeLost1)
stargazer(TimeLost1, type = "text")


#Adding the Interactions loses significance
EMP1 <- lm(
  EMP ~ NonMetro + Female + ChildCare  + WelfareorSS +
    Education + ParentAge + Race + factor(KidsUnder5) +
    SingleFamily + ChildCare:NonMetro , 
  data, weights = Weight
)
summary(EMP1)

modelsummary(EMP1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")





HRSWorked1 <- lm(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCare +
    Education + ParentAge + WorkFromHome + Race + factor(KidsUnder5) + ChildCare:WelfareorSS,
  HRsData, weights = Weight
)
summary(HRSWorked1)
modelsummary(HRSWorked1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")



## IVs ----

EMPIV <- ivreg(
  EMP ~ NonMetro + Female + ChildCare   +
    Education + ParentAge + Race + factor(KidsUnder5) +
    SingleFamily | . -ChildCare + WelfareorSS, 
  data = data, weights = Weight)
summary(EMPIV, diagnostics = TRUE)


## Fixed Effects Model ----



TimeLostFE <- feols(
  TimeLost ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    factor(KidsUnder5)|HouseholdID + Year + Month + TimeID,
  data, weights = ~Weight
)
summary(TimeLostFE)


EMPFE <- feols(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + Race + ChildCare +
    factor(KidsUnder5)| Year + Month + TimeID,
  data, weights = ~Weight
)
summary(EMPFE)

EMPFEINT <- feols(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + Race +
    factor(KidsUnder5) + WelfareorSS:NonMetro| HouseholdID + Year + Month + TimeID,
  data, weights = ~Weight
)
summary(EMPFEINT)



modelsummary(EMPFE, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")


HRSWorkedFE <- feols(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race +
    factor(KidsUnder5) |Year + Month,
  HRsData, weights = ~Weight
)
summary(HRSWorkedFE)

HRSWorkedFE <- feols(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race +
    factor(KidsUnder5)|Year + Month,
  HRsData, weights = ~Weight
)
summary(HRSWorkedFE)


HRSWorkedFEINT <- feols(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race +
    factor(KidsUnder5) + WelfareorSS:NonMetro + WelfareorSS:Female|Year + Month,
  HRsData, weights = ~Weight
)
summary(HRSWorkedFEINT)


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
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6 + ChildCare + NonMetro:ChildCare +
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






## Creating Figure

EMPBasic <- lm(EMP ~ Female + WelfareorSS + NonMetro + ChildCare+ NonMetro:ChildCare,
               data = data, weights = Weight)
summary(EMPBasic)




EMP1 <- lm(
  EMP ~ NonMetro + Female + ChildCare  + WelfareorSS +
    Education + ParentAge + Race + factor(KidsUnder5) +
    SingleFamily + NonMetro:ChildCare, 
  data, weights = Weight
)
summary(EMP1)

modelsummary(EMP1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")


EMP2 <- lm(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6 + ChildCare + NonMetro:ChildCare +
    age_group1 + Kid2:age_group2 + Kid3:age_group3 + Kid4:age_group4 + Kid5:age_group5 +
    Kid6:age_group6,
  data
)

EMPFE <- feols(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + Race + ChildCare + NonMetro:ChildCare+
    factor(KidsUnder5)| Year + Month ,
  data, weights = ~Weight
)
summary(EMPFE)


EMPIV <- ivreg(
  EMP ~ NonMetro + Female + ChildCare   +
    Education + ParentAge + Race + factor(KidsUnder5) +
    SingleFamily + NonMetro:ChildCare | . -ChildCare + WelfareorSS, 
  data = data, weights = Weight)
summary(EMPIV, diagnostics = TRUE)


#Mako for Website (G) and rocket for Poster
plot_summs(EMPBasic, EMP1, EMP2, EMPIV,
           colors = viridis_pal(option = "G")(6)[c(1, 3, 4, 5)],
           model.names = c("OLS", "OLS with Controls", 
                           "OLS With Child Interactions", "IV"),
           coefs = c("Female", "ChildCare", "WelfareorSS", 
                     "NonMetro", "NonMetro:ChildCare"),
           inner_ci_level = .9) +
  labs(title = "Key Predictors of Employment",
       x = "Coefficient Estimate",
       y = NULL) +
  theme_bw(base_family = "Times New Roman") +
  theme(legend.position = "bottom",
        legend.justification = c(0, 0), 
        legend.box.just = "left",
        text = element_text(size = 14),                    
        axis.text.y = element_text(size = 14, face = "bold"),  
        axis.text.x = element_text(size = 14),              
        plot.title = element_text(size = 18, face = "bold")
  )

