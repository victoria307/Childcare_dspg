rm(list=ls())
cat("\014")


library("data.table")
library("bit64")
library("dplyr")
library("tidyr")
library("forcats")
library(stringr)
library(httr)
library(R.utils)
library(fixest)


data <- read.csv("Data/cps_00003.csv")
Cost <- read.csv("Data/EMPandRural.csv")

Cost <- Cost %>% 
  select(COUNTY_FIPS_CODE, STUDYYEAR, MCINFANT, MCTODDLER, MCPRESCHOOL, MFCCINFANT, 
         MFCCTODDLER, MFCCPRESCHOOL,Urban, POPPCT_URB) %>% 
  filter(STUDYYEAR == 2022)

mean(Cost$MFCCINFANT)

#Key Questions
#Should we filter out ppl not in labor force

data <- data %>% 
  filter(COUNTY != 0,
         EMPSTAT <= 30
         ) %>% 
  mutate(UHRSWORK1 = if_else(UHRSWORK1 == 999,0,UHRSWORK1),
         Education = case_when(
           EDUC <= 72 ~ "No HS",
           EDUC == 73 ~ "HS",
           EDUC > 73 & EDUC <= 110 ~ "Some College",
           EDUC == 111 ~ "Bachelors",
           EDUC > 111 ~ "Graduate/Professional Degree",
           TRUE ~ NA
          ),
         EMP = case_when(
           EMPSTAT == 10 | EMPSTAT == 12 ~ 100,
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
         Female = if_else(SEX ==2,1,0)
         )

combined <- left_join(data, Cost, by = c("COUNTY" = "COUNTY_FIPS_CODE"))

combined <- combined %>% 
  mutate(ChildCostCenter = case_when(
    YNGCH <= 1 ~ MCINFANT *NCHLT5,
    YNGCH <= 3 ~ MCTODDLER *NCHLT5,
    YNGCH <= 5 ~ MCPRESCHOOL *NCHLT5,
    TRUE ~ 0
  ),
  ChildCostFamily = case_when(
    YNGCH <= 1 ~ MFCCINFANT *NCHLT5,
    YNGCH <= 3 ~ MFCCTODDLER *NCHLT5,
    YNGCH <= 5 ~ MFCCPRESCHOOL *NCHLT5,
    TRUE ~ 0
  )
  )


CenterModel <- feols(EMP ~ Female + Post + ChildCostCenter:Post +
                    AGE + factor(NCHLT5) + Education + EMPSPOUSE + Married
                    | COUNTY + YEAR + MONTH + SERIAL, 
                    data = combined)

summary(CenterModel)

FamilyModel <- feols(EMP ~ Female + Post + ChildCostFamily:Post +
                       AGE + factor(NCHLT5) + Education + EMPSPOUSE + Married
                     | COUNTY + YEAR + MONTH + SERIAL, 
                     data = combined)
summary(FamilyModel)


XiaoYiModel <-  feols(EMP ~ Female + Post + MCINFANT:Post +
                        AGE + factor(NCHLT5) + Education + EMPSPOUSE + Married
                      | COUNTY + YEAR + MONTH + SERIAL, 
                      data = combined)
summary(XiaoYiModel)
modelsummary(XiaoYiModel, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")




