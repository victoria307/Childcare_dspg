#Clearing the workspace
cat("\014")
rm(list = ls())


#Packages needed (install.packages if these lines do not work)
library("data.table")
library("bit64")
library("dplyr")
library("tidyr")
library("forcats")
library(stringr)
library(httr)
library(jsonlite)
library(tidycensus)

setwd("C:/Users/joshu/Desktop/DSPG/Data")
extracted_data <- unzip("pu2020_csv.zip", "pu2020.csv")


#Getting all useful variables
pu <- fread(extracted_data, sep = "|", select = c(
  "SSUID","SHHADID","PNUM","MONTHCODE","TDEPNDNTEXP","SPANEL","WPFINWGT", "EEDUC","TAGE","ESEX", "ERACE", "EMS", "RFAMKIND", "TEHC_METRO", "TEHC_REGION","EOTHR", "THTOTINC",
  "EPAR", "EPAYHELP","ETIMELOST", "ETIMELOST_TP", "EWHOPAID1", "ELIST", "TWKHRS1", "TWKHRS2", "TWKHRS3", "TWKHRS4",
  "TWKHRS5", "TMWKHRS", "EJB1_PVWKTR9", 'TCBYR_1', 'TCBYR_2', 'TCBYR_3', 'TCBYR_4', 'TCBYR_5', 'TCBYR_6', "EJB1_SCRNR"
  
))
data <- pu

#Filtering for working age adults 
data <- data %>% 
  filter(TAGE >= 18) %>% 
  filter(TAGE <= 67) 



## Outcome Variables ----

#Harmonizing Hours lost into standard unit (hours)
#ETIMELOST_TP
#1: Hours 
#2: Days 
#3: Weeks
data <- data %>%
  mutate(TimeLost = case_when(
    ETIMELOST_TP == 1 ~ ETIMELOST * 1,
    ETIMELOST_TP == 2 ~ ETIMELOST * 24,
    ETIMELOST_TP == 3 ~ ETIMELOST * 24 * 7,
    TRUE ~ 0
  ))
summary(data$TimeLost)
sum(data$TimeLost>0)
#Issue: 1740 out of 28000+ people lost hours

#Employment Indicator
data <- data %>% 
  mutate(EMP = if_else(EJB1_SCRNR == 1,1,0))

#Hours Worked
data <- data %>% 
  mutate(HRSWorked = TMWKHRS) %>% 
  mutate(HRSWorked = replace_na(HRSWorked,0)) 


## Child Variables -------------------------------------------

#Creating Indicator Variables for kids under the age of 13
data <- data %>% 
  mutate(
    Kid1 = case_when(SPANEL - TCBYR_1 <= 13 & SPANEL - TCBYR_1 >= 0 ~ 1, TRUE ~ 0),
    Kid2 = case_when(SPANEL - TCBYR_2 <= 13 & SPANEL - TCBYR_2 >= 0 ~ 1, TRUE ~ 0),
    Kid3 = case_when(SPANEL - TCBYR_3 <= 13 & SPANEL - TCBYR_3 >= 0 ~ 1, TRUE ~ 0),
    Kid4 = case_when(SPANEL - TCBYR_4 <= 13 & SPANEL - TCBYR_4 >= 0 ~ 1, TRUE ~ 0),
    Kid5 = case_when(SPANEL - TCBYR_5 <= 13 & SPANEL - TCBYR_5 >= 0 ~ 1, TRUE ~ 0),
    Kid6 = case_when(SPANEL - TCBYR_6 <= 13 & SPANEL - TCBYR_6 >= 0 ~ 1, TRUE ~ 0)
  )

#Creating Age of Child Buckets
# 1 = infant (0 - 1)
# 2 = toddler (2 -3)
# 3 = Preschool (4-5)
# 4 = School age (6 - 13)
# 5 = Too Old (14+)
# -1 = Child does not exist
data <- data %>%
  mutate(age1 = if_else(is.na(TCBYR_1), -1, SPANEL - TCBYR_1),
         age_group1 = cut(
           age1,
           breaks = c(-Inf, -1, 1, 3, 5, 13, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy"),
           right = TRUE
         ),
         age2 = if_else(is.na(TCBYR_2), -1, SPANEL - TCBYR_2),
         age_group2 = cut(
           age2,
           breaks = c(-Inf, -1, 1, 3, 5, 13, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy"),
           right = TRUE
         ),
         age3 = if_else(is.na(TCBYR_3), -1, SPANEL - TCBYR_3),
         age_group3 = cut(
           age3,
           breaks = c(-Inf, -1, 1, 3, 5, 13, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy"),
           right = TRUE),
         age4 = if_else(is.na(TCBYR_4), -1, SPANEL - TCBYR_4),
         age_group4 = cut(
           age4,
           breaks = c(-Inf, -1, 1, 3, 5, 13, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy"),
           right = TRUE),
         age5 = if_else(is.na(TCBYR_5), -1, SPANEL - TCBYR_5),
         age_group5 = cut(
           age5,
           breaks = c(-Inf, -1, 1, 3, 5, 13, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy"),
           right = TRUE),
         age6 = if_else(is.na(TCBYR_6), -1, SPANEL - TCBYR_6),
         age_group6 = cut(
           age6,
           breaks = c(-Inf, -1, 1, 3, 5, 13, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy"),
           right = TRUE)
  )


#Waiting List (ELISt)
#23k NA's, 21k when filtering for Kid1, so not useful

summary(data$ELIST)
summary(data$ELIST[data$Kid1 == 1])


## Primary Individual Variables ------

#Education Buckets
#Less than HS, HS, undergrad, More than College
data <- data %>%
  mutate(Education = cut(
    EEDUC,
    breaks = c(-Inf, 38, 42, 43, Inf),
    labels = c("No High School Diploma", "High School or Some College",
               "Bachelor’s Degree", "Graduate or Professional Degree"),
    right = TRUE
  ))


#Age of Primary Parent
data <- data %>% 
  mutate(ParentAge = TAGE)

#Race of Parent
data$ERACE <- as.factor(data$ERACE)
data <- data %>% 
  mutate(Race = factor(ERACE,
                       levels = c(1,2,3,4),
                       labels = c("White", "Black", "Asian", "Residual")))
data <- data %>% 
  mutate(ChildCareCost = replace_na(TDEPNDNTEXP,0))

#Income
summary(data$THTOTINC)
sum(is.na(data$THTOTINC))
data <- data %>% 
  mutate(Income = THTOTINC)

#Household ID
data <- data %>% 
  mutate(HouseholdID = SHHADID,
         PersonalNumber = PNUM,
         PID = paste(SHHADID, PNUM, sep = ""),
         Month = MONTHCODE,
         Year = SPANEL,
         TimeID = paste(Month, Year, sep = ""),
         Weight = WPFINWGT) 



## Control Factors----
#Make Metro into a factor
#TEHC_METRO
#1: Metropolitan Area
#2: Nonmetropolitan Area  
data <- data %>% 
  filter(TEHC_METRO != 3) %>% 
  mutate(NonMetro = if_else(TEHC_METRO == 2,1,0))


#Making sex into a factor
data <- data %>% 
  mutate(Female = if_else(data$ESEX == 2,1,0))


#Create EMS into a usable Factor
data <- data %>% 
  mutate(SingleFamily = if_else(EMS != 1,1,0))


#Making EPAYHELP into a factor
data <- data %>% 
  mutate(WelfareorSS = if_else(EWHOPAID1 == 1,1,0)) %>% 
  mutate(WelfareorSS = replace_na(WelfareorSS,0))

#Creating Work from home Indicator
data <- data %>% 
  mutate(WorkFromHome = if_else(EJB1_PVWKTR9 == 1,1,0)) %>% 
  mutate(WorkFromHome = replace_na(WorkFromHome,0))
summary(data$WorkFromHome)


data2 <- data %>% 
  select(38:ncol(data)) %>% 
  filter(Kid1 == 1)

HRSWorkedData <- data2 %>% 
  filter(EMP == 1)

TimeLost1 <- lm(
  TimeLost ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCareCost +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6,
  data2, weights = Weight
)
summary(TimeLost1)

EMP1 <- lm(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCareCost +
             Education + ParentAge + WorkFromHome + Race + Income +
             Kid2 + Kid3 + Kid4 + Kid5 + Kid6,
           data2, weights = Weight
)
summary(EMP1)


EMP1 <- feols(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCareCost +
    NonMetro*ChildCareCost + Female*ChildCareCost+
    Education + ParentAge + WorkFromHome + Race +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6|PersonalNumber + TimeID,
  data2, weights = ~Weight
)
summary(EMP1)
head(data2)







HRSWorked1 <- lm(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCareCost +
    Education + ParentAge + WorkFromHome + Race + Income +
    Kid2 + Kid3 + Kid4 + Kid5 + Kid6,
  HRSWorkedData, weights = Weight
)
summary(EMP1)

