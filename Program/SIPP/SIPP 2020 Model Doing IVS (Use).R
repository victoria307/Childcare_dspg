#Notes
#looking at changes to Age Indicator

#Clearing the workspace
cat("\014")
rm(list = ls())


#Packages needed (install.packages if these lines do not work)
library("dplyr")
library("tidyr")
library(stringr)
library(fixest)
library(modelsummary)
library(car)
library(AER)
library(ggplot2)

#Can't put actual file on git bc too big
#This file is in my own data foler
setwd("C:/Users/joshu/Desktop/DSPG/Data")
extracted_data <- unzip("pu2020_csv.zip", "pu2020.csv")


### Data Cleaning -----------------------------------------------------------


#Getting all useful variables
pu <- fread(extracted_data, sep = "|", select = c(
  "SSUID","SHHADID","PNUM","MONTHCODE","TDEPNDNTEXP","SPANEL","WPFINWGT", "EEDUC","TAGE","ESEX", "ERACE", "EMS", "RFAMKIND", "TEHC_METRO", "TEHC_REGION","EOTHR", "THTOTINC",
  "EPAR", "EPAYHELP","ETIMELOST", "ETIMELOST_TP", "EWHOPAID1", "ELIST", "TWKHRS1", "TWKHRS2", "TWKHRS3", "TWKHRS4",
  "TWKHRS5", "TMWKHRS", "EJB1_PVWKTR9", 'TCBYR_1', 'TCBYR_2', 'TCBYR_3', 'TCBYR_4', 'TCBYR_5', 'TCBYR_6', "EJB1_SCRNR", "TST_INTV"
  
))
data <- pu

#Filtering for working age adults 
data <- data %>% 
  filter(TAGE >= 18) %>% 
  filter(TAGE <= 67) 

## Identification Variables
#Household ID
data <- data %>% 
  mutate(HouseholdID = SSUID,
         PersonalNumber = PNUM,
         PID = paste(SSUID, PNUM, sep = ""),
         Month = MONTHCODE,
         Year = SPANEL,
         TimeID = paste(Month, Year, sep = ""),
         Weight = WPFINWGT,
         State = TST_INTV) 
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

#Creating Indicator Variables for kids under the age of 5
data <- data %>% 
  mutate(
    Kid1 = case_when(SPANEL - TCBYR_1 <= 5 & SPANEL - TCBYR_1 >= 0 ~ 1, TRUE ~ 0),
    Kid2 = case_when(SPANEL - TCBYR_2 <= 5 & SPANEL - TCBYR_2 >= 0 ~ 1, TRUE ~ 0),
    Kid3 = case_when(SPANEL - TCBYR_3 <= 5 & SPANEL - TCBYR_3 >= 0 ~ 1, TRUE ~ 0),
    Kid4 = case_when(SPANEL - TCBYR_4 <= 5 & SPANEL - TCBYR_4 >= 0 ~ 1, TRUE ~ 0),
    Kid5 = case_when(SPANEL - TCBYR_5 <= 5 & SPANEL - TCBYR_5 >= 0 ~ 1, TRUE ~ 0),
    Kid6 = case_when(SPANEL - TCBYR_6 <= 5 & SPANEL - TCBYR_6 >= 0 ~ 1, TRUE ~ 0)
  )
data <- data %>%
  mutate(KidsUnder5 = factor(rowSums(select(., Kid1:Kid5), na.rm = TRUE)))


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

#Child Care Cost (in 100s)
data <- data %>% 
  mutate(ChildCareCostper100 = replace_na(TDEPNDNTEXP,0)/100)

#Income
summary(data$THTOTINC)
sum(is.na(data$THTOTINC))
data <- data %>% 
  mutate(Income = THTOTINC)




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


# Making Final Data -------------------------------------------------------


data2 <- data %>% 
  select(39:ncol(data)) %>% 
  filter(KidsUnder5 != 0)  %>% 
  filter(Weight != 0) %>% 
  mutate(EMP = EMP * 100)

VAData <- data2 %>% 
  filter(State == 51)
write.csv(VAData,"../Childcare Project/Data/17-20VASIPPData.csv")



# VA Dataset --------------------------------------------------------------
# Future Analysis looking at who qualifies for subsidy
#VAData <- data %>% 
  #select(39:ncol(data)) %>% 
  #filter(State == 51)
#write.csv(VAData,"17-20VASIPPData.csv")
#length(unique(VAData$PID))


# Checking Some Stuff (PIDs) but should be fine ---------------------------


unique_people <- data2 %>%
  select(HouseholdID, PersonalNumber, KidsUnder5) %>%
  distinct() %>%
  group_by(HouseholdID) %>%
  summarise(
    num_people = n_distinct(PersonalNumber),
    kids = first(as.numeric(as.character(KidsUnder5))),
    hh_size = num_people + kids
  )
check <- data2 %>% 
  filter(HouseholdID == 41808644918)

Subsidy <- data2 %>%
  group_by(HouseholdID) %>%
  mutate(hh_size = n_distinct(PersonalNumber) + as.numeric(as.character(first(KidsUnder5)))) %>%
  ungroup()



# EMP Model ---------------------------------------------------------------


EMP1 <- lm(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCareCostper100 +
    ChildCareCostper100:NonMetro + ChildCareCostper100:Female+
    Education + ParentAge + WorkFromHome + Race + Income + factor(KidsUnder5),
  data2, weights = Weight
)
summary(EMP1)


EMP1 <- feols(
  EMP ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCareCostper100 +
    NonMetro*ChildCareCostper100 + Female*ChildCareCostper100+
    Education + ParentAge  + Race + factor(KidsUnder5)|HouseholdID + TimeID,
  data2, weights = ~Weight
)
summary(EMP1)

#Showing Denisty Chart of Child Care Costs
data2 %>%
  filter(ChildCareCostper100 > 0) %>%
  mutate(ChildCareCost = ChildCareCostper100 * 100) %>%
  ggplot(aes(x = ChildCareCost)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
  geom_density(color = "red", size = 1) +
  scale_x_continuous(breaks = seq(0, 6000, by = 500),  # adjust max based on your data
                     limits = c(0, 6000))+
  labs(title = "Distribution of Child Care Cost", x = "Child Care Cost", y = "Density")
median(data2$ChildCareCostper100[data2$ChildCareCostper100>0])

check <- data2 %>% 
  filter(ChildCareCostper100>0) %>% 
  select(HouseholdID, PersonalNumber, Month, Year, ChildCareCostper100, KidsUnder5)

modelsummary(EMP1, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")





# IV EMP Model (No FE) ----------------------------------------------------
IVCCCost <- lm(
  ChildCareCostper100 ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + Race + KidsUnder5,
  data2, weights = Weight
)
summary(IVCCCost)
linearHypothesis(IVCCCost, c("WelfareorSS = 0"), test = "F")


IVCOSTxFemale <- lm(
  I(ChildCareCostper100 * Female) ~ NonMetro + SingleFamily + WelfareorSS + 
    WelfareorSS:Female + Education + ParentAge + Race + KidsUnder5,
  data = data2,
  weights = Weight
)
summary(IVCOSTxFemale)
linearHypothesis(IVCOSTxFemale, c("WelfareorSS = 0", "WelfareorSS:Female = 0"), test = "F")


IVCOSTxNonMetro <- lm(
  I(ChildCareCostper100 * NonMetro) ~  WelfareorSS:NonMetro+ Female + SingleFamily + WelfareorSS +
    Education + ParentAge + Race + KidsUnder5,
  data2, weights = Weight
)
summary(IVCOSTxNonMetro)
linearHypothesis(IVCOSTxNonMetro, c("WelfareorSS = 0", "WelfareorSS:NonMetro = 0"), test = "F")


IVData <- data2 %>% 
  mutate(PredictedCost = predict(IVCCCost),
         PredictedFemale = predict(IVCOSTxFemale),
         PredictedNonMetro = predict(IVCOSTxNonMetro))


IVEMP <- lm(
  EMP ~ NonMetro + Female + SingleFamily + PredictedCost  +
    PredictedNonMetro + PredictedFemale +
    Education + ParentAge + Race + KidsUnder5,
  IVData, weights = Weight
)
modelsummary(IVEMP, 
             stars = TRUE,
             statistic = "p.value",
             output = "latex")
summary(IVEMP)


# IV Fixed Effects EMP Model ------------------------------------------------------------
#This Model Is Different from the 2020 SIPP Data Clearning and other files BC
# New Factor variable for number of kids under AGe
# Messing around the age to count a child
# Multiplied Employment by 100 and divided Child Care cost by 100 to better understand coef
# Check F stat for strength of IV, Looking for above 10
# Problem: All the IVS are super weak
FEIVCCCost <- feols(
  ChildCareCost ~ NonMetro + Female + SingleFamily + WelfareorSS +
    Education + ParentAge + Race + KidsUnder5|HouseholdID + TimeID,
  data2, weights = ~Weight
)
summary(IVCCCost)



linearHypothesis(IVCCCost, c("WelfareorSS = 0"), test = "F")

FEIVCOSTxFemale <- feols(
  I(ChildCareCost * Female) ~ NonMetro + SingleFamily + WelfareorSS + 
    WelfareorSS:Female + Education + ParentAge + Race + KidsUnder5 | HouseholdID + TimeID,
  data = data2,
  weights = ~Weight
)
linearHypothesis(IVCOSTxFemale, c("WelfareorSS = 0", "WelfareorSS:Female = 0"), test = "F")



FEIVCOSTxNonMetro <- feols(
  I(ChildCareCost * NonMetro) ~  WelfareorSS:NonMetro+ Female + SingleFamily + WelfareorSS +
    Education + ParentAge + Race + KidsUnder5|PID + TimeID,
  data2, weights = ~Weight
)
linearHypothesis(IVCOSTxNonMetro, c("WelfareorSS = 0", "WelfareorSS:NonMetro = 0"), test = "F")





summary(IVCCCost)
data2$PredictedCost <- predict(FEIVCCCost)/100
data2$PredictedFemale <- predict(FEIVCOSTxFemale)
data2$PredictedNonMetro <- predict(FEIVCOSTxNonMetro)



IVEMP <- feols(
  EMP ~ NonMetro + Female + SingleFamily + PredictedCost  +
    PredictedNonMetro + PredictedFemale +
    Education + ParentAge + Race + KidsUnder5|PID + TimeID,
  data2, weights = ~Weight
)
summary(IVEMP)





# Robustness Check --------------------------------------------------------

iv_model <- ivreg(
  EMP ~ ChildCareCost + ChildCareCost:Female + ChildCareCost:NonMetro +
    NonMetro + Female + SingleFamily + Education + ParentAge + Race + KidsUnder5 |
    NonMetro + Female + SingleFamily + Education + ParentAge + Race + KidsUnder5 +
    WelfareorSS + WelfareorSS:Female + WelfareorSS:NonMetro,
  data = data2,
  weights = Weight
)
summary(iv_model, diagnostics = TRUE)

# Anderson–Rubin robust CI
confint(iv_model, level = 0.95, method = "AR")

# Conditional Likelihood Ratio (CLR) CI — best for weak IVs
confint(iv_model, level = 0.95, method = "CLR")





# Other Models ------------------------------------------------------------


HRSWorkedData <- data2 %>% 
  filter(EMP == 100)

TimeLost1 <- lm(
  TimeLost ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCareCost +
    Education + ParentAge + WorkFromHome + Race + Income +
    KidsUnder5,
  data2, weights = Weight
)
summary(TimeLost1)



HRSWorked1 <- lm(
  HRSWorked ~ NonMetro + Female + SingleFamily + WelfareorSS + ChildCareCost +
    Education + ParentAge + WorkFromHome + Race + Income + KidsUnder5,
  HRSWorkedData, weights = Weight
)
summary(EMP1)

