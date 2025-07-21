# File Overview -----------------------------------------------------------

#This code will 
#1. Upload the file to rstudio
#2. Extract useful variables
#3. Clean the variables to be ready to plug into a regression
#4. Rename Columns for clarity
#5. Download completed File



# File Setup --------------------------------------------------------------

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




#Download the data from this google drive link https://drive.google.com/file/d/1bC0NgqXxP0sTyHe3jC7cAqeDn_LJrOOC/view?usp=drive_link
#If this does not work https://www.census.gov/programs-surveys/sipp/data/datasets/2023-data/2023.html
#Download the Pipe Delimited text data
# Put this data in the Data folder of the git
#Pulling the Data
setwd("C:/Users/joshu/Desktop/DSPG")
extracted_data <- unzip("Data/SIPPDataset2023.zip", "pu2023.csv")

#Getting all useful variables
pu <- fread(extracted_data, sep = "|", select = c(
  #Identifier
  'EDOB_BMONTH', 'EEDUC', 'EMS', 'ERACE', 'ESEX', 
  'RCHTYP1', 'RCHTYP2', 'RCHTYP3','RCHTYP4', 'RCHTYP5', 'RCHTYP6', 'RCHTYP7', 'RCHTYP8',
  'RCHTYP9', 'RCHTYP10',
  'RFAMKIND', 'TAGE', 'TDOB_BYEAR','TIMSTAT', 'TRACE', 'TYRENTRY', 'ENATCIT',
  'TAGE_FB', 'TEHC_METRO', 'TEHC_REGION',
  #Childcare
  'ECHLD_ANY', 'EDAYCARE', 'EDAYHS', 'EFAM', 'EGRAN','EHEADST','EKIDSELF', 'ELIST',
  #'ECREPAYANYON', DNE, only in 2020
  'ENREL', 'ENUR','ENURHS', 'EOTHR','EPAR', 'EPAY', 'EPAYHELP','EPROG','ESELF',
  'ESIB15','ETIMELOST','ETIMELOST_TP','EWHOPAID1','EWHOPAID2','EWHOPAID3','EWHOPAID4',
  'EWHOPAID5','EWORKMORE','TPAYWK','RDAYCARE','RDAYHS','RFAM','RGRAN','RHEADST',
  'RKIDSELF','RNREL','RNUR','RNURHS','ROTHR','RPAR','RPROG','RSELF','RSIB15',
  #Controls
  'RGED','EJB1_DYSWKD','EJB2_DYSWKD','SPANEL',
  #Labor Supply
  'EJB1_AWOP1','EJB1_PTRESN1','EJB1_WSMNR',
  'TWKHRS1','TWKHRS2', 'TWKHRS3', 'TWKHRS4', 'TWKHRS5',
  'TMWKHRS',"EJB1_PVWKTR9", "EJB1_SCRNR", "THTOTINC",
  #Child Variables
  'TCBYR_1', 'TCBYR_2', 'TCBYR_3', 'TCBYR_4', 'TCBYR_5', 'TCBYR_6',"ECHLD_MNYN",
  # Any number after is suppressed
  "SHHADID", "MONTHCODE", "PNUM", "SSUID", "WPFINWGT", "TST_INTV"
  
  
))

#Choosing Variables needed for Model
vars <- c("SSUID","SHHADID","PNUM","MONTHCODE","SPANEL","WPFINWGT", "EEDUC","TAGE","ESEX", "ERACE", "EMS", "RFAMKIND", "TEHC_METRO", "TEHC_REGION","EOTHR", "THTOTINC",
          "EPAR", "EPAYHELP","ETIMELOST", "ETIMELOST_TP", "EWHOPAID1", "ELIST", "TWKHRS1", "TWKHRS2", "TWKHRS3", "TWKHRS4",
          "TWKHRS5", "TMWKHRS", "EJB1_PVWKTR9", 'TCBYR_1', 'TCBYR_2', 'TCBYR_3', 'TCBYR_4', 'TCBYR_5', 'TCBYR_6', "EJB1_SCRNR", "TST_INTV","ECHLD_MNYN")
data <- pu %>% 
  select(all_of(vars))


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

#Childcare Usage
data <- data %>% 
  mutate(ChildCare = if_else(ECHLD_MNYN == 1,1,0))

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

data2$PID <- paste0(as.character(data2$PID),"'")

write.csv(data2,"Childcare Project/Data/SIPP Output/20-23SIPPData.csv")


VAData <- data2 %>% 
  filter(State == 51)

write.csv(VAData,"Childcare Project/Data/SIPP Output/20-23VASIPPData.csv")
