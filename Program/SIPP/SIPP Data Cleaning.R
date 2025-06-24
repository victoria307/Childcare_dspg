
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
library(jsonlite)
library(tidycensus)

# API ---------------------------------------------------------------------

#Get your own API Key at https://www.census.gov/data/developers.html
# Replace key
api_key <- "cfd736f1fb3d1423e9518483a9d58aff08e22403"
vars <- c("SSUID","SHHADID","PNUM","MONTHCODE","SPANEL","WPFINWGT", "EEDUC","TAGE","ESEX", "ERACE", "EMS", "RFAMKIND", "TEHC_METRO", "TEHC_REGION","EOTHR", "THTOTINC",
          "EPAR", "EPAYHELP","ETIMELOST", "ETIMELOST_TP", "EWHOPAID1", "ELIST", "TWKHRS1", "TWKHRS2", "TWKHRS3", "TWKHRS4",
          "TWKHRS5", "TMWKHRS", "EJB1_PVWKTR9", 'TCBYR_1', 'TCBYR_2', 'TCBYR_3', 'TCBYR_4', 'TCBYR_5', 'TCBYR_6', "EJB1_SCRNR")

# Define endpoint and parameters
url <- "https://api.census.gov/data/2023/sipp"
params <- list(
  get = paste(vars, collapse = ","),  # Variable to retrieve
  key = api_key
)

# Make the GET request
res <- GET(url, query = params)

# Parse JSON content
raw_data <- fromJSON(content(res, "text"))

# Convert to a data frame and assign column names
data <- as.data.frame(raw_data[-1, ], stringsAsFactors = FALSE)
colnames(data) <- raw_data[1, ]
data[] <- lapply(data, function(x) {
  if (all(is.na(as.numeric(as.character(x)))) & any(!is.na(x))) x else as.numeric(as.character(x))
})


# Alternative to API ---------------------------------------------------------

#If API does not work
#Download the data from this google drive link https://drive.google.com/file/d/1bC0NgqXxP0sTyHe3jC7cAqeDn_LJrOOC/view?usp=drive_link
# Put this data in the Data folder of the git
#Pulling the Data
extracted_data <- unzip("Data/pu2023_csv.zip", "pu2023.csv")


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
  'TCBYR_1', 'TCBYR_2', 'TCBYR_3', 'TCBYR_4', 'TCBYR_5', 'TCBYR_6',
  # Any number after is suppressed
  "SHHADID", "MONTHCODE", "PNUM", "SSUID"
  
))

#Choosing Variables needed for Model
data <- pu %>% 
  select(all_of(vars))





# Data Cleaning -----------------------------------------------------------



## Filtering --------------------------------------------------------------
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
unique(data$TMWKHRS)
data <- data %>%
  mutate(
    HRSWorked = replace_na(TMWKHRS, 0),
    HRSWorked = case_when(
      HRSWorked == -999 ~ 0,
      TRUE ~ HRSWorked
    )
  )


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
# Dataset uses -999 for NA Birth Years
data <- data %>%
  mutate(age1 = if_else(is.na(TCBYR_1), -1, SPANEL - TCBYR_1),
         age_group1 = cut(
           age1,
           breaks = c(-Inf, -1, 1, 3, 5, 13, 1000, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy","No Child"),
           right = TRUE
         ),
         age2 = if_else(is.na(TCBYR_2), -1, SPANEL - TCBYR_2),
         age_group2 = cut(
           age2,
           breaks = c(-Inf, -1, 1, 3, 5, 13, 1000, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy","No Child"),
           right = TRUE
         ),
         age3 = if_else(is.na(TCBYR_3), -1, SPANEL - TCBYR_3),
         age_group3 = cut(
           age3,
           breaks = c(-Inf, -1, 1, 3, 5, 13, 1000, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy","No Child"),
           right = TRUE),
         age4 = if_else(is.na(TCBYR_4), -1, SPANEL - TCBYR_4),
         age_group4 = cut(
           age4,
           breaks = c(-Inf, -1, 1, 3, 5, 13, 1000, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy","No Child"),
           right = TRUE),
         age5 = if_else(is.na(TCBYR_5), -1, SPANEL - TCBYR_5),
         age_group5 = cut(
           age5,
           breaks = c(-Inf, -1, 1, 3, 5, 13, 1000, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy","No Child"),
           right = TRUE),
         age6 = if_else(is.na(TCBYR_6), -1, SPANEL - TCBYR_6),
         age_group6 = cut(
           age6,
           breaks = c(-Inf, -1, 1, 3, 5, 13, 1000, Inf),
           labels = c("No Child", "Infant", "Toddler", "Preschool", "School Age", "Past Subsidy","No Child"),
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


#Making EWHOPAID into a factor
data <- data %>% 
  mutate(WelfareorSS = if_else(EWHOPAID1 == 1,1,0)) %>% 
  mutate(WelfareorSS = replace_na(WelfareorSS,0))

#Creating Work from home Indicator
data <- data %>% 
  mutate(WorkFromHome = if_else(EJB1_PVWKTR9 == 1,1,0)) %>% 
  mutate(WorkFromHome = replace_na(WorkFromHome,0))
summary(data$WorkFromHome)


data2 <- data %>% 
  select(37:ncol(data)) %>% 
  filter(Kid1 == 1)


write.csv(data2,"Data/SIPPcleaneddata.csv")




