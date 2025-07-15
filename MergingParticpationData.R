#Packages-----------
library(dplyr)
library(tidyr)
library(fixest)
library(ggplot2)
library(tidyverse)
library(readxl)

#load data---------
VAdata <- read.csv("Data/EMPandRural.csv")
participation<- read_excel("~/DPSG/DPSG dir/Data/Child care assistance participation  (1).xlsx")

#Clean and Merge data---------

#clean VA data
VAdata_clean <- VAdata %>%
  rename(
    County = COUNTY_NAME,
    Year = STUDYYEAR
  )

VAdata_clean <- VAdata_clean %>%
  mutate(County = str_remove_all(County, "County"))%>%
  mutate(County = str_trim(str_to_title(County)))
  

#clean participation data
participation_clean <- participation %>%
  rename(
    County = Location,
    Year = TimeFrame
  )

# Convert character to integer
VAdata_clean$Year <- as.integer(VAdata_clean$Year)
participation_clean$Year <- as.integer(participation_clean$Year)

VA_full_data <- left_join(VAdata_clean, participation_clean, by = c("County", "Year"))

VA_full_data$Year <- as.integer(VA_full_data$Year)

merged_data <- VA_full_data %>%
  filter(Year >= 2014 & Year <= 2022)

merged_data <- merged_data %>%
  rename(
    Subsidy_Participation = Data
  )
check <- merged_data %>% 
  filter(is.na(Subsidy_Participation))

write.csv(merged_data, "Data/VAdataparticipation.csv", row.names = FALSE)


