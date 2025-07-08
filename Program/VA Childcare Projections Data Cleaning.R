#Clean UVA Population Data to be readable in R

rm(list=ls())
cat("\014")

library(dplyr)
library(readxl)
library(stringr)
library(tidyr)

# Import Data
# Getting each sheet for yearly projections
# Cleaning each sheet
data1 <- read_excel("Data/VApopProjection.xlsx", sheet = 1)
data1 <- data1[-c(1:3),]
colnames(data1) <- c("FIPS", "Name", "2030 Total Population", "2030 Total 0-4") 
data1 <- data1[,c(1:4)]


data2 <- read_excel("Data/VApopProjection.xlsx", sheet = 2)
data2 <- data2[-c(1:3),]
colnames(data2) <- c("FIPS", "Name", "2040 Total Population", "2040 Total 0-4")
data2 <- data2[,c(1:4)]

data3 <- read_excel("Data/VApopProjection.xlsx", sheet = 3)
data3 <- data3[-c(1:3),]
colnames(data3) <- c("FIPS", "Name", "2050 Total Population", "2050 Total 0-4")
data3 <- data3[,c(1:4)]

# Combining the Sheet
total <- merge(data1, data2,by = c("FIPS","Name"))
total <- merge(total,data3,by = c("FIPS","Name"))

#Split the county name so it can be split
total <- total %>% 
  mutate(Name = str_replace_all(Name, "County", "")) %>% 
  mutate(Name = str_trim(str_to_title(Name))) %>% 
  mutate(Name = str_replace_all(Name, "City","")) %>% 
  mutate(Name = str_trim(str_to_title(Name))) 
  
  

#Bring in Child Care Supply Data
supply <- read.csv("Data/childcareawareSupply.csv")
supply <- supply %>% 
  mutate(Name = str_replace_all(City, "%20", " ")) %>% 
  mutate(Name = str_replace_all(Name, "City","")) %>% 
  mutate(Name = str_replace_all(Name, "County", "")) %>% 
  mutate(Name = str_trim(str_to_title(Name)))
#Fix errors in data
supply$Name[21] <- "Carroll"
supply$Name[95] <- "Portsmouth"  

#Merge together to get final dataset
FinalData <- left_join(total, supply, by = c("Name"))

#check to see which columns did not work
check <- FinalData %>% 
  filter(is.na(Licensed_Capacity_Total))



write.csv(FinalData, "Data/CleanedVAPopData.csv")
