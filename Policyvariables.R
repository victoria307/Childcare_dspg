#Packages
library(dplyr)
library(tidyverse)
library(readxl)
library(lubridate)
library(tidyr)

# Read the Excel file
data <- read_excel("~/DPSG/DPSG dir/Data/VDOE/CE by locality month_2025-07-03_VA Tech.xlsx")
#Recode Date to be full date
data$date <- ym(data$date)  # e.g., "2017-08" → 2017-08-01

#Creating Dummy's ---------
#Change to Market Rate Change to
data$MR70per <- ifelse(data$date >= ymd("2018-06-01"), 1, 0) 

data$EL85LFW <- ifelse(data$date >= ymd("2021-03-01"), 1, 0)

data$NoWaitListPeriod <- ifelse(data$date >= ymd("2022-03-01") & data$date <= ymd("2024-07-01"), 1, 0)

data$NoCopayPeriod <- ifelse(data$date >= ymd("2021-04-01")& data$date <= ymd("2022-12-31"), 1, 0)

#Clean data-----
data <- data %>%
  separate(Locality, into = c("code", "county"), sep = "-") %>%
  mutate(
    code = stringr::str_pad(code, width = 3, pad = "0"),  # make sure it's 3 digits
    fips = paste0("51", code)                             # prepend "51"
  )
data <- data %>%
  rename_with(~ gsub("\\s+", "_", .))


write.csv(data, "~/DPSG/DPSG dir/Data/VDOE/VDOEpolicydummy", row.names = FALSE)

