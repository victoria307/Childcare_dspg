#Packages
library(dplyr)
library(tidyverse)
library(fixest)
library(lubridate)

# When reading from CSV
mydata <- read_csv("~/DPSG/DPSG dir/Data/VDOE/VDOEpolicydummy")

#Make total count numeric
mydata$`Total_Count` <- ifelse(mydata$`Total_Count` == "redacted", NA, mydata$`Total_Count`)
mydata$`Total_Count` <- as.numeric(mydata$`Total_Count`)


#Model
expmodel <- feols(Expenditures ~ 
                 MR70per +
                 EL85LFW +
                 NoWaitListPeriod +
                 NoCopayPeriod |
                 county^year +
                 month, data=mydata)
summary(expmodel)

tcmodel <- feols(Total_Count ~ 
                 MR70per +
                 EL85LFW +
                 NoWaitListPeriod +
                 NoCopayPeriod |
                 county^year +
                 month, data=mydata)
summary(tcmodel)

#Log Modeling
logexp <- feols(log(Expenditures) ~ 
                  MR70per + 
                  EL85LFW + 
                  NoWaitListPeriod + 
                  NoCopayPeriod | county + year^month,
                  data = mydata)
logtc <- feols(log(Total_Count) ~
                     MR70per +
                     EL85LFW +
                     NoWaitListPeriod +
                     NoCopayPeriod |
                     county^year + month, 
                   data = mydata)
summary(logexp)


#EVENT STUDIES---------------------------- 
#MR 70 Event Study Parralel Trends do not hold true
mr70_start <- ymd("2018-06-01")

mydata <- mydata %>%
  mutate(event_time_mr70 = as.integer((year(date) - year(mr70_start)) * 12 + month(date) - month(mr70_start)))
mydata_filtered <- mydata %>%
  filter(event_time_mr70 >= -24 & event_time_mr70 <= 24)

event_model_mr70 <- feols(
  log(Expenditures) ~ i(event_time_mr70, ref = -1) | county + factor(month) + factor(year),
  data = mydata_filtered
)

iplot(event_model_mr70,
      xlab = "Months Since MR70per Policy Start",
      ylab = "Change in log(Expenditures)",
      main = "Event Study (±2 Years Around MR70per)")
---------------------------------------------------------------------------------
  
#Event study 85% FPR and Looking for work Parallel assumptions hold true.
EL85_start <- ymd("2021-03-01")

# Create event time in months
mydata <- mydata %>%
  mutate(event_time_EL85 = as.integer((year(date) - year(EL85_start)) * 12 + month(date) - month(EL85_start)))
mydata_filtered <- mydata %>%
  filter(event_time_EL85 >= -24 & event_time_EL85 <= 24)

event_model_EL85 <- feols(
  log(Expenditures) ~ i(event_time_EL85, ref = -1) | county + factor(year) + factor(month),  #had to drop year and month interacting fixed effects due to coliniearity
  data = mydata_filtered
)


iplot(event_model_EL85 ,
      xlab = "Months Since EL85 Policy Start",
      ylab = "Change in log(Expenditures)",
      main = "Event Study (±2 Years Around EL85)")

#Event study no waitlist 
NOWL_start <- ymd("2022-03-01")

  mydata <- mydata %>%
  mutate(event_time_NOWL = as.integer((year(date) - year(NOWL_start)) * 12 + month(date) - month(NOWL_start)))
mydata_filtered <- mydata %>%
  filter(event_time_NOWL >= -24 & event_time_NOWL <= 24)

event_model_NOWL <- feols(
  log(Expenditures) ~ i(event_time_NOWL, ref = -1) | county + factor(year) + factor(month),  #had to drop year and month interacting fixed effects due to coliniearity
  data = mydata_filtered
)


iplot(event_model_NOWL ,
      xlab = "Months Since NOWL Policy Start",
      ylab = "Change in log(Expenditures)",
      main = "Event Study (±2 Years Around NOWL)")

#No Copayments 
NOCP_start <- ymd("2021-04-01")

mydata <- mydata %>%
  mutate(event_time_NOCP = as.integer((year(date) - year(NOCP_start)) * 12 + month(date) - month(NOCP_start)))
mydata_filtered <- mydata %>%
  filter(event_time_NOCP >= -24 & event_time_NOCP <= 24)

event_model_NOCP <- feols(
  log(Expenditures) ~ i(event_time_NOCP, ref = -1) | county + factor(year) + factor(month),  #had to drop year and month interacting fixed effects due to coliniearity
  data = mydata_filtered
)

iplot(event_model_NOCP ,
      xlab = "Months Since NOCP Policy Start",
      ylab = "Change in log(Expenditures)",
      main = "Event Study (±2 Years Around NOCP)")

summary(event_model_NOCP)
