cat("\014")
rm(list = ls())


library("dplyr")
library("tidyr")
library(leaflet)
library(purrr)
library(broom)


data <- read.csv("Data/VA_data.csv") 

data <- data %>% 
  select(STATE_NAME, COUNTY_NAME, COUNTY_FIPS_CODE, STUDYYEAR, MFI, MCINFANT, MCTODDLER, 
         MCPRESCHOOL, MFCCINFANT, MFCCTODDLER, MFCCPRESCHOOL) %>% 
  filter(STUDYYEAR != 2021, STUDYYEAR != 2008)


yearly_models <- data %>%
  group_by(STUDYYEAR) %>%
  group_split() %>% 
  map(~ lm(MCINFANT ~ MFI, data = .x))

# To get residuals and attach them back to the data
df_with_residuals <- map2_dfr(
  yearly_models, 
  data %>% group_split(STUDYYEAR), 
  ~ mutate(.y, Residuals = resid(.x))
)