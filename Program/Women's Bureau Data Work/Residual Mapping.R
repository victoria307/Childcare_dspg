cat("\014")
rm(list = ls())


library("dplyr")
library("tidyr")
library(leaflet)
library(purrr)
library(broom)
library(leaflet)
library(tigris)
library(sf)



data <- read.csv("Data/VA_data.csv") 

data <- data %>% 
  select(STATE_NAME, COUNTY_NAME, COUNTY_FIPS_CODE, STUDYYEAR, MFI, MCINFANT, MCTODDLER, 
         MCPRESCHOOL, MFCCINFANT, MFCCTODDLER, MFCCPRESCHOOL) %>% 
  filter(STUDYYEAR != 2021, STUDYYEAR != 2008)


data_long <- data %>%
  pivot_longer(cols = c(MCINFANT, MCTODDLER, MCPRESCHOOL,MFCCINFANT, MFCCTODDLER,
                        MFCCPRESCHOOL),
               names_to = "CostType",
               values_to = "ChildcareCost")

# Step 2: Nest by STUDYYEAR and CostType
nested <- data_long %>%
  group_by(STUDYYEAR, CostType) %>%
  nest()

# Step 3: Run regressions and get residuals
results <- nested %>%
  mutate(model = map(data, ~ lm(ChildcareCost ~ MFI, data = .x)),
         residuals = map2(data, model, ~ resid(.y))) %>%
  unnest(c(data, residuals)) %>% 
  mutate(COUNTY_FIPS_CODE = as.character(COUNTY_FIPS_CODE),
         CostType = case_when(
           CostType == "MCINFANT" ~ "Median Center Based Infant Care",
           CostType == "MCTODDLER" ~"Median Center Based Toddler Care",
           CostType == "MCPRESCHOOL" ~ "Median Center Based Preschool Care",
           CostType == "MFCCINFANT" ~ "Median Family Based Infant Care",
           CostType == "MFCCTODDLER" ~ "Median Family Based Toddler Care",
           CostType == "MFCCPRESCHOOL" ~ "Median Family Based Preschool Care"
         )) %>% 
  group_by(STUDYYEAR, CostType) %>%
  mutate(
    Residuals_Normalized = (residuals - min(residuals, na.rm = TRUE)) /
      (max(residuals, na.rm = TRUE) - min(residuals, na.rm = TRUE)) * 100
  ) %>%
  ungroup()

options(tigris_use_cache = TRUE)
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")

va_map_data <- va_counties %>%
  left_join(results, by = c("GEOID" = "COUNTY_FIPS_CODE"))

va_map_data <- st_transform(va_map_data, crs = 4326)
pal <- colorQuantile("YlOrRd", domain = va_map_data$MFCCPRESCHOOL, n = 5)







# rshiny ------------------------------------------------------------------


