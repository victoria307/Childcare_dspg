# Load necessary libraries
library(readr)
library(readxl)
library(dplyr)
library(janitor)
library(tidyr)
library(writexl)

# Loads the dataset
data <- read_excel("Women's Bureau Data Work/Female Labor Force Participation/Virginia_Only.xlsx")


# Clean column names to be consistent 
data <- clean_names(data)

# Creates the model_data with only needed variables
model_data <- data %>%
  select(
    studyyear, county_name, county_fips_code,  # Keep time/location info
    femr_20to64,             # Target variable
    mcinfant, mfccinfant, mctoddler, mcpreschool,    # Childcare prices
    mhi, mfi,                # Income
    pr_f, pr_p,              # Poverty
    onerace_w, onerace_b, hispanic,  # Race
    h_under6_singlem, h_under6_bothwork  # Household structure
  ) %>%
  filter(!is.na(femr_20to64)) %>%   # Ensure target is present
  drop_na()  # Drop rows with NA in any other selected fields

# Preview cleaned dataset
glimpse(model_data)
write_xlsx(model_data, path = "Virginia_Model_Data.xlsx")
