# Upload Packages ---------------------------------------------------------

#load packages
library(tidyverse)
library(readxl)
library(tigris)
library(leaflet)
library(sf)
library(plotly)
library(shiny)
library(shinydashboard)

# Data --------------------------------------------------------------------

#import data
labor_data <- read_excel("Program/Women's Bureau Data Work/labor_data.xlsx")
view(labor_data)

# Create VA data
VA_data <- labor_data %>%
  filter(STATE_NAME == "Virginia")
view(VA_data)
write.csv(VA_data,"Data/VA_data.csv")