library(readxl)
library(dplyr)
library(sf)
library(tigris)
library(leaflet)

# Load the Excel data
virginia_data <- read_excel("C:/Users/waiy/OneDrive - Berea College/Documents/Childcare_draft/Virginia_Only.xlsx")

# Making sure FIPS codes are in correct format (zero-padded)
virginia_data <- virginia_data %>%
  mutate(COUNTY_FIPS_CODE = sprintf("%05d", as.numeric(COUNTY_FIPS_CODE)))

# Load VA county shapefile
options(tigris_use_cache = TRUE)
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")

# Join shapefile with the  data
va_map_data <- va_counties %>%
  left_join(virginia_data, by = c("GEOID" = "COUNTY_FIPS_CODE"))

# Reproject to WGS84 (Leaflet needs this CRS)
va_map_data <- st_transform(va_map_data, crs = 4326)

# Defining the pallete
pal <- colorQuantile("YlOrRd", domain = va_map_data$MFCCPRESCHOOL, n = 5)

# Using the palette in the map
leaflet(va_map_data) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    fillColor = ~pal(MFCCPRESCHOOL),
    color = "#333333",
    weight = 1,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>", NAME, "</b><br>",
      "Weekly Child Care Cost: $", round(MFCCPRESCHOOL, 2), "<br>",
      "Median Family Income: $", formatC(MFI, format = "f", big.mark = ",", digits = 0)
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~MFCCPRESCHOOL,
    title = "Weekly Child Care Cost",
    opacity = 0.8
  )

