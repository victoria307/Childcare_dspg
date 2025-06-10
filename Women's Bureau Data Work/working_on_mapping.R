#load packages
library(tidyverse)
library(readxl)
library(tigris)
library(leaflet)
library(sf)

#import data
labor_data <- read_excel("/Users/gracemullins/DPSG/labor_data.xlsx")
view(labor_data)

# Create VA data
VA_data <- labor_data %>%
  filter(STATE_NAME == "Virginia")
view(VA_data)

# Prepare VA counties spatial data
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  mutate(FIPS = GEOID)

# Prepare VA_data: format FIPS codes consistently
VA_data <- VA_data %>%
  mutate(COUNTY_FIPS_CODE = str_pad(as.character(COUNTY_FIPS_CODE), width = 5, pad = "0")) %>%

# Join spatial data once (keep all VA_data rows, so many-to-one join)
map_data_all <- va_counties %>%
  left_join(VA_data, by = c("GEOID" = "COUNTY_FIPS_CODE"))


# Ref year  -----------------------------------------------------------------

#create year ref
year_to_plot <- 2016

# Employment Rate Graph  --------------------------------------------------
ggplot(
  filter(map_data_all, STUDYYEAR == year_to_plot)
) +
  geom_sf(aes(fill = EMR_16), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "1", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = paste("Employment Rate (EMR_16) by County in Virginia -", year_to_plot),
    fill = "Percent Employment Rate"
  )

#Map of Price of Childcare Centers for 6-11 Months 2022----------------------
ggplot(
  filter(map_data_all, STUDYYEAR == year_to_plot)
) +
  geom_sf(aes(fill = MC6to11), color = "white", size = 0.1) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey90") +
  theme_minimal() +
  labs(
    title = paste("Median Weekly Price for Childcare Centers for 6–11 Month Olds in", year_to_plot),
    fill = "Median Weekly Cost"
  )


#Map of Price of Childcare centers as percent of income ------------------
# Then mutate and plot

# mutate into % of income
map_data_all <- map_data_all %>%
mutate(CCBto5_as_income = (MCBto5 * 52) / MHI)  # calculate ratio here for all years

# Map: Childcare cost as % of income
ggplot(
  filter(map_data_all, STUDYYEAR == year_to_plot)
) +
  geom_sf(aes(fill = CCBto5_as_income), color = "white", size = 0.1) +
  scale_fill_viridis_c(
    option = "viridis",
    na.value = "grey90",
    labels = scales::label_percent(accuracy = 1)
  ) +
  theme_minimal() +
  labs(
    title = paste("Childcare Center Cost as % of Income (Ages B–5) in", year_to_plot),
    fill = "Cost as % of Income"
  )

