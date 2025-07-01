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
  filter(STUDYYEAR != 2021, STUDYYEAR != 2008, STUDYYEAR != 2009)


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


# UI
ui <- fluidPage(
  titlePanel("Income-Adjusted Childcare Costs in Virginia"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:",
                  min = min(va_map_data$STUDYYEAR),
                  max = max(va_map_data$STUDYYEAR),
                  value = min(va_map_data$STUDYYEAR),
                  step = 1,
                  sep = ""),
      selectInput("type", "Select Childcare Type:",
                  choices = sort(unique(va_map_data$CostType))),
      actionButton("play", "▶ Play"),
      actionButton("pause", "⏸ Pause")
    ),
    mainPanel(
      leafletOutput("childcareMap", height = 600)
    )
  )
)

# Server
server <- function(input, output, session) {
  playing <- reactiveVal(FALSE)
  
  # Reactive subset of the data
  filtered_data <- reactive({
    va_map_data %>%
      filter(STUDYYEAR == input$year, CostType == input$type)
  })
  
  # Debugging output
  observe({
    data_now <- filtered_data()
    cat("🧪 Filtered Data Check:\n")
    cat("  Rows returned:", nrow(data_now), "\n")
    cat("  Year:", input$year, "\n")
    cat("  CostType:", input$type, "\n")
    if (nrow(data_now) == 0) {
      cat("⚠️  Warning: No data for this selection.\n")
    }
  })
  
  # Handle Play button
  observeEvent(input$play, {
    playing(TRUE)
  })
  
  # Handle Pause button
  observeEvent(input$pause, {
    playing(FALSE)
  })
  
  # Animate year
  observe({
    req(playing())
    isolate({
      current_year <- input$year
      years <- sort(unique(va_map_data$STUDYYEAR))
      next_year <- years[which(years == current_year) + 1]
      if (!is.na(next_year)) {
        updateSliderInput(session, "year", value = next_year)
      } else {
        playing(FALSE)  # Stop if at final year
      }
    })
    invalidateLater(1000, session)
  })
  
  # Set consistent color scale across all data
  pal <- colorNumeric(
    palette = "YlOrRd",
    domain = va_map_data$Residuals_Normalized,
    na.color = "transparent"
  )
  
  # Initial Map
  output$childcareMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -78.5, lat = 37.5, zoom = 7)
  })
  
  # Update map with current filtered data
  observe({
    data_now <- filtered_data()
    req(nrow(data_now) > 0)
    
    leafletProxy("childcareMap", data = data_now) %>%
      clearGroup("childcare") %>%
      addPolygons(
        group = "childcare",
        fillColor = ~pal(Residuals_Normalized),
        color = "#666666",
        weight = 1,
        fillOpacity = 0.8,
        smoothFactor = 0.5,
        popup = ~paste0(
          "<b>", NAME, "</b><br>",
          "Childcare affordability scores, adjusted for income differences: ", round(Residuals_Normalized, 2), "<br>",
          "Median Family Income: $", formatC(MFI, format = "f", big.mark = ",", digits = 0)
        )
      ) %>%
      clearControls() %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = data_now$Residuals_Normalized,
        title = "Income-adjusted childcare costs",
        opacity = 0.8
      )
  })
}

# Run the app
shinyApp(ui, server)





