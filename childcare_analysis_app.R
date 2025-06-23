# Load Packages ---------------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(tigris)
library(leaflet)
library(sf)
library(plotly)
library(shinydashboard)

# Data --------------------------------------------------------------------

# Import data
labor_data <- read_excel("Women's Bureau Data Work/labor_data.xlsx")

# Create VA data
VA_data <- labor_data %>%
  filter(STATE_NAME == "Virginia")

# Prepare VA counties spatial data
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  mutate(FIPS = GEOID) %>%
  st_transform(crs = 4326)

# Format FIPS codes consistently
VA_data <- VA_data %>%
  mutate(COUNTY_FIPS_CODE = str_pad(as.character(COUNTY_FIPS_CODE), width = 5, pad = "0"))

# Join spatial and attribute data
map_data_all <- va_counties %>%
  left_join(VA_data, by = c("GEOID" = "COUNTY_FIPS_CODE"))

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  title = "Childcare Analysis Project",
  
#CSS Styling for the colors, fonts, and layouts
  header = tagList(
    tags$head(
      tags$style(HTML("
        body {
          background-color: #ffffff;
          font-family: 'Times New Roman', serif;
        }
        .navbar {
          background-color: #800000;
        }
        .navbar-default .navbar-nav > li > a {
          color: white;
        }
        .navbar-default .navbar-brand {
          color: white;
          font-family: 'Times New Roman', serif;
          font-weight: bold;
          font-size: 20px;
        }
        .tab-content {
          background-color: #ffffff;
          padding: 20px;
          border-radius: 5px;
        }
        h2 {
          color: #2e8b57;
          font-family: 'Times New Roman', serif;
        }
        p {
          color: #333333;
          font-family: 'Times New Roman', serif;
        }
      "))
    )
  ),
  
#Background Tab
  tabPanel("Background",
           fluidPage(
             h2("Background"),
             p("Write the background information here.")
           )
  ),

#Methodology Tab
  tabPanel("Methodology",
           fluidPage(
             h2("Methodology"),
             p("Details of our methodology.")
           )
  ),

#Timeline Tab
  tabPanel("Timeline",
           fluidPage(
             h2("Timeline"),
             p("Timeline content goes here.")
           )
  ),

#Text Mining Tab  
  tabPanel("Text Mining Info",
           fluidPage(
             h2("Text Mining Info"),
             p("Explaining text mining methods or results.")
           )
  ),

#Maps Tab with Leaflet Map  
  tabPanel("Maps",
           fluidPage(
             h2("Virginia Childcare Costs as a Percent of Annual Family Income"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("STUDYYEAR", "Select Year", choices = 2009:2022),
                 selectInput("percentile", "Select Pricing Point:", choices = c("50th Percentile"="M", "75th Percentile"="_75")),
                 selectInput("age_group", "Select Age Group:", choices = c(
                   "0 to 5 Months" = "Bto5",
                   "6 to 11 Months" = "6to11",
                   "12 to 17 Months" = "12to17",
                   "18 to 23 Months" = "18to23",
                   "24 to 29 Months" = "24to29",
                   "30 to 35 Months" = "30to35",
                   "36 to 41 Months" = "36to41",
                   "42 to 47 Months" = "42to47",
                   "48 to 53 Months" = "48to53",
                   "54 Months to School Age" = "54toSA",
                   "School Age" = "SA"
                 )),
                 selectInput("provider", "Select Provider Type:", choices = c("Center Care"="C", "Home Care"="FCC"))
               ),
               mainPanel(
                 leafletOutput("childcareMap", height = "600px")
               )
             )
           )
  ),
  
  tabPanel("Modeling",
           fluidPage(
             h2("Modeling"),
             p("Describing the models or predictions.")
           )
  ),
  
  tabPanel("Thank You",
           fluidPage(
             h2("Thank You"),
             p("Closing remarks and acknowledgments.")
           )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  filtered_data <- reactive({
    cost_col <- paste0(input$percentile, input$provider, input$age_group)
    df <- map_data_all %>%
      filter(STUDYYEAR == input$STUDYYEAR) %>%
      mutate(cost_pct_income = (.data[[cost_col]] * 52) / MFI) %>%
      filter(!is.na(cost_pct_income), is.finite(cost_pct_income), cost_pct_income > 0)
    df
  })
  
  output$childcareMap <- renderLeaflet({
    df <- filtered_data()
    pal <- colorNumeric("viridis", df$cost_pct_income, na.color = "gray90")
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(cost_pct_income),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = ~paste0(NAME, ": ", scales::percent(cost_pct_income, accuracy = 1)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = df$cost_pct_income,
        title = "Cost as % of Income",
        labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x)
      )
  })
}

# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)
