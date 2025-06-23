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

# Create VA data
VA_data <- read_excel("Data/VA_data.csv")

# Prepare VA counties spatial data
va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  mutate(FIPS = GEOID) %>%
  st_transform(crs = 4326)

# Prepare VA_data: format FIPS codes consistently
VA_data <- VA_data %>%
  mutate(COUNTY_FIPS_CODE = str_pad(as.character(COUNTY_FIPS_CODE), width = 5, pad = "0"))

# Join spatial data once (keep all VA_data rows, so many-to-one join)
map_data_all <- va_counties %>%
  left_join(VA_data, by = c("GEOID" = "COUNTY_FIPS_CODE"))

view(map_data_all)



# Working on Shiny % Of Income --------------------------------------------------------

#create user interface
ui <- fluidPage(
  titlePanel("Virginia Childcare Costs as a Percent of Annual Family Income"), 
  sidebarLayout(
    sidebarPanel( #these next things end up being the choice boxes for it all
      selectInput("STUDYYEAR", "Select Year", choices = 2009:2022),
      selectInput("percentile", "Select Pricing Point:", choices = c("50th Percentile"="M", "75th Percentile"="_75")),
      selectInput("age_group", "Select Age Group:", choices = c(
        #MAKE SURE WHAT YOU WANT TO SEE IS FIRST AND AFTER IS REAL CODE
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
      selectInput("provider", "Select Provider Type:", choices = c( "Center Care"="C", "Home Care"="FCC"))
    ), 
    mainPanel ( #tells r to create a map 
      leafletOutput("childcareMap", height= "600px")
    )
  )
)


#make server filer data based on what the user selects

server <- function(input, output, session) { 
  filtered_data <- reactive ({
    cost_col <- paste0(input$percentile, input$provider, input$age_group) # builds the name of the column based on the choices ex: MCBto5
    #filter the spatial data into the selected years
    df <- map_data_all %>% 
      filter(STUDYYEAR == input$STUDYYEAR) %>% 
      #mutate cost to a percent of FAMILY income
      mutate(cost_pct_income = (.data[[cost_col]]*52)/MFI) %>%
      filter(!is.na(cost_pct_income), is.finite(cost_pct_income), cost_pct_income > 0 )
    cat("Columns in filtered df:\n")
    print(colnames(df))
    print(head(df[, c("GEOID", "NAME", cost_col)]))
    cat("Cost column used:", cost_col, "\n")
    cat("Summary of cost_pct_income:\n")
    print(summary(df$cost_pct_income))
    df
  })
  #render the leaflet map
  output$childcareMap <- renderLeaflet({
    df <- filtered_data()
    #color palette
    pal <- colorNumeric("viridis", df$cost_pct_income, na.color= "gray90")
    #build map
    leaflet(df) %>%  #tells to use the right data(sets base layer)
      addProviderTiles(providers$CartoDB.Positron) %>%  #provides background tile layer to the map
      addPolygons( #adds county shapes 
        fillColor = ~pal(cost_pct_income),
        weight = 1,
        color = "white",
        fillOpacity = 0.7, 
        label = ~paste0(NAME, ":", scales::percent(cost_pct_income, accuracy = 1)),
        labelOptions = labelOptions(
          style = list(style = list("font-weight" = "normal", padding = "3px 8px"),
                       textsize = "15px",
                       direction = "auto")
        )
      ) %>%
      addLegend(
        pal= pal,
        values = df$cost_pct_income,
        title = "Cost as % of Income",
        labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x )
      )
  })
}
shinyApp(ui=ui, server = server)