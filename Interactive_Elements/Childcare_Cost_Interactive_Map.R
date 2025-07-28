# Clear the console and workspace
cat("\014")
rm(list=ls())

# Load Required Packages ------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(sf)
library(tigris)
library(plotly)
library(viridisLite)
library(scales)
options(tigris_use_cache = TRUE)

# Prepare Data -------------------------------------------------
data <- read_excel("Women's Bureau Data Work/Cleaned_Virginia_Womens_Bureau.xlsx") %>%
  mutate(
    FIPS = str_pad(as.character(COUNTY_FIPS_CODE), 5, pad = "0"),
    MCINFANT = as.numeric(MCINFANT),
    MCTODDLER = as.numeric(MCTODDLER),
    MCPRESCHOOL = as.numeric(MCPRESCHOOL),
    FLFPR_20to64_UNDER6 = as.numeric(FLFPR_20to64_UNDER6),
    Urban = as.numeric(Urban)
  )


va_map <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  mutate(FIPS = str_pad(as.character(GEOID), 5, pad = "0"))

# UI --------------------------------------------------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #ffffff;
        font-family: 'Times New Roman', serif;
      }
      h2 {
        color: #333333;
        font-size: 32px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 20px;
      }
      .sidebar {
        background-color: #d3ebf5;
        padding: 15px;
        border-radius: 5px;
      }
      .form-group {
        margin-bottom: 15px;
      }"))
  ),
  
  h2("Virginia Childcare Cost & Female Labor Force Participation"),
  
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      
      selectInput("view_mode", "Choose View Mode:",
                  choices = c("Single Year View" = "year", "Trend Over Time" = "trend"),
                  selected = "year"),
      
      conditionalPanel(
        condition = "input.view_mode == 'year'",
        selectInput("year", "Select Year:", choices = sort(unique(data$STUDYYEAR[data$STUDYYEAR != 2008])))
      ),
      
      selectInput("map_type", "Choose Data to Display:",
                  choices = c("Childcare Cost" = "cost", "Female Labor Force Participation (Under 6)" = "flfpr")),
      
      conditionalPanel(
        condition = "input.view_mode == 'year' && input.map_type == 'cost'",
        selectInput("cost_type", "Select Childcare Type:",
                    choices = c("Infant Care" = "MCINFANT",
                                "Toddler Care" = "MCTODDLER",
                                "Preschool Care" = "MCPRESCHOOL"),
                    selected = "MCINFANT")
      ),
      
      conditionalPanel(
        condition = "input.view_mode == 'trend' && input.map_type == 'cost'",
        checkboxGroupInput("trend_cost_types", "Select Childcare Types:",
                           choices = c("Infant Care" = "MCINFANT",
                                       "Toddler Care" = "MCTODDLER",
                                       "Preschool Care" = "MCPRESCHOOL"),
                           selected = c("MCINFANT", "MCTODDLER", "MCPRESCHOOL"))
      ),
      
      radioButtons("urban_filter", "Show Counties:",
                   choices = c("All" = "all", "Urban Only" = "urban", "Rural Only" = "rural"),
                   selected = "all"),
      
      conditionalPanel(
        condition = "input.view_mode == 'trend'",
        selectizeInput("county_picker", "Select Counties for Trend:", choices = NULL, multiple = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.view_mode == 'trend'",
        actionButton("clear_county", "Clear County Selection", class = "btn btn-warning")
      ),
      
      helpText("Childcare costs reflect weekly, full-time median prices charged by providers."),
      helpText("In 'Trend Over Time' mode, select or click counties to compare trends.")
    ),
    
    mainPanel(
      leafletOutput("map", height = "650px"),
      br(),
      conditionalPanel(
        condition = "input.view_mode == 'trend'",
        h4("Trend for Selected Counties", style = "text-align:center; font-weight:bold; font-family:'Times New Roman'"),
        plotlyOutput("trend_plot", height = "400px")
      )
    )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output, session) {
  selected_counties <- reactiveVal(character(0))
  
  observe({
    county_names <- sort(unique(data$COUNTY_NAME))
    updateSelectizeInput(session, "county_picker", choices = county_names)
  })
  
  observeEvent(input$county_picker, {
    selected_counties(input$county_picker)
  })
  
  observeEvent(input$map_shape_click, {
    if (input$view_mode == "trend") {
      clicked_fips <- input$map_shape_click$id
      clicked_name <- data %>% filter(FIPS == clicked_fips) %>% pull(COUNTY_NAME) %>% unique()
      if (length(clicked_name) == 1) {
        updated <- union(selected_counties(), clicked_name)
        selected_counties(updated)
        updateSelectizeInput(session, "county_picker", selected = updated)
      }
    }
  })
  
  observeEvent(input$clear_county, {
    selected_counties(character(0))
    updateSelectizeInput(session, "county_picker", selected = character(0))
  })
  
  filtered_data <- reactive({
    if (input$view_mode == "year") {
      data %>% filter(STUDYYEAR == input$year)
    } else {
      data %>%
        group_by(FIPS) %>%
        summarise(across(c(MCINFANT, MCTODDLER, MCPRESCHOOL, FLFPR_20to64_UNDER6, Urban), mean, na.rm = TRUE),
                  .groups = "drop")
    }
  })
  
  output$map <- renderLeaflet({
    map_data <- left_join(va_map, filtered_data(), by = "FIPS")
    
    if (input$map_type == "cost") {
      var <- if (input$view_mode == "year") input$cost_type else input$trend_cost_types[1]
      
      title <- paste(
        ifelse(input$view_mode == "trend", "Avg. Weekly Median Price", "Weekly Median Price"),
        "-", gsub("MC", "", var), "(USD)"
      )
      
      palette_values <- map_data[[var]]
      label_value <- paste0("Cost: $", ifelse(is.na(palette_values), "No data", round(palette_values, 2)))
      
      pal <- colorNumeric(
        palette = plasma(100),
        domain = palette_values,
        na.color = "#f0f0f0"
      )
      
    } else {
      var <- "FLFPR_20to64_UNDER6"
      title <- ifelse(input$view_mode == "trend",
                      "Avg. FLFPR of Mothers (Under 6)",
                      "FLFPR of Mothers (Under 6)")
      palette_values <- map_data[[var]]
      label_value <- paste0("FLFPR: ", ifelse(is.na(palette_values), "No data", round(palette_values, 1)), "%")
      
      pal <- colorNumeric(
        palette = viridis(100),
        domain = palette_values,
        na.color = "#f0f0f0"
      )
    }
    
    map_data$fill_color <- ifelse(
      input$urban_filter == "all" |
        (input$urban_filter == "urban" & map_data$Urban == 1) |
        (input$urban_filter == "rural" & map_data$Urban == 0),
      pal(palette_values),
      "#d3d3d3"
    )
    
    map_data$fill_opacity <- ifelse(
      input$urban_filter == "all" |
        (input$urban_filter == "urban" & map_data$Urban == 1) |
        (input$urban_filter == "rural" & map_data$Urban == 0),
      0.7, 0.3
    )
    
    label_text <- paste0(
      map_data$NAME, "<br>", label_value,
      "<br>Type: ", ifelse(map_data$Urban == 1, "Urban", "Rural")
    )
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~fill_color,
        fillOpacity = ~fill_opacity,
        weight = 1,
        color = "white",
        label = lapply(label_text, HTML),
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE),
        layerId = ~FIPS
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = palette_values,
        title = title,
        labFormat = if (input$map_type == "cost") labelFormat(prefix = "$") else labelFormat(suffix = "%"),
        opacity = 1
      )
  })
  
  output$trend_plot <- renderPlotly({
    locs <- selected_counties()
    req(length(locs) > 0)
    county_data <- data %>% filter(COUNTY_NAME %in% locs)
    
    num_colors <- length(locs)
    palette_fn <- function(n) viridis(n)
    county_colors <- palette_fn(num_colors)
    names(county_colors) <- locs
    
    if (input$map_type == "cost") {
      req(length(input$trend_cost_types) > 0)
      
      cost_data <- county_data %>%
        filter(!STUDYYEAR %in% c(2008, 2021)) %>%
        select(c("COUNTY_NAME", "STUDYYEAR", input$trend_cost_types)) %>%
        pivot_longer(cols = all_of(input$trend_cost_types), names_to = "Type", values_to = "Cost") %>%
        mutate(Type = recode(Type, MCINFANT = "Infant", MCTODDLER = "Toddler", MCPRESCHOOL = "Preschool")) %>%
        drop_na()
      
      gg <- ggplot(cost_data, aes(x = STUDYYEAR, y = Cost, color = COUNTY_NAME, group = COUNTY_NAME)) +
        geom_line(size = 1.1) +
        geom_point(size = 2) +
        scale_color_manual(values = county_colors) +
        labs(
          title = "Weekly Full-Time Median Childcare Cost Trends",
          subtitle = "Aggregated across providers in each county",
          x = "Year", y = "Cost (USD)", color = "County"
        ) +
        theme_minimal(base_family = "Times New Roman")
      
      if (length(unique(cost_data$Type)) > 1) {
        gg <- gg + facet_wrap(~ Type, scales = "free_y")
      } else {
        gg <- gg + ggtitle(paste0("Childcare Cost Trend - ", unique(cost_data$Type)))
      }
      
      ggplotly(gg, tooltip = c("x", "y", "color"))
      
    } else {
      flfpr_data <- county_data %>%
        filter(STUDYYEAR != 2008) %>%
        select(COUNTY_NAME, STUDYYEAR, FLFPR_20to64_UNDER6) %>%
        drop_na()
      
      gg <- ggplot(flfpr_data, aes(x = STUDYYEAR, y = FLFPR_20to64_UNDER6, color = COUNTY_NAME)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = county_colors) +
        labs(
          title = "FLFPR (Under 6) Trends",
          x = "Year", y = "Participation Rate (%)", color = "County"
        ) +
        theme_minimal(base_family = "Times New Roman")
      
      ggplotly(gg, tooltip = c("x", "y", "color"))
    }
  })
}

# Run the App -----------------------------------------------------------
shinyApp(ui, server)
