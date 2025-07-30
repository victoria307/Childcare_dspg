cat("\014")
rm(list=ls())


# Load Required Packages ------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(leaflet)
library(sf)
library(tigris)
library(RColorBrewer)
library(plotly)          # <--- for interactive hover tooltips in plots
library(scales)          # for nicer number formatting
options(tigris_use_cache = TRUE)

# Utility formatters ----------------------------------------------------
fmt_count <- label_comma(accuracy = 0.1)           # e.g., 1,234.5
fmt_dollar <- label_dollar(accuracy = 1)           # e.g., $1,234

# Load and Prepare Data -------------------------------------------------
childcare_data <- read_excel("Interactive_Elements/Facilities_Count_And_Expenditure.xlsx") %>%
  mutate(
    year = as.integer(year),
    Avg_Total_Count = as.numeric(Avg_Total_Count),
    Avg_Expenditures = as.numeric(Avg_Expenditures)
  )

# Load FIPS codes for Virginia counties ---------------------------------
va_fips <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  st_drop_geometry() %>%
  select(NAME, GEOID) %>%
  rename(Locality = NAME, FIPS = GEOID) %>%
  mutate(FIPS = as.character(FIPS))

# Join FIPS to data -----------------------------------------------------
childcare_data <- childcare_data %>%
  mutate(Locality = str_trim(Locality)) %>%
  left_join(va_fips, by = "Locality") %>%
  mutate(FIPS = as.character(FIPS))

# Load county shapes with FIPS ------------------------------------------
va_shape <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  mutate(FIPS = as.character(GEOID))

# Define a function to build the bins for map legends -------------------
map_bins_fn <- function(varname) {
  if (varname == "Avg_Expenditures") {
    c(0, 2500, 5000, 7500, 10000, 15000, Inf)
  } else {
    c(0, 10, 20, 30, 40, 50, Inf)
  }
}

# Custom map color palette (excluding the lightest yellow) --------------
map_palette <- RColorBrewer::brewer.pal(7, "YlGnBu")[2:7]

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
        font-size: 38px;
        font-weight: bold;
        text-align: center;
        margin-bottom: 10px;
      }
    "))
  ),
  
  h2("Virginia Childcare Cost and Total Count Map"),
  
  sidebarLayout(
    sidebarPanel(
      style = "background-color: #d3ebf5;",
      conditionalPanel(
        condition = "input.tabs == 'year_view'",
        selectInput("year", "Select Year:", choices = sort(unique(childcare_data$year)), selected = max(childcare_data$year)),
        selectInput("data_type", "Select Data Type:",
                    choices = c("Average Total Count" = "Avg_Total_Count",
                                "Average Expenditures (USD)" = "Avg_Expenditures"))
      ),
      conditionalPanel(
        condition = "input.tabs == 'trend_view'",
        selectInput("trend_var", "Select a metric to show in time trend:",
                    choices = c("Total Count" = "Avg_Total_Count",
                                "Expenditures (USD)" = "Avg_Expenditures"),
                    selected = "Avg_Total_Count")
        
      ),
      selectizeInput("county_picker", "Select Counties:", 
                     choices = sort(unique(childcare_data$Locality)), 
                     multiple = TRUE),
      actionButton("reset_counties", "Clear Selected Counties", class = "btn btn-warning")
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel(title = "Year-Specific View", value = "year_view",
                           leafletOutput("map", height = "600px"),
                           br(),
                           h4("Selected County Values for Selected Year", style = "font-family:'Times New Roman';"),
                           plotlyOutput("year_specific_plot", height = "400px")   # <- plotly
                  ),
                  tabPanel(title = "Time Trend View", value = "trend_view",
                           leafletOutput("trend_map", height = "600px"),
                           h4("Trends Over Time for Selected Counties", style = "font-family:'Times New Roman';"),
                           plotlyOutput("multi_county_plot", height = "400px")   # <- plotly
                  )
      )
    )
  )
)

# Server ----------------------------------------------------------------
server <- function(input, output, session) {
  
  # Store selected counties in a reactiveVal ---------------------------------
  selected_counties <- reactiveVal(character(0))
  
  # Reactive for valid county choices based on non-NA data -------------------
  valid_county_choices <- reactive({
    req(input$year, input$data_type)
    
    # Filter for the selected year and non-NA values
    valid_locs <- childcare_data %>%
      filter(year == input$year) %>%
      filter(!is.na(.data[[input$data_type]])) %>%
      pull(Locality) %>%
      unique()
    
    # Return only those counties that exist in the shape file as well
    intersect(valid_locs, va_shape$NAME) %>% sort()
  })
  
  
  # Observe to update county picker based on valid counties ------------------
  observe({
    req(input$year, input$data_type)
    
    valid_choices <- valid_county_choices()
    print(valid_choices)  # for debugging
    
    new_selected <- intersect(selected_counties(), valid_choices)
    selected_counties(new_selected)
    updateSelectizeInput(
      session,
      "county_picker",
      choices = valid_choices,
      selected = new_selected
    )
  })
  
  
  
  # Map click (Year-Specific View) --------------------------------------------
  observeEvent(input$map_shape_click, {
    clicked_fips <- input$map_shape_click$id
    clicked_name <- childcare_data %>%
      filter(FIPS == clicked_fips) %>%
      pull(Locality) %>%
      unique() %>%
      first()
    
    if (!is.null(clicked_name)) {
      current <- selected_counties()
      updated <- if (clicked_name %in% current) {
        setdiff(current, clicked_name)
      } else {
        union(current, clicked_name)
      }
      selected_counties(updated)
      updateSelectizeInput(session, "county_picker", selected = updated)
    }
  })
  
  # Map click (Trend View) ----------------------------------------------------
  observeEvent(input$trend_map_shape_click, {
    clicked_fips <- input$trend_map_shape_click$id
    clicked_name <- childcare_data %>%
      filter(FIPS == clicked_fips) %>%
      pull(Locality) %>%
      unique() %>%
      first()
    
    if (!is.null(clicked_name)) {
      current <- selected_counties()
      updated <- if (clicked_name %in% current) {
        setdiff(current, clicked_name)
      } else {
        union(current, clicked_name)
      }
      selected_counties(updated)
      updateSelectizeInput(session, "county_picker", selected = updated)
    }
  })
  
  # Sync picker -> selected_counties ------------------------------------------
  # Sync picker -> selected_counties
  observeEvent(input$county_picker, {
    selected_counties(input$county_picker)
  })
  
  # ADD THIS: Dynamically update choices to exclude counties with NA values
  observe({
    valid_choices <- valid_county_choices()
    new_selected <- intersect(selected_counties(), valid_choices)
    selected_counties(new_selected)  # update the reactiveVal
    updateSelectizeInput(
      session,
      "county_picker",
      choices = valid_choices,
      selected = new_selected
    )
  })
  
  
  # Year-filtered data ---------------------------------------------------------
  filtered_data <- reactive({
    validate(need(!is.null(input$year), "Please choose a year."))
    childcare_data %>% filter(year == input$year)
  })
  
  # Join shapes + data for the year map ---------------------------------------
  merged_data <- reactive({
    left_join(va_shape, filtered_data(), by = "FIPS")
  })
  
  # Shared label builder for leaflet polygons ----------------------------------
  poly_label <- function(df, varname) {
    val_fmt <- if (varname == "Avg_Expenditures") fmt_dollar else fmt_count
    paste0(
      "<strong>", df$NAME, "</strong><br/>",
      ifelse(varname == "Avg_Expenditures", "Avg Expenditures: ", "Avg Facility Count: "),
      ifelse(is.na(df[[varname]]), "NA", val_fmt(df[[varname]]))
    ) %>% lapply(htmltools::HTML)
  }
  
  # Year-Specific Map ----------------------------------------------------------
  output$map <- renderLeaflet({
    req(input$data_type)
    df <- merged_data()
    var <- input$data_type
    
    bins <- map_bins_fn(var)
    pal <- colorBin(palette = map_palette, domain = df[[var]], bins = bins, na.color = "gray")
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId = ~FIPS,
        fillColor = ~pal(get(var)),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = poly_label(df, var),
        labelOptions = labelOptions(direction = "auto"),
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9)
      ) %>%
      addLegend("bottomright", pal = pal, values = df[[var]], title = ifelse(var == "Avg_Expenditures", "Avg Expenditures", "Avg Facility Count"), opacity = 1)
  })
  
  # Trend Map (averaged across all years) --------------------------------------
  output$trend_map <- renderLeaflet({
    req(input$trend_var)
    var <- input$trend_var
    
    summary_df <- childcare_data %>%
      group_by(FIPS) %>%
      summarise(
        Avg_Total_Count = mean(Avg_Total_Count, na.rm = TRUE),
        Avg_Expenditures = mean(Avg_Expenditures, na.rm = TRUE),
        .groups = "drop"
      )
    
    df <- left_join(va_shape, summary_df, by = "FIPS")
    bins <- map_bins_fn(var)
    pal <- colorBin(palette = map_palette, domain = df[[var]], bins = bins, na.color = "gray")
    
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        layerId = ~FIPS,
        fillColor = ~pal(get(var)),
        weight = 1,
        color = "white",
        fillOpacity = 0.7,
        label = poly_label(df, var),
        labelOptions = labelOptions(direction = "auto"),
        highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9)
      ) %>%
      addLegend("bottomright", pal = pal, values = df[[var]],
                title = ifelse(var == "Avg_Expenditures", "Avg Expenditures", "Avg Facility Count"), opacity = 1)
  })
  
  # Year-Specific Bar Plot (plotly) --------------------------------------------
  output$year_specific_plot <- renderPlotly({
    locs <- selected_counties()
    req(input$data_type)
    if (length(locs) == 0) return(NULL)
    
    df <- childcare_data %>%
      filter(year == input$year, Locality %in% locs)
    
    if (input$data_type == "Avg_Total_Count") {
      df <- df %>% mutate(hover_txt = paste0(Locality, "<br>Facility Count: ", fmt_count(Avg_Total_Count)))
      y_lab <- "Facility Count"
    } else {
      df <- df %>% mutate(hover_txt = paste0(Locality, "<br>Expenditures: ", fmt_dollar(Avg_Expenditures)))
      y_lab <- "Expenditures (USD)"
    }
    
    g <- ggplot(df, aes(x = reorder(Locality, .data[[input$data_type]]), 
                        y = .data[[input$data_type]], 
                        fill = Locality,
                        text = hover_txt)) +
      geom_col() +
      labs(
        title = paste("Selected County", ifelse(input$data_type == "Avg_Total_Count", "Facility Count", "Expenditures"), "in", input$year),
        x = "County",
        y = y_lab
      ) +
      scale_fill_manual(values = rep(map_palette, length.out = length(unique(df$Locality)))) +
      theme_minimal(base_family = "Times New Roman") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(g, tooltip = "text")
  })
  
  # Multi-County Time Trend Plot (plotly) --------------------------------------
  output$multi_county_plot <- renderPlotly({
    locs <- selected_counties()
    req(input$trend_var)
    if (length(locs) == 0) return(NULL)
    
    plot_data <- childcare_data %>% filter(Locality %in% locs)
    
    plot_long <- plot_data %>%
      select(Locality, year, Avg_Total_Count, Avg_Expenditures) %>%
      pivot_longer(cols = c("Avg_Total_Count", "Avg_Expenditures"),
                   names_to = "Metric", values_to = "Value") %>%
      filter(Metric == input$trend_var) %>%
      mutate(
        Metric_label = case_when(
          Metric == "Avg_Total_Count" ~ "Facility Count",
          Metric == "Avg_Expenditures" ~ "Expenditures (USD)"
        ),
        hover_txt = case_when(
          Metric == "Avg_Total_Count" ~ paste0(Locality, "<br>Year: ", year, "<br>Facility Count: ", fmt_count(Value)),
          Metric == "Avg_Expenditures" ~ paste0(Locality, "<br>Year: ", year, "<br>Expenditures: ", fmt_dollar(Value))
        )
      )
    
    g <- ggplot(plot_long, aes(x = year, y = Value, color = Locality, group = Locality, text = hover_txt)) +
      geom_line(size = 1.2, na.rm = TRUE) +
      geom_point(size = 2, na.rm = TRUE) +
      labs(
        title = paste("Trend of", unique(plot_long$Metric_label), "by County"),
        x = "Year",
        y = unique(plot_long$Metric_label),
        color = "County"
      ) +
      scale_color_manual(values = rep(map_palette, length.out = length(unique(plot_long$Locality)))) +
      theme_minimal(base_family = "Times New Roman")
    
    ggplotly(g, tooltip = "text") %>% layout(legend = list(orientation = "h", x = 0, y = -0.2))
  })
}


# Run App ---------------------------------------------------------------
shinyApp(ui, server)

