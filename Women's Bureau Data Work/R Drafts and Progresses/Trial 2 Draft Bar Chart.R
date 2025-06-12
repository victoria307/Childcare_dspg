library(shiny)
library(dplyr)
library(ggplot2)
library(readxl)
library(tidyr)
library(scales)  #for formatting dollar 

 
virginia_data <- read_excel("C:/Users/waiy/OneDrive - Berea College/Documents/Childcare_draft/Virginia_Only.xlsx")

# To make sure the YEAR column is numeric
virginia_data <- virginia_data %>%
  mutate(YEAR = as.numeric(STUDYYEAR))


# UI ----------------------------------------------------------------------


ui <- fluidPage(
  titlePanel("Virginia County Child Care Cost vs. Median Family Income"),
  
  #layout for the sidebar
  sidebarLayout(
    sidebarPanel(
      #Dropdown option to select year
      selectInput(
        inputId = "selected_year",
        label = "Select year:",
        choices = sort(unique(virginia_data$STUDYYEAR)),
        selected = max(virginia_data$STUDYYEAR)      #Shows the most recent year as default
      ),
      selectInput(
        inputId = "selected_counties",
        label = "Select counties to compare:",
        choices = sort(unique(virginia_data$COUNTY_NAME)),
        selected = NULL,
        multiple = TRUE
      )
    ),
    #Main panel for the plt and the data table 
    mainPanel(
      plotOutput("comparison_plot"),         # Bar Chart
      tableOutput("comparison_table")       # Table for Data
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$selected_counties, input$selected_year)
    virginia_data %>%
      filter(
        COUNTY_NAME %in% input$selected_counties,
        YEAR == input$selected_year
      ) %>%
      mutate(MFCCPRESCHOOL = MFCCPRESCHOOL * 52)  # Convert weekly to yearly
  })
  
  output$comparison_plot <- renderPlot({
    data <- filtered_data()
    
    bar_data <- data %>%
      select(COUNTY_NAME, MFCCPRESCHOOL, MFI) %>%
      pivot_longer(cols = c(MFCCPRESCHOOL, MFI),
                   names_to = "Metric", values_to = "Value")
    
    ggplot(bar_data, aes(x = COUNTY_NAME, y = Value, fill = Metric)) +
      geom_col(position = "dodge") +scale_fill_manual(values = c("MFCCPRESCHOOL" = "red", "MFI" = "#ff7f0e"))+
      labs(
        title = paste("Child Care Cost and Median Family Income (", input$selected_year, ")", sep = ""),
        x = "County",
        y = "Value (Yearly Child Care Cost or Median Family Income in $)"
      ) +
      scale_y_continuous(labels = dollar) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$comparison_table <- renderTable({
    filtered_data() %>%
      select(COUNTY_NAME, MFCCPRESCHOOL, MFI) %>%
      rename(
        "County" = COUNTY_NAME,
        "Yearly Child Care Cost ($)" = MFCCPRESCHOOL,
        "Median Family Income ($)" = MFI
      )
  })
}


shinyApp(ui = ui, server = server)
