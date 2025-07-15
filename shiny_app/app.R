# Load Packages ---------------------------------------------------------
library(shiny)
library(tidyverse)
library(readxl)
library(tigris)
library(leaflet)
library(sf)
library(plotly)
library(shinydashboard)
library(timevis)

# VA Data --------------------------------------------------------------------

# Import data
<<<<<<< HEAD:childcare_analysis_app.R
labor_data <- read_excel("Women's Bureau Data Work/WB Data/Source/labor_data.xlsx")
=======
#labor_data <- read_excel("Women's Bureau Data Work/labor_data.xlsx")
>>>>>>> 6746d3230e54c21e762def221d3db875e60a79ab:shiny_app/app.R

# Create VA data
#VA_data <- labor_data %>%
  #filter(STATE_NAME == "Virginia")

# Prepare VA counties spatial data
#va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
 # mutate(FIPS = GEOID) %>%
  #st_transform(crs = 4326)

# Format FIPS codes consistently
#VA_data <- VA_data %>%
  #mutate(COUNTY_FIPS_CODE = str_pad(as.character(COUNTY_FIPS_CODE), width = 5, pad = "0"))

# Join spatial and attribute data
#map_data_all <- va_counties %>%
  #left_join(VA_data, by = c("GEOID" = "COUNTY_FIPS_CODE"))

#Timeline Data ----------------------------------------------------------------
policy_data <- data.frame(
  id = 1:13,
  content = c(
    "2018 Reinbursement Rate Increase", 
    "2020 CCDF Funding Allocation",
    "2020 CCDF Transition Plan",
    "2021 COVID-19 Relief",
    "2021 House Bill 2206",
    "2021 House Bill 1800",
    "2022 House Bill 30",
    "2022 Supplier Correspondence on Copays",
    "2022 Supplier Correspondence on Revisions to Provider Payment Rates",
    "2023 Correspondence on Update in Paid Childcare Absences",
    "2024 HB419",
    "2024 Provider Correspondence on Waitlists Being Reinstated",
    "2024 HB627"
  ),
  start = as.Date(c(
    "2018-06-01",
    "2020-10-01",
    "2020-11-24",
    "2021-02-22",
    "2021-01-28",
    "2021-01-01",
    "2022-01-01",
    "2022-12-06",
    "2022-09-29",
    "2023-09-20",
    "2024-02-07",
    "2024-06-04",
    "2024-01-10"
  )),
  summary = c(
    "In 2018, Virginia increased reimbursement rates for licensed providers to align more closely with the 70th percentile of the market rate survey.",
    "In 2020, a report was released explaining funding allocation within CCDF.",
    "The CCDF was transitioned from the Virginia Department of Social Services (VDSS) to the Virginia Department of Education (VDOE). The plan included a phased shift to bringing all aspects of subsidies to the VDOE. It also brought up the shift to Virginia Quality Inititiavies (VQI) and how it will be implemented in phases, along with impact of COVID-19 on chilcare and updates on supporting CCDF through COVID-19.",
    "In 2021 the CCDF was given funds under the Coronavirus Response and Relief Supplemental appropriations Act (CRRSA). The goals to utilize these funds was to increase the supply of childcare, increase resources for providers and families, expand requirements for subsidies, and to increase access to childcare in underserved communities. Copayments were eliminated and other eligibility requirements would soon be changing as well.",
    "A bill temporarily expanding subsidy program to those who are actively looking for work and raising the qualifying threshhold to those who's income is below 85% of the state median income.",
    "Provided 9.6 Million Dollars to CCDF to expand the subsidy program.",
    "This bill maximized the use of federal CCDF funds to eliminate the waitlist for subsidies. It also continued the 85% of median income eligibility to the 2022-2024 biennium.",
    "This correspondence informed subsidy providers that copays will be reinstated January 1 2023, and brought up that with it would be a new copayment scale.",
    "This correspondence informed subsidy providers that payment rates would be changing to fit a new cost estimation model, giving a majority of centers increased rates. It also mentioned new copayment scales, along with subsidy providers now being paid for 15 days of planned holidays.",
    "This correspondence effectively changed the number of allotted absence days still paid to the provider from 36 to 60 days a year.",
    "This bill amended the codes that handled funding for childcare, teducators indecisive,  and nonreverting funds to capture unspent funding balances.",
    "This correspondence explained that due to need that does not meet supply subsidy waitlists will be reinstated throughout the state.",
    "This bill was to provide subsidies to childcare employees regardless of income status, in hopes of incentivizing working in childcare. It passed, but then died in the house."
  ),
  link = c(
    "2018-06-MR-Survey.qmd.pdf",
    "2020-10-01-RD367.pdf",
    "2020-11-24-CCDF-transition-plan.pdf",
    "2021-02-22-Covid-Relief.pdf",
    "2021-01-28-HB2206.pdf",
    "2021-HB1800.pdf",
    "2022-HB30.pdf",
    "2022-12-6-ProvLet-Copays.pdf",
    "Correspondence-on-revisions-to-provider-payment-rates.pdf",
    "2023-09-20-paidabsenceincrease.pdf",
    "2024-02-07-HB419.pdf",
    "2024-06-04-waitlistvendorsletter.pdf",
    "2024-01-10-subsidysexpandtoCCworkers.pdf"
  ),
  stringsAsFactors = FALSE
)

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
           p("📌 Click a timeline event to view its summary and document."),
           fluidRow(
             column(8, timevisOutput("timeline")),
             column(4, uiOutput("policyDetails"))
           )
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
                 #leafletOutput("childcareMap", height = "600px")
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
#Childcare Map
server <- function(input, output, session) {
  #filtered_data <- reactive({
   # cost_col <- paste0(input$percentile, input$provider, input$age_group)
   # df <- map_data_all %>%
    #  filter(STUDYYEAR == input$STUDYYEAR) %>%
     # mutate(cost_pct_income = (.data[[cost_col]] * 52) / MFI) %>%
     # filter(!is.na(cost_pct_income), is.finite(cost_pct_income), cost_pct_income > 0)
   # df
 # })
  
<<<<<<< HEAD:childcare_analysis_app.R
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

#Timeline
# --------- Timeline Logic ----------
output$timeline <- renderTimevis({
  timevis(policy_data)
})

output$policyDetails <- renderUI({
  selected_id <- input$timeline_selected
  
  if (!is.null(selected_id)) {
    selected_row <- policy_data[policy_data$id == selected_id, ]
    
    tagList(
      h4(selected_row$content),
      p(selected_row$summary),
      tags$a(
        href = selected_row$link,
        target = "_blank",
        "📄 View Document",
        style = "color: blue; text-decoration: underline; cursor: pointer;"
      )
    )
  } else {
    h5("📌 Click a timeline event to view its summary and document.")
  }
})
=======
  #output$childcareMap <- renderLeaflet({
   # df <- filtered_data()
   # pal <- colorNumeric("viridis", df$cost_pct_income, na.color = "gray90")
    #leaflet(df) %>%
     # addProviderTiles(providers$CartoDB.Positron) %>%
     # addPolygons(
     #   fillColor = ~pal(cost_pct_income),
      #  weight = 1,
      #  color = "white",
      #  fillOpacity = 0.7,
       # label = ~paste0(NAME, ": ", scales::percent(cost_pct_income, accuracy = 1)),
       # labelOptions = labelOptions(
        #  style = list("font-weight" = "normal", padding = "3px 8px"),
        #  textsize = "15px",
        #  direction = "auto"
      #  )
     # ) %>%
     # addLegend(
      #  pal = pal,
      #  values = df$cost_pct_income,
      #  title = "Cost as % of Income",
      #  labFormat = labelFormat(suffix = "%", transform = function(x) 100 * x)
     # )
 # })
>>>>>>> 6746d3230e54c21e762def221d3db875e60a79ab:shiny_app/app.R
}
observe({  print(input$timeline_selected)
})



# Run App -----------------------------------------------------------------

shinyApp(ui = ui, server = server)


