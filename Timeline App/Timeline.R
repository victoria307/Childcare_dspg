#######Timeline
#install packages
library(tidyverse)
library(shiny)
library(shinydashboard)
library(timevis)

#Create Shiny
# Load required packages
library(shiny)
library(timevis)


setwd("/Users/gracemullins/DPSG/DPSG dir/")
# Sample data: policies with summaries and document links
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
    "2022-09-29-Correspondence-on-revisions-to-provider-payment-rates.pdf",
    "2023-09-20-paidabsenceincrease.pdf",
    "2024-02-07-HB419.pdf",
    "2024-06-04-waitlistvendorsletter.pdf",
    "2024-01-10-subsidysexpandtoCCworkers.pdf"
  ),
  stringsAsFactors = FALSE
)



# UI layout
ui <- fluidPage(
  titlePanel("Virginia Childcare Policy Timeline"),
  fluidRow(
    column(8, timevisOutput("timeline")),
    column(4, uiOutput("policyDetails"))
  )
)

# Server logic
policy_data$start <- as.character(policy_data$start)

server <- function(input, output, session) {
  
  output$timeline <- renderTimevis({
    timevis(policy_data)
  })
  
  # Render the interactive timeline
  output$policyDetails <- renderUI({
    selected_id <- input$timeline_selected
    
    if (!is.null(selected_id)) {
      selected_row <- policy_data[policy_data$id == selected_id, ]
      
      print(paste("Selected link:", selected_row$link))  # Debugging
      
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
}
  
# Run the app
shinyApp(ui = ui, server = server) 


