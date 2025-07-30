# Load libraries
library(shiny)
library(readr)
library(dplyr)
library(randomForest)
library(caret)
library(readxl)
library(plotly)
library(viridisLite)


employment_cost_df <- read_excel("Interactive_Elements/RFModel_Data_For_Factors_On_Female_Employment.xlsx")

selected_model_df <- employment_cost_df %>%
  select(
    femr_20to64,
    mcinfant, mfccinfant, 
    mctoddler, mfcctoddler,
    mcpreschool, mfccpreschool,
    mhi, mfi, pr_f, pr_p,
    onerace_w, onerace_b, hispanic,
    h_under6_singlem, h_under6_bothwork
  ) %>%
  na.omit()


set.seed(123)
training_indices <- createDataPartition(selected_model_df$femr_20to64, p = 0.8, list = FALSE)
training_data <- selected_model_df[training_indices, ]
testing_data <- selected_model_df[-training_indices, ]


female_employment_rf <- randomForest(
  femr_20to64 ~ .,
  data = training_data,
  importance = TRUE,
  ntree = 500
)

rf_predictions <- predict(female_employment_rf, testing_data)
rf_rmse <- sqrt(mean((rf_predictions - testing_data$femr_20to64)^2))


rf_importance_raw <- importance(female_employment_rf)
rf_importance_df <- as.data.frame(rf_importance_raw)
rf_importance_df$Variable <- rownames(rf_importance_df)

rf_importance_sorted <- rf_importance_df %>%
  arrange(desc(`%IncMSE`)) %>%
  rename(
    Increase_in_Error_Percent = `%IncMSE`,
    Node_Purity_Contribution = IncNodePurity
  )


variable_labels <- c(
  "mcinfant" = "Infant Center Cost",
  "mfccinfant" = "Infant Family Childcare Cost",
  "mctoddler" = "Toddler Center Cost",
  "mfcctoddler" = "Toddler Family Childcare Cost",
  "mcpreschool" = "Preschool Center Cost",
  "mfccpreschool" = "Preschool Family Childcare Cost",
  "mfi" = "Median Family Income",
  "mhi" = "Median Household Income",
  "pr_f" = "Poverty Rate (Families)",
  "pr_p" = "Poverty Rate (Individuals)",
  "onerace_w" = "White Population %",
  "onerace_b" = "Black Population %",
  "hispanic" = "Hispanic Population %",
  "h_under6_singlem" = "Single Moms w/ Kids <6",
  "h_under6_bothwork" = "Both Parents Working w/ Kids <6"
)
rf_importance_sorted$Clean_Var <- variable_labels[rf_importance_sorted$Variable]

# Define UI
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
      h4 {
        font-family: 'Times New Roman', serif;
        font-size: 20px;
        text-align: center;
        margin-bottom: 30px;
      }
    "))
  ),
  
  h2("What Drives Female Employment in Virginia?"),
  
  h4("Variable importance from Random Forest model (Higher = More Predictive)"),
  
  plotlyOutput("importancePlot", height = "600px")
)

# Define server logic
server <- function(input, output, session) {
  output$importancePlot <- renderPlotly({
    bar_color <- "#2e708c"  # dashboard-matching blue
    
    plot_ly(
      data = rf_importance_sorted,
      x = ~Increase_in_Error_Percent,
      y = ~reorder(Clean_Var, Increase_in_Error_Percent),
      type = 'bar',
      orientation = 'h',
      text = ~paste0(Clean_Var, "<br>Importance: ", round(Increase_in_Error_Percent, 2), "%"),
      textposition = "none",
      hoverinfo = 'text',
      marker = list(color = bar_color)
    ) %>%
      layout(
        title = list(text = "Variable Importance", font = list(family = "Times New Roman")),
        xaxis = list(
          title = "% Increase in Error When Removed",
          tickfont = list(family = "Times New Roman"),
          titlefont = list(family = "Times New Roman")
        ),
        yaxis = list(title = "", tickfont = list(family = "Times New Roman")),
        margin = list(l = 150),
        font = list(family = "Times New Roman"),
        showlegend = FALSE
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
