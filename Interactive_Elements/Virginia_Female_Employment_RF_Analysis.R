# Load libraries
library(shiny)
library(readr)
library(dplyr)
library(randomForest)
library(caret)
library(readxl)
library(plotly)
library(viridisLite)

# Load and preprocess data
data <- read_excel("c:/Users/waiy/OneDrive - Berea College/Documents/Childcare_draft/Childcare and Female Laborforce/Virginia_Model_Data.xlsx")

model_data <- data %>%
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

# Train-test split
set.seed(123)
train_index <- createDataPartition(model_data$femr_20to64, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train Random Forest model
rf_model <- randomForest(
  femr_20to64 ~ .,
  data = train_data,
  importance = TRUE,
  ntree = 500
)

# Predict and evaluate
predictions <- predict(rf_model, test_data)
rmse <- sqrt(mean((predictions - test_data$femr_20to64)^2))

# Get and format importance
importance_values <- importance(rf_model)
importance_df <- as.data.frame(importance_values)
importance_df$Variable <- rownames(importance_df)

importance_sorted <- importance_df %>%
  arrange(desc(`%IncMSE`)) %>%
  rename(
    Increase_in_Error_Percent = `%IncMSE`,
    Node_Purity_Contribution = IncNodePurity
  )

# Add readable variable names
labels <- c(
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
importance_sorted$Clean_Var <- labels[importance_sorted$Variable]

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
      data = importance_sorted,
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
