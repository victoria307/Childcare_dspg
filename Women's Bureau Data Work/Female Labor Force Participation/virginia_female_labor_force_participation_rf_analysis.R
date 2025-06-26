library(readr)
library(dplyr)
library(randomForest)
library(caret)
library(readxl)

# Load data
data <- read_excel("Women's Bureau Data Work/Female Labor Force Participation/Virginia_Model_Data.xlsx")

# Clean and select
model_data <- data %>%
  select(
    femr_20to64,
    mcinfant, mfccinfant, mctoddler, mcpreschool, 
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


rf_model <- randomForest(
  femr_20to64 ~ .,
  data = train_data,
  importance = TRUE,
  ntree = 500
)

# Predict and evaluate
predictions <- predict(rf_model, test_data)
rmse <- sqrt(mean((predictions - test_data$femr_20to64)^2))
cat("RMSE:", rmse, "\n")

# Plot feature importance
varImpPlot(rf_model)

# Get variable importance values
importance_values <- importance(rf_model)

# Create a tidy data frame of importance results
importance_df <- as.data.frame(importance_values)
importance_df$Variable <- rownames(importance_df)

# Sort by %IncMSE (most informative variables first)
importance_sorted <- importance_df %>%
  arrange(desc(`%IncMSE`))

# View the sorted importance table
print(importance_sorted)

library(ggplot2)


labels <- c(
  "mcinfant" = "Infant Center Cost",
  "mfccinfant" = "Infant Family Childcare Cost",
  "mctoddler" = "Toddler Center Cost",
  "mcpreschool" = "Preschool Center Cost",
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

# Plot
ggplot(importance_sorted, aes(x = reorder(Clean_Var, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "What Drives Female Labor Force Participation in Virginia?",
    subtitle = "Variable importance from Random Forest model (Higher = More Predictive)",
    x = "Factor",
    y = "% Increase in Error When Removed"
  ) +
  theme_minimal(base_size = 13)

