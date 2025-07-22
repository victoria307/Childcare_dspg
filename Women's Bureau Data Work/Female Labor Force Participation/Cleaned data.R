library(dplyr)
library(readr)

# File path
file_path <- "C:/Users/waiy/OneDrive - Berea College/Desktop/Childcare_dspg/Women's Bureau Data Work/Female Labor Force Participation/CE by locality month_2025-07-03_VA Tech(OneDrive_2025-04-102025-07-03).csv"

# Read the file
data <- read_csv(file_path)

# Convert 'Total Count' to numeric (safely handle any non-numeric issues)
data <- data %>%
  mutate(`Total Count` = as.numeric(`Total Count`))

# Now group and summarize
summary <- data %>%
  group_by(Locality, year) %>%
  summarise(
    Avg_Total_Count = mean(`Total Count`, na.rm = TRUE),
    Avg_Expenditures = mean(`Total Expenditures`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Locality, year)

# Print the summary
print(summary)
library(writexl)

# Write the summary to an Excel file
write_xlsx(summary, "C:/Users/waiy/Desktop/summary_output.xlsx")
# Load the library
library(writexl)

# Define the full path using forward slashes (best for Windows paths in R)
output_path <- "C:/Users/waiy/OneDrive - Berea College/Desktop/Childcare_dspg/Women's Bureau Data Work/Female Labor Force Participation/summary_output.xlsx"

# Write to Excel
write_xlsx(summary, output_path)
