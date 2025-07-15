rm(list=ls())
cat("\014")

library(pdftools)
library(dplyr)
library(stringr)

# Step 1: Read pages 30–32 from the PDF
file_path <- "Data/CCDF MArket Rate Survey Virginia 2018.pdf"
pages <- pdf_text(file_path)[30:32]

# Step 2: Split into lines and trim
lines <- unlist(strsplit(pages, "\n")) %>% 
  str_trim() %>% 
  .[. != ""]

# Step 3: Filter lines that begin with FIPS code and contain 8 numbers
data_lines <- lines[str_detect(lines, "^\\d{3}\\s") & str_count(lines, "\\d{2,3}") >= 8]

# Step 4: Use regex to extract FIPS, Locality, and 8 numeric fields
pattern <- "^ *(\\d{3})\\s+([A-Za-z\\-/ &'\\.]+?)\\s+(\\d{2,3})\\s+(\\d{2,3})\\s+(\\d{2,3})\\s+(\\d{2,3})\\s+(\\d{2,3})\\s+(\\d{2,3})\\s+(\\d{2,3})\\s+(\\d{2,3})$"
parsed <- str_match(data_lines, pattern)

# Step 5: Build dataframe
final_data <- data.frame(
  FIPS             = parsed[, 2],
  Locality         = parsed[, 3],
  Center_Infant    = as.numeric(parsed[, 4]),
  Center_Toddler   = as.numeric(parsed[, 5]),
  Center_Preschool = as.numeric(parsed[, 6]),
  Center_SchoolAge = as.numeric(parsed[, 7]),
  Family_Infant    = as.numeric(parsed[, 8]),
  Family_Toddler   = as.numeric(parsed[, 9]),
  Family_Preschool = as.numeric(parsed[,10]),
  Family_SchoolAge = as.numeric(parsed[,11]),
  stringsAsFactors = FALSE
)

# Step 6: View result
head(final_data)

# Optional: Save to CSV
write.csv(final_data, "Data/SubsidyRates.csv")
