rm(list = ls())
cat("\014")

library(pdftools)
library(stringr)
library(dplyr)

# Step 1: Load pages 12–14 from the PDF
file_path <- "Data/2020CCDFReport.pdf"
pages <- pdf_text(file_path)[12:14]

# Step 2: Clean up lines
lines <- unlist(strsplit(pages, "\n")) %>% 
  str_trim() %>% 
  .[. != ""]

# Step 3: Keep only lines that look like data rows (start with 3-digit FIPS and have 4 numbers at the end)
data_lines <- lines[str_detect(lines, "^\\d{3}\\s") & str_count(lines, "\\d{1,3}\\s{2,}") >= 4]

# Step 4: Parse lines — use flexible regex to extract fields
parse_line <- function(line) {
  # Extract 4 projected values at the end
  values <- str_extract_all(line, "\\d{1,3}")[[1]]
  if (length(values) < 5) return(NULL)
  
  fips <- values[1]
  sfy20 <- values[2]
  sfy21 <- values[3]
  sfy22 <- values[4]
  sfy23 <- values[5]
  
  # Remove FIPS and numbers from line to get locality name
  locality <- str_remove(line, paste0("^", fips, "\\s+"))
  locality <- str_remove(locality, paste(values[2:5], collapse = "\\s+"))
  locality <- str_trim(locality)
  
  return(c(fips, locality, sfy20, sfy21, sfy22, sfy23))
}

# Apply parser
parsed_data <- lapply(data_lines, parse_line)
parsed_data <- do.call(rbind, parsed_data) |> as.data.frame(stringsAsFactors = FALSE)

# Step 5: Rename and convert
colnames(parsed_data) <- c("FIPS", "Locality", "SFY20", "SFY21", "SFY22", "SFY23")
parsed_data <- parsed_data %>%
  mutate(across(SFY20:SFY23, as.integer),
         FIPS = str_pad(FIPS, width = 3, pad = "0"))

# View result
head(parsed_data, 10)

write.csv(parsed_data, "Data/SubsidyProvderCounts.csv")
