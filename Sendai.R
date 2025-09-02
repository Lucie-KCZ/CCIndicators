#########################################
###   Sendai Framework focused data   ###
#########################################

# luciek@spc.int
# created: July 2025
# last edit: 09/07/2025

# Remove all objects from the current R session (start with a clean workspace)
rm(list = ls())

# Packages ----------------------------------------------------------------
# The next section loads R packages.
library(readxl)

# User settings -----------------------------------------------------------
# The following section defines all user-specific settings and parameters.
# Update file paths, sheet names, clustering thresholds, and review tags as needed.
# Set the working directory for file read/write operations
setwd("C:/Users/luciek/My Documents/Knowledge/Indicators/Analysis/")

# File and Data Column Settings
# Path to the Excel file containing your questions
folder_path <- "C:/Users/luciek/Documents/Knowledge/Indicators/Analysis"
file_path <- paste0(folder_path, "/202506_clean_clusters.csv")

# Load the cleaned indicator dataset and keep only rows marked for SDG processing
indicators <- read.csv(file_path)
indicators <- indicators[indicators$Sendai, ]

# Remove extra framework columns
nb_frameworks <- apply(indicators[, 8:32], 1, sum)
indicators <- indicators[, -c(8:32)]
indicators$nb_frameworks <- nb_frameworks

# Add data sources --------------------------------------------------------
# Read the raw indicator file and clean up empty strings
raw_data <- read.csv(paste0(folder_path, '/202505_indicators.csv'))
raw_data$Data.sources[ raw_data$Data.sources == "" ] <- NA

# Normalize "% of" → " percent of" in your raw indicator text
raw_data$Indicator <- gsub("%\\s*of", " percent of", raw_data$Indicator, perl = TRUE)
raw_data$Indicator <- gsub("%", " percent", raw_data$Indicator, fixed = TRUE)

# Keep only rows where there is at least one data need or data source
data_sources <- raw_data[ ! (is.na(raw_data$Data.needs) & is.na(raw_data$Data.sources)), ]

results <- indicators
# Loop row by row through those with data_sources
for (i in seq_len(nrow(data_sources))) {
  this_indicator <- data_sources$Indicator[i]
  
  # Find all rows in results whose Details contain the exact indicator text
  # (use fixed=TRUE to avoid regex surprises)
  row_i <- grep(this_indicator, results$Details, fixed = TRUE)
  
  # If we found at least one match, update Data_sources
  if (length(row_i) > 0) {
    for (r in row_i) {
      if (is.na(results[r, "Data_sources"])) {
        # First time: just insert
        results[r, "Data_sources"] <- data_sources$Data.sources[i]
      } else {
        # Append with semicolon
        results[r, "Data_sources"] <- paste(
          results[r, "Data_sources"],
          data_sources$Data.sources[i],
          sep = "; "
        )
      }
    }
  }
}

# (Optionally) write back to CSV
write.csv(results, paste0("./Sendai/", format(Sys.time(), "%Y%m"), "_Sendai_sources.csv"), row.names = FALSE)

