############################
###   SDG focused data   ###
############################

# luciek@spc.int
# created: June 2025
# last edit: 22/06/2025

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

# Match MFAT table --------------------------------------------------------
# Load the cleaned indicator dataset and keep only rows marked for SDG processing
indicators <- read.csv(file_path)
indicators <- indicators[indicators$SDG, ]

# Remove extra framework columns
nb_frameworks <- apply(indicators[, 8:32], 1, sum)
indicators <- indicators[, -c(8:32)]
indicators$nb_frameworks <- nb_frameworks

# Read in the MFAT snapshot data
mfat <- as.data.frame(
  readxl::read_excel("../References/MFAT_Snapshot2025.xlsx", sheet = "work")
)

# Pattern to pull out codes like “2.3”, “11.2.a” from our indicator text
pattern_indicators <- "\\b\\d{1,2}(?:\\.\\d{1,2})?(?:\\.[a-z])?\\b"

# Pattern to extract SDG codes from MFAT entries (case-insensitive “SDG” prefix)
pattern_mfat <- "(?i)\\bSDG\\s*(\\d{1,2}(?:\\.\\d{1,2})?(?:\\.[a-z])?)\\b"

# Extract all SDG codes from the MFAT dataset
raw_mfat_matches <- regmatches(
  mfat$EN,
  gregexec(pattern_mfat, mfat$EN, perl = TRUE)
)
mfat_codes <- sapply(raw_mfat_matches, function(m) {
  if (length(m) > 1) tolower(m[2]) else NA
})
all_codes <- na.omit(unique(mfat_codes))

# Keep only those codes whose primary number is 19 or below
lead_num <- as.numeric(sub("\\..*$", "", all_codes))
all_codes <- all_codes[!is.na(lead_num) & lead_num <= 19]

# Prepare an empty container for our results
results <- NULL

# Loop over each indicator row, try to match it to MFAT entries
for (i in seq_len(nrow(indicators))) {
  txt <- indicators$Details[i]
  
  # Find any SDG‐style code in the indicator text
  found <- regmatches(txt, gregexpr(pattern_indicators, txt, perl=TRUE))[[1]]
  codes_here <- intersect(tolower(unique(found)), all_codes)
  
  # If no valid code is found, record a row with NAs for the MFAT fields
  if (length(codes_here) == 0) {
    results <- rbind(
      results,
      cbind(
        indicators[i, , drop = FALSE],
        SDG_code       = NA,  MFAT_indicator = NA,
        data_entry     = NA,  data_api       = NA,
        computation_SPC= NA,  dashboard2050  = NA,
        comments       = NA,
        stringsAsFactors = FALSE
      )
    )
    next
  }
  
  # Use the first code if multiple appear
  code_i <- codes_here[1]
  
  # Find matching MFAT rows that mention that code (with or without “SDG”)
  mfat_hits <- mfat[grepl(
    paste0("\\b(?:SDG\\s*)?", code_i, "\\b"),
    mfat$EN, perl=TRUE, ignore.case=TRUE
  ), ]
  
  # If none of the MFAT entries match, record a single NA row
  if (nrow(mfat_hits) == 0) {
    results <- rbind(
      results,
      cbind(
        indicators[i, , drop = FALSE],
        SDG_code       = code_i,  MFAT_indicator = NA,
        data_entry     = NA,       data_api       = NA,
        computation_SPC= NA,       dashboard2050  = NA,
        comments       = NA,
        stringsAsFactors = FALSE
      )
    )
    next
  }
  
  # Show the user the candidate MFAT entries and let them choose
  cat(sprintf(
    "\nIndicator #%d: \"%s\"\n\nMatched code: %s\nPossible MFAT entries:\n",
    i, txt, code_i
  ))
  for (j in seq_len(nrow(mfat_hits))) {
    cat(sprintf("  [%d] %s\n", j, mfat_hits$EN[j]))
  }
  reply <- readline("Select one or more by index (e.g. 1 or 1,3), or press Enter to skip: ")
  picks <- as.integer(unlist(strsplit(gsub("\\s+","", reply), ",")))
  picks <- picks[picks >= 1 & picks <= nrow(mfat_hits)]
  
  # If the user skips, record a single row with NA MFAT fields
  if (length(picks) == 0) {
    results <- rbind(
      results,
      cbind(
        indicators[i, , drop = FALSE],
        SDG_code       = code_i,  MFAT_indicator = NA,
        data_entry     = NA,       data_api       = NA,
        computation_SPC= NA,       dashboard2050  = NA,
        comments       = NA,
        stringsAsFactors = FALSE
      )
    )
  } else {
    # Otherwise, for each picked MFAT row, write one result row
    for (j in picks) {
      mf <- mfat_hits[j, , drop = FALSE]
      results <- rbind(
        results,
        cbind(
          indicators[i, , drop = FALSE],
          SDG_code       = code_i,
          MFAT_indicator = mf$EN,
          data_entry     = mf$DE,
          data_api       = mf$API,
          computation_SPC= mf$DO,
          dashboard2050  = as.integer(mf$`2050 Dashboard` == 1),
          comments       = paste(
            na.omit(c(mf$COMMENT...12, mf$`comment 2025`)),
            collapse = "; "
          ),
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

# Remove duplicate matches, show total count, and write to CSV
results <- unique(results)
cat(sprintf("\nFinal number of matched rows: %d\n", nrow(results)))
write.csv(results, paste0("./SDG/", format(Sys.time(), "%Y%m"), "_SDG_MFAT.csv"), row.names = FALSE)


# Add data sources --------------------------------------------------------
# # Read in your previously matched SDG/MFAT results
# results <- read.csv('./202506_SDG_MFAT.csv')

# Read the raw indicator file and clean up empty strings
raw_data <- read.csv(paste0(folder_path, '/202505_indicators.csv'))
raw_data$Data.sources[ raw_data$Data.sources == "" ] <- NA

# Normalize "% of" → " percent of" in your raw indicator text
raw_data$Indicator <- gsub("%\\s*of", " percent of", raw_data$Indicator, perl = TRUE)
raw_data$Indicator <- gsub("%", " percent", raw_data$Indicator, fixed = TRUE)

# Keep only rows where there is at least one data need or data source
data_sources <- raw_data[ ! (is.na(raw_data$Data.needs) & is.na(raw_data$Data.sources)), ]

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
write.csv(results, paste0("./SDG/", format(Sys.time(), "%Y%m"), "_SDG_MFAT_sources.csv"), row.names = FALSE)

