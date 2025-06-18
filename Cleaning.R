############################################################
##  Collapse manually-flagged duplicates in cluster table  ##
############################################################

# luciek@spc.int
# created: June 2025
# last edit: 18/06/2025

# Remove all objects from the current R session (start with a clean workspace)
rm(list = ls())

# User settings -----------------------------------------------------------
# The following section defines all user-specific settings and parameters.
# Update file paths, sheet names, and frameworks as needed.
# Set the working directory for file read/write operations
folder_path <- "C:/Users/luciek/Documents/Knowledge/Indicators"
# File and Data Column Settings
# Path to the Excel file containing your questions
file_path   <- file.path(folder_path, "062025_clusters.xlsx")
# Sheet index or name to load (1 = first sheet)
excel_sheet <- 1

# Framework list
frameworks <- c(
  "Blue_pacific", "BRS_convention", "CBD", "FDES", "Global_set", "FGS",
  "HH_survey", "Migratory_fish_convention_Pacific", "Minamata_convention",
  "Montreal_protocol", "Noumea_convention", "PIRT", "Ramsar", "Regional_goal",
  "Rotterdam_convention", "Samoa_pathway", "SDG", "Sendai", "SPREP",
  "Stockholm_convention", "UN_fish", "UNCCD", "UNFCCC",
  "Underwater_cultural_heritage_convention", "Waigani_convention"
)

# Functions ---------------------------------------------------------------
# Collapse a character vector into a single '; '-separated string
collapse <- function(x) paste(unique(na.omit(x)), collapse = "; ")

# Identify if there is at least T in a Boolean vector
any_colwise <- function(df) apply(df, 2L, any, na.rm = TRUE)

# Remove (manually added) duplicates in the indicators
clean_indicators <- function(clustered_data) {
  
  # Identify duplicated global indicators
  duplicated_indicators <- clustered_data$Global_Indicator[duplicated(clustered_data$Global_Indicator)]
  
  if (length(duplicated_indicators) == 0) {
    message("✅ No duplicate Global_Indicator values found – nothing to do.")
    return(clustered_data)
  }
  
  message("🔍 Found ", length(duplicated_indicators), " duplicated indicators…")
  
  # Start result with all *non*-duplicated rows
  result_df <- clustered_data[!clustered_data$Global_Indicator %in% duplicated_indicators, ]
  
  # Collapse each duplicate block into one row
  removed_total <- 0L
  
  for (indicator_i in duplicated_indicators) {
    
    block <- clustered_data[clustered_data$Global_Indicator == indicator_i, ]
    n_rows <- nrow(block)
    
    ## Build the single summarised row
    summary_row <- data.frame(
      ClusterID        = block$ClusterID[1],
      Global_Indicator = indicator_i,
      Topic            = collapse(block$Topic),
      Subtopic         = collapse(block$Subtopic),
      Details          = collapse(block$Details),
      Data_needs       = collapse(block$Data_needs),
      Data_sources     = collapse(block$Data_sources),
      stringsAsFactors = FALSE
    )
    
    ## Add framework flags
    summary_row <- cbind(
      summary_row,
      as.data.frame(t(any_colwise(block[ , frameworks, drop = FALSE])),
                    row.names = NULL, stringsAsFactors = FALSE)
    )
    
    result_df <- rbind(result_df, summary_row)
    
    removed_total <- removed_total + n_rows - 1L
    message("   • ‘", indicator_i, "’ → kept 1 row, removed ", n_rows - 1L)
  }
  
  message("🚮️  Total rows removed: ", removed_total)
  message("📊  Final table has ", nrow(result_df), " rows.")
  
  rownames(result_df) <- NULL
  result_df
}

# Main --------------------------------------------------------------------
# Import the data
processed_df <- as.data.frame(readxl::read_excel(file_path, sheet = excel_sheet))
# Run the function
final_clusters <- clean_indicators(processed_df)

# Enjoy the result
cat("\n🎉 Cleaning complete! Results saved to clean_clusters.csv\n")
# write.csv(final_clusters, file = paste0(format(Sys.time(), "%m%Y"), "_clean_clusters.csv"), row.names = FALSE)


