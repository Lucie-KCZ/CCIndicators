########################################################
###     Long format and clean output to be shared    ###
########################################################

# luciek@spc.int
# created: June 2025
# last edit: 20/06/2025

# Clear the current R environment
rm(list = ls())

# User settings -----------------------------------------------------------
# Set working directory where your files are located
setwd("C:/Users/luciek/My Documents/Knowledge/Indicators/Analysis/")

# Load the cleaned and clustered indicator dataset
folder_path <- "C:/Users/luciek/Documents/Knowledge/Indicators/Analysis"
file_path <- paste0(folder_path, "/062025_clean_clusters.csv")

# Functions ---------------------------------------------------------------
# Helper to clean and split strings on semicolons
split_and_trim <- function(x) {
  if (is.na(x) || trimws(x) == "") return(character(0))
  trimws(unlist(strsplit(as.character(x), ";", fixed = TRUE)))
}

# Main function to convert to long format per topic
long_format <- function(df, ref, topic) {
  cat(sprintf("Preparing topic: %s\n", topic))
  
  # Filter rows where topic name is found in the Topic column
  df_topic <- df[grep(topic, df$Topic), ]
  df_topic_out <- NULL
  
  # Process each row individually
  for (i in seq_len(nrow(df_topic))) {
    row_i <- df_topic[i, c("Subtopic", "Global_Indicator", "Details", "Nb_frameworks")]
    
    # Extract subtopics and detail indicators as lists
    details_i  <- split_and_trim(row_i$Details)
    subtopics_i <- split_and_trim(row_i$Subtopic)
    
    # Retain only subtopics that match the current topic in the reference table
    allowed_subtopics <- ref[ref$Climate.change.topic == topic, "Subtopic"]
    subtopics_i <- subtopics_i[subtopics_i %in% allowed_subtopics]
    
    # If only one detail, retain row with collapsed subtopics
    if (length(details_i) <= 1) {
      df_topic_out <- rbind(
        df_topic_out,
        data.frame(Topic = topic,
                   Subtopic = paste(subtopics_i, collapse = "; "),
                   Global_Indicator = row_i$Global_Indicator,
                   Details = paste(details_i, collapse = "; "),
                   Nb_frameworks = row_i$Nb_frameworks,
                   stringsAsFactors = FALSE)
      )
    } else {
      # If multiple detail indicators, expand into multiple rows
      details_df <- data.frame(Global_Indicator = row_i$Global_Indicator,
                               Details = details_i,
                               stringsAsFactors = FALSE)
      
      # Combine each detail with each relevant subtopic
      for (sub in subtopics_i) {
        expanded_rows <- data.frame(
          Topic = topic, 
          Subtopic = sub,
          details_df, 
          Nb_frameworks = row_i$Nb_frameworks,
          stringsAsFactors = FALSE
        )
        df_topic_out <- rbind(df_topic_out, expanded_rows)
      }
    }
  }
  
  # Ensure unique and ordered output
  df_topic_out <- unique(df_topic_out[order(df_topic_out$Subtopic, 
                                            df_topic_out$Global_Indicator, 
                                            df_topic_out$Nb_frameworks), ])
  return(df_topic_out)
}

# Main --------------------------------------------------------------------
indicators <- read.csv(file_path)

# Drop unused column and count how many frameworks are used per indicator
indicators$ClusterID <- NULL
indicators$Nb_frameworks <- apply(indicators[, 7:31], 1, sum)  # Count of frameworks where indicator appears
indicators[, 7:31] <- NULL  # Remove original framework columns

# Clean and standardise the 'Subtopic' column (fix typos and formatting)
indicators$Subtopic <- tolower(indicators$Subtopic)
indicators$Subtopic <- gsub("cc", "climate change", indicators$Subtopic)
indicators$Subtopic <- gsub("ghg", "greenhouse gases", indicators$Subtopic)
indicators$Subtopic <- gsub("eduction", "education", indicators$Subtopic)
indicators$Subtopic <- gsub("emisions", "emissions", indicators$Subtopic)
indicators$Subtopic <- gsub("areas affected", "areas impacted", indicators$Subtopic)

# Load and clean reference table mapping subtopics to topics
ref <- unique(read.csv('./052025_indicators.csv')[, 7:8])
ref$Subtopic <- tolower(ref$Subtopic)
ref$Subtopic <- gsub("cc", "climate change", ref$Subtopic)
ref$Subtopic <- gsub("ghg", "greenhouse gases", ref$Subtopic)
ref$Subtopic <- gsub("eduction", "education", ref$Subtopic)
ref$Subtopic <- gsub("emisions", "emissions", ref$Subtopic)
ref$Subtopic <- gsub("areas affected", "areas impacted", ref$Subtopic)
ref <- unique(ref)  # Ensure no duplicate rows



# Loop through each topic and save corresponding expanded file
for (i in unique(ref$Climate.change.topic)) {
  write.csv(x = long_format(indicators, ref, i), 
            file = paste0("./long_format/", i, ".csv"), 
            row.names = FALSE, na = "")
}
