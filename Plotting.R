############################
#    Plotting Indicators   #
############################

# luciek@spc.int
# created: June 2025
# last edit: 20/06/2025

# Remove all objects from the current R session (start with a clean workspace)
rm(list = ls())

# Packages ----------------------------------------------------------------
# The next section loads required system settings and R packages.
library(ggraph)
library(igraph)
library(RColorBrewer)

# User settings -----------------------------------------------------------
# This section defines all user-specific settings and file paths.
# Update paths as needed to reflect your working environment.
# Set the working directory where input/output files are located.
setwd("C:/Users/luciek/My Documents/Knowledge/Indicators/Analysis/")

# File and Data Column Settings
# Specify path to the CSV file that contains cleaned and clustered indicators.
folder_path <- "C:/Users/luciek/Documents/Knowledge/Indicators/Analysis"
file_path <- paste0(folder_path, "/062025_clean_clusters.csv")

# Functions --------------------------------------------------------------
# This function generates mindmap-format .txt files for each topic.
# It outputs one version with the individual details listed and one without.
write_mindmap_files <- function(df, ref, out_dir = "mindmap", include_details = TRUE) {
  # Create output directory if it doesn't exist
  if (!dir.exists(out_dir)) dir.create(out_dir)
  
  # Helper function to split and clean semicolon-separated strings
  collapse_trim <- function(x) trimws(unlist(strsplit(as.character(x), ";", fixed = TRUE)))
  
  # Get unique topics
  topics <- unique(collapse_trim(df$Topic))
  
  for (topic in topics) {
    # Notify user about current topic processing
    cat(sprintf("\nProcessing topic: %s\n", topic))
    
    # Filter data for current topic
    df_topic <- df[grepl(topic, df$Topic), ]
    
    # Get allowed subtopics for this topic
    allowed_subtopics <- unique(ref$Subtopic[ref$Climate.change.topic == topic])
    
    # Initialise file contents
    lines_no_details <- lines_with_details <- c("mindmap", paste0("  root((", topic, "))"))
    
    for (subtopic in allowed_subtopics) {
      # Filter rows matching this subtopic
      df_sub <- df_topic[grepl(subtopic, df_topic$Subtopic), ]
      
      if (nrow(df_sub) == 0) next  # Skip if no match
      
      lines_with_details <- c(lines_with_details, paste0("      ", subtopic))
      lines_no_details   <- c(lines_no_details,   paste0("      ", subtopic))
      
      for (i in seq_len(nrow(df_sub))) {
        global_ind <- as.character(df_sub$Global_Indicator[i])
        lines_with_details <- c(lines_with_details, paste0("        ", global_ind))
        lines_no_details   <- c(lines_no_details,   paste0("        ", global_ind))
        
        # Include individual details as separate leaves if desired
        if (include_details) {
          detail_lines <- collapse_trim(df_sub$Details[i])
          if (length(detail_lines) > 0) {
            detail_lines <- detail_lines[detail_lines != ""]
            lines_with_details <- c(lines_with_details, paste0("          ", detail_lines))
          }
        }
      }
    }
    
    # Write the output to files
    base_file <- gsub("[^a-zA-Z0-9]", "_", topic)
    file_with_details <- file.path(out_dir, paste0(base_file, "_with_details.txt"))
    file_no_details   <- file.path(out_dir, paste0(base_file, "_no_details.txt"))
    
    writeLines(lines_with_details, file_with_details)
    writeLines(lines_no_details,   file_no_details)
    
    # Print summary
    cat(sprintf("✅ Topic '%s' exported:\n  - %s (%d lines)\n  - %s (%d lines)\n\n",
                topic,
                basename(file_with_details), length(lines_with_details),
                basename(file_no_details), length(lines_no_details)))
  }
}

# Helper to clean and split semi-colon-separated strings, trimming whitespace
split_and_trim <- function(x) {
  if (is.na(x) || trimws(x) == "") return(character(0))
  trimws(unlist(strsplit(as.character(x), ";", fixed = TRUE)))
}

# Function to clean and expand indicator rows per topic and subtopic
prepare_topic_details <- function(df, ref, topic) {
  cat(sprintf("Preparing topic: %s\n", topic))
  
  # Filter rows that contain the current topic (partial match allowed)
  df_topic <- df[grep(topic, df$Topic), ]
  df_topic_out <- NULL
  
  for (i in seq_len(nrow(df_topic))) {
    row_i <- df_topic[i, c("Subtopic", "Global_Indicator", "Details")]
    
    details_i  <- split_and_trim(row_i$Details)
    subtopics_i <- split_and_trim(row_i$Subtopic)
    
    # Filter subtopics to those known to be associated with this topic
    allowed_subtopics <- ref[ref$Climate.change.topic == topic, "Subtopic"]
    subtopics_i <- subtopics_i[subtopics_i %in% allowed_subtopics]
    
    # If there is only one detail, attach as is
    if (length(details_i) <= 1) {
      df_topic_out <- rbind(
        df_topic_out,
        data.frame(Topic = topic,
                   Subtopic = paste(subtopics_i, collapse = "; "),
                   Global_Indicator = row_i$Global_Indicator,
                   Details = paste(details_i, collapse = "; "),
                   stringsAsFactors = FALSE)
      )
    } else {
      # Expand: assign each detail to the relevant subtopics
      details_df <- data.frame(Global_Indicator = row_i$Global_Indicator,
                               Details = details_i,
                               stringsAsFactors = FALSE)
      
      for (sub in subtopics_i) {
        expanded_rows <- data.frame(Topic = topic, 
                                    Subtopic = sub,
                                    details_df,
                                    stringsAsFactors = FALSE)
        df_topic_out <- rbind(df_topic_out, expanded_rows)
      }
    }
  }
  
  return(df_topic_out)
}

# Function to plot a circular tree of subtopics and indicators
plot_circular_tree <- function(
    df, label_wrap = 30, text_size = NULL, node_size = NULL) {
  
  # Load necessary libraries
  library(ggraph)
  library(igraph)
  library(dplyr)
  library(RColorBrewer)
  
  # Order the data by subtopic and indicator for consistency
  df <- df[order(df$Subtopic, df$Global_Indicator), ]
  rownames(df) <- seq_len(nrow(df))
  
  # Edge construction
  # Build hierarchical relationships as 'edges' between levels of classification
  
  # Link: Topic -> Subtopic
  d1 <- unique(df[, 1:2]); colnames(d1) <- c("from", "to")
  
  # Link: Subtopic -> Global_Indicator (numbering tips to force uniqueness in graph)
  d2 <- unique(df[, 2:3]); colnames(d2) <- c("from", "to")
  d2$to <- paste(seq_len(nrow(d2)), d2$to, sep = '_')  # prevent label collisions
  
  # Combine both edge levels
  edges <- rbind(d1, d2)
  
  # Vertex construction
  # Create nodes ("vertices") from edge list
  
  vertices <- data.frame(
    name = unique(c(as.character(edges$from), as.character(edges$to))),
    stringsAsFactors = FALSE
  )
  
  # Assign each node to its parent group (for colour coding)
  vertices$group <- edges$from[match(vertices$name, edges$to)]
  
  # Optional: constant node size (not used for scaling here)
  vertices$value <- 5
  
  # Label formatting
  # Function to wrap long text labels
  wrap_label <- function(label, width) {
    sapply(label, function(x) paste(strwrap(x, width = width), collapse = "\n"))
  }
  
  # Clean up tip labels by removing anything after " / "
  vertices$label_clean <- gsub(" / .*", "", vertices$name)
  vertices$label_wrapped <- wrap_label(vertices$label_clean, label_wrap)
  
  # Layout calculation
  # Calculate label angle and alignment for tips (leaf nodes)
  
  vertices$id <- NA
  leaves <- which(!(vertices$name %in% edges$from))  # tips only
  n_leaves <- length(leaves)
  
  # Assign ID and angles for circular positioning
  vertices$id[leaves] <- seq_len(n_leaves)
  vertices$angle <- 90 - 360 * vertices$id / n_leaves
  vertices$hjust <- ifelse(vertices$angle < -90, 1, 0)
  vertices$angle <- ifelse(vertices$angle < -90, vertices$angle + 180, vertices$angle)
  
  # Graph build & plot
  # Construct graph object from edges and vertices
  g <- graph_from_data_frame(edges, vertices = vertices)
  
  # Build circular tree plot
  ggraph(g, layout = "dendrogram", circular = TRUE) +
    geom_edge_diagonal(colour = "grey60", alpha = 0.5) +  # branches
    
    # Tips (leaf nodes): invisible points to hold the labels
    geom_node_point(aes(filter = leaf, x = x*1.03, y = y*1.03, colour = group), 
                    size = 2.5, alpha = 0) +
    
    # Tip labels: coloured, rotated and wrapped
    geom_node_text(aes(x = x*1.10, y = y*1.10, filter = leaf,
                       label = label_wrapped, angle = angle, hjust = hjust, colour = group),
                   size = ifelse(!is.null(text_size), text_size, ifelse(n_leaves > 50, 2.2, 2.8)),
                   lineheight = 0.85) +
    
    # Subtopic node labels: fixed orientation, larger, bolder
    geom_node_text(aes(x = x*1.6, y = y*1.6, label = wrap_label(label_wrapped, 15)),
                   data = function(d) d %>% filter(name %in% df$Subtopic),
                   size = ifelse(!is.null(node_size), node_size, 4.5), 
                   fontface = "bold", colour = "black", lineheight = 0.8) +
    
    theme_void() +  # clean background
    theme(
      legend.position = "none",
      plot.margin = unit(rep(0.5, 4), "cm")
    ) +
    expand_limits(x = c(-2, 2), y = c(-2, 2))  # extra space around edges
}

# Main --------------------------------------------------------------------
# Read indicators file
indicators <- read.csv(file_path)
indicators <- indicators[, c("Global_Indicator", "Topic", "Subtopic", "Details")]

# Clean subtopic column: standardise format and fix common typos
indicators$Subtopic <- tolower(indicators$Subtopic)
indicators$Subtopic <- gsub(pattern = "cc", replacement = "climate change", indicators$Subtopic)
sort(unique(indicators$Subtopic))
indicators$Subtopic <- gsub(pattern = "ghg", replacement = "greenhouse gases", indicators$Subtopic)
indicators$Subtopic <- gsub(pattern = "eduction", replacement = "education", indicators$Subtopic)
indicators$Subtopic <- gsub(pattern = "emisions", replacement = "emissions", indicators$Subtopic)
indicators$Subtopic <- gsub(pattern = "areas affected", replacement = "areas impacted", indicators$Subtopic)

# Read reference topic-subtopic file and clean similarly
ref <- unique(read.csv('./052025_indicators.csv')[, 7:8])
ref$Subtopic <- tolower(ref$Subtopic)
ref$Subtopic <- gsub(pattern = "cc", replacement = "climate change", ref$Subtopic)
sort(unique(ref$Subtopic))
ref$Subtopic <- gsub(pattern = "ghg", replacement = "greenhouse gases", ref$Subtopic)
ref$Subtopic <- gsub(pattern = "eduction", replacement = "education", ref$Subtopic)
ref$Subtopic <- gsub(pattern = "emisions", replacement = "emissions", ref$Subtopic)
ref$Subtopic <- gsub(pattern = "areas affected", replacement = "areas impacted", ref$Subtopic)

ref <- unique(ref)

# Generate prepared tables per topic
Mitigation <- prepare_topic_details(indicators, ref, 'Mitigation')
Adaptation <- prepare_topic_details(indicators, ref, 'Adaptation')
Drivers <- prepare_topic_details(indicators, ref, 'Drivers')
Impacts <- prepare_topic_details(indicators, ref, 'Impacts')
Vulnerability <- prepare_topic_details(indicators, ref, 'Vulnerability')

# Plot indicators by their topic and subtopic
# Scaling factor for width/height
k <- 1.3
# Export each topic's tree as a PDF
ggsave("./tree/Adaptation.pdf",
       plot = plot_circular_tree(Adaptation, label_wrap = 85, text_size = 2.8),
       width = 25*k, height = 15*k, limitsize = FALSE)
ggsave("./tree/Drivers.pdf",
       plot = plot_circular_tree(Drivers, label_wrap = 55, text_size = 4.5),
       width = 25*k, height = 15*k, limitsize = FALSE)
ggsave("./tree/Mitigation.pdf",
       plot = plot_circular_tree(Mitigation, label_wrap = 70, text_size = 4),
       width = 25*k, height = 15*k, limitsize = FALSE)
ggsave("./tree/Impacts.pdf",
       plot = plot_circular_tree(Impacts, label_wrap = 70, text_size = 4, node_size = 3),
       width = 25*k, height = 15*k, limitsize = FALSE)
ggsave("./tree/Vulnerability.pdf",
       plot = plot_circular_tree(Vulnerability, label_wrap = 55, text_size = 4),
       width = 25*k, height = 15*k, limitsize = FALSE)