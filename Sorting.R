#######################################################
#    Indicators Deduplication – Interactive Workflow  #
#######################################################

# luciek@spc.int
# created: June 2025
# last edit: 18/06/2025

# Remove all objects from the current R session (start with a clean workspace)
rm(list = ls())

# Packages ----------------------------------------------------------------
# The next section loads required system settings and R packages.
# Ensure Java and Python environments are correctly configured.

# Required Libraries
Sys.setenv(JAVA_HOME = "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot")
Sys.setenv(PATH = paste(Sys.getenv("PATH"),
                        "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot/bin",
                        "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot/bin/server",
                        sep = ";"))
library(rJava)

library(reticulate)
use_python("C:/Users/luciek/AppData/Local/Programs/Python/Python311/python.exe", required = TRUE)
py_run_string("
import nltk
nltk.download('punkt')
nltk.download('averaged_perceptron_tagger')
nltk.download('wordnet')
nltk.download('omw-1.4')
nltk.download('maxent_ne_chunker')
nltk.download('words')
")

Sys.unsetenv("https_proxy")
Sys.unsetenv("http_proxy")

library(readxl)
library(text)
library(future.apply)


# User settings -----------------------------------------------------------
# The following section defines all user-specific settings and parameters.
# Update file paths, sheet names, clustering thresholds, and review tags as needed.
# Set the working directory for file read/write operations
setwd("C:/Users/luciek/My Documents/Knowledge/Indicators/Analysis/")

# File and Data Column Settings
# Path to the Excel file containing your questions
folder_path <- "C:/Users/luciek/Documents/Knowledge/Indicators"
file_path <- paste0(folder_path, "/052025_indicators.xlsx")
# Sheet index or name to load (1 = first sheet)
excel_sheet <- 1
# Column name in the Excel file containing the survey questions
question_col <- "Indicator"

# Deduplication & Clustering Parameters
# Starting threshold for cosine similarity (higher = stricter, lower = more matches)
similarity_start <- .75
# Maximum number of deduplication rounds (to avoid infinite loops)
max_rounds <- 10
# Minimum size for a question cluster (rarely changed)
min_cluster_size <- 1

# Value used to tag clusters as false positives (for later reprocessing)
false_positive_tag <- "FP"

# Framework list
frameworks <- c(
  "Blue_pacific", "BRS_convention", "CBD", "FDES", "Global_set", "FGS", "HH_survey",
  "Migratory_fish_convention_Pacific", "Minamata_convention", "Montreal_protocol",
  "Noumea_convention", "PIRT", "Ramsar", "Regional_goal", "Rotterdam_convention",
  "Samoa_pathway", "SDG", "Sendai", "SPREP", "Stockholm_convention",
  "UN_fish", "UNCCD", "UNFCCC", "Underwater_cultural_heritage_convention", "Waigani_convention"
)

# Functions ---------------------------------------------------------------
# The following functions provide core transformations: text cleaning, embedding,
# similarity computation, and framework detection.

# Clean questions
# Standardizes text: lowercases, removes enumeration prefixes, replaces '%' with words,
# strips punctuation, and collapses extra spaces.
clean_question <- function(txt) {
  txt <- tolower(txt)
  tokens <- strsplit(txt, "\\s+")[[1]]
  if (length(tokens) > 1 && grepl("\\d", tokens[1])) tokens <- tokens[-1]
  txt <- paste(tokens, collapse = " ")
  
  # Replace "% of" or "%of" → " percent of"
  txt <- gsub("%\\s*of", " percent of", txt, perl = TRUE)
  # 2) Replace any remaining "%" → " percent"
  txt <- gsub("%", " percent", txt, fixed = TRUE)
  
  # Then clean up punctuation/spaces as before
  txt <- gsub("[^a-z0-9 ]", " ", txt)
  txt <- gsub("\\s+", " ", txt)
  trimws(txt)
}

# Embedding & similarity
# Generates MiniLM embeddings for cleaned questions in parallel,
# then computes the cosine similarity matrix.
embed_and_sim <- function(questions_clean) {
  plan(multisession, workers = max(1, parallel::detectCores() - 4))
  emb_obj <- textEmbed(
    questions_clean,
    model = "C:/Users/luciek/all-MiniLM-L6-v2",
    aggregation_from_tokens_to_texts = "mean",
    batch_size = 128,
    via_parallel = TRUE,
    max_processes = future::nbrOfWorkers()
  )
  emb_mat <- as.data.frame(emb_obj$texts)
  emb_mat <- apply(as.matrix(emb_mat), 2, as.numeric)
  norms <- sqrt(rowSums(emb_mat^2))
  emb_n <- emb_mat / norms
  sim_mat <- tcrossprod(emb_n)
  return(sim_mat)
}

# Frameworks extraction
# Scans concatenated 'Source' and 'Link(s)' fields for any form of each framework name.
get_frameworks <- function(cluster_df, frameworks) {
  # Collapse all values from Source and Link(s) w other frameworks into one big string
  field_string <- paste(
    as.character(cluster_df[["Source"]]),
    as.character(cluster_df[["Link(s) w other frameworks"]]),
    collapse = " "
  )
  field_string <- tolower(field_string)
  sapply(frameworks, function(fw) {
    fw_snake  <- tolower(fw)
    fw_space  <- gsub("_", " ", fw_snake)
    fw_nospace <- gsub("_", "", fw_snake)
    grepl(fw_snake, field_string, fixed = TRUE) ||
      grepl(fw_space, field_string, fixed = TRUE) ||
      grepl(fw_nospace, field_string, fixed = TRUE)
  })
}

# Interactive Deduplication
#'
#' This function iteratively groups similar survey questions (indicators) into clusters,
#' prompts the user to review each cluster, and allows interactive assignment of new "global" indicators
#' or flagging of clusters as false positives. The workflow continues until all clusters are resolved
#' or the maximum number of rounds is reached. Each round, flagged clusters are reclustered at a stricter threshold.
#'
#' @param raw_df            The raw data.frame with indicator questions and associated info.
#' @param frameworks        A vector of framework names to scan for (used for output columns).
#' @param question_col      Column name in raw_df containing the question text.
#' @param similarity_start  Initial similarity threshold (e.g., 0.90).
#' @param min_cluster_size  Minimum cluster size to retain (typically 1).
#' @param max_rounds        Maximum number of reclustering rounds (default 10).
#' @param false_positive_tag String to use for clusters flagged as "not a real cluster" (e.g., "FP").
#'
#' @return data.frame with deduplicated and clustered indicators and all relevant columns.
deduplicate_interactive <- function(
    raw_df, frameworks, question_col,
    similarity_start, min_cluster_size, max_rounds, false_positive_tag) {
  
  # Prepare the dataframe: add a question ID, keep the raw text, and create a cleaned version
  df <- raw_df
  df$QID <- seq_len(nrow(df))  # Unique identifier for each row/question
  df$Raw_Indicator <- as.character(df[[question_col]])  # Preserve original question wording
  df$Clean_Indicator <- vapply(df$Raw_Indicator, clean_question, character(1))  # Cleaned version for similarity
  
  # Initialize storage for each round’s results, similarity threshold, and round counter
  all_cluster_results <- list()  # Stores all results from each round
  threshold <- similarity_start  # Set starting similarity threshold
  round <- 1                    # Track the current round
  
  repeat {
    # Compute embeddings and pairwise similarities for the cleaned questions
    cat(sprintf("\n[2/%d] Iteration %d: Computing embeddings and similarity (threshold=%.2f)...\n",
                max_rounds + 2, round, threshold))
    sim_mat <- embed_and_sim(df$Clean_Indicator)
    
    # Convert similarity to distance and perform average-linkage clustering
    question_dist <- as.dist(1 - sim_mat)
    hclust_obj    <- stats::hclust(question_dist, method = "average")
    clusters      <- stats::cutree(hclust_obj, h = 1 - threshold)
    df$ClusterID  <- clusters
    
    # Prepare for manual review of each cluster
    cluster_results <- list()
    false_positives <- c()
    cat(sprintf("[3/%d] Reviewing clusters (%d clusters found)...\n",
                max_rounds + 2, length(unique(clusters))))
    
    for (cl_id in sort(unique(clusters))) {
      # Extract the rows belonging to this cluster and their cleaned indicators
      cluster_rows <- df[df$ClusterID == cl_id, ]
      cluster_cleaned <- unique(cluster_rows$Clean_Indicator)
      
      # Determine which frameworks apply to this cluster
      fw_vals <- get_frameworks(cluster_rows, frameworks)
      fw_df <- as.data.frame(matrix(
        rep(unlist(fw_vals), each = nrow(cluster_rows)),
        nrow = nrow(cluster_rows),
        dimnames = list(NULL, names(fw_vals))
      ), stringsAsFactors = FALSE)
      
      # If all cleaned indicators are identical, accept automatically
      if (length(cluster_cleaned) == 1) {
        global_indicator <- cluster_cleaned
        cat(sprintf("  ✔️ Cluster %d: All indicators identical ('%s').\n", cl_id, global_indicator))
      } else {
        # Otherwise prompt the user to enter a unified label or flag as false positive
        cat(sprintf("\n  ⚠️ Cluster %d has %d unique indicators:\n", cl_id, length(cluster_cleaned)))
        cat(paste0("      ", seq_along(cluster_cleaned), ". ", cluster_cleaned, collapse = "\n"), "\n")
        user_input <- readline(
          prompt = sprintf("    → Enter new global indicator, or hit Enter to flag as FP [%s]: ", false_positive_tag)
        )
        if (user_input == "") user_input <- false_positive_tag
        global_indicator <- user_input
      }
      
      # Summarize topics, subtopics, and raw details for traceability
      topics_collapsed <- paste(unique(cluster_rows[["Climate change topic"]]), collapse = "; ")
      subtopics_collapsed <- paste(unique(cluster_rows[["Subtopic"]]), collapse = "; ")
      details <- paste(unique(cluster_rows$Raw_Indicator), collapse = "; ")
      
      # Combine all information into one data.frame per cluster row
      cluster_results[[as.character(cl_id)]] <- 
        cbind(data.frame(
          QID             = cluster_rows$QID,
          Raw_Indicator   = cluster_rows$Raw_Indicator,
          Clean_Indicator = cluster_rows$Clean_Indicator,
          Topic           = topics_collapsed,
          Subtopic        = subtopics_collapsed,
          ClusterID       = cl_id,
          Global_Indicator= global_indicator,
          Details         = details,
          Data_needs      = cluster_rows[["Data needs"]],
          Data_sources    = cluster_rows[["Data sources"]],
          stringsAsFactors = FALSE), 
          fw_df)
      
      
      # Record any clusters flagged as false positives for further rounds
      if (global_indicator == false_positive_tag) false_positives <- c(false_positives, cl_id)
    }
    
    # Collect this round's results and report counts
    cluster_results_df <- do.call(rbind, cluster_results)
    all_cluster_results[[round]] <- cluster_results_df
    
    cat(sprintf("\n[4/%d] This round: %d clusters, %d flagged as FP.\n",
                max_rounds + 2, length(unique(clusters)), length(false_positives)))
    
    # Stop if no false positives or maximum rounds reached
    if (length(false_positives) == 0) {
      cat("✅ No false positive clusters flagged. Deduplication complete!\n")
      break
    }
    if (round == max_rounds) {
      cat("🛑 Max number of rounds reached. Stopping.\n")
      break
    }
    
    # Ask whether to tighten the similarity threshold or exit early
    cat("\n[5] Would you like to tighten the threshold to reprocess FPs?\n")
    cat(sprintf("Current threshold is %.2f.\n", threshold))
    user_choice <- readline("    (1) Use default increment (+0.05)\n    (2) Enter a custom threshold\n    (3) Stop now\nChoice [1/2/3]: ")
    
    if (user_choice == "3") {
      cat("Exiting early per user choice.\n")
      break
    }
    if (user_choice == "2") {
      new_thresh <- as.numeric(readline("    Enter new threshold (0.00–1.00): "))
      if (!is.na(new_thresh) && new_thresh > threshold && new_thresh < 1) {
        threshold <- new_thresh
      } else {
        threshold <- min(threshold + 0.05, 0.99)
      }
    } else {
      threshold <- min(threshold + 0.05, 0.99)
    }
    cat(sprintf("Threshold increased to %.2f for next round.\n", threshold))
    
    # Retain only the false-positive clusters for the next iteration
    cat(sprintf("    Reprocessing %d flagged clusters only...\n", length(false_positives)))
    df <- df[df$ClusterID %in% false_positives, ]
    df$ClusterID <- NULL
    round <- round + 1
  }
  
  # Combine all rounds
  final_df <- do.call(rbind, all_cluster_results)
  
  # For each original question (QID) keep only the last time we saw it,
  # (i.e., the label assigned in the most-recent round that touched it)
  final_df <- final_df[ !duplicated(final_df$QID, fromLast = TRUE), ] 
  
  # Only aggregate actual clusters, not false positives
  globals <- setdiff(unique(final_df$Global_Indicator), false_positive_tag)
  
  out_list <- lapply(seq_along(globals), function(i) {
    subdf <- final_df[final_df$Global_Indicator == globals[i], ]
    
    Topic        <- paste(unique(na.omit(subdf$Topic)),         collapse = "; ")
    Subtopic     <- paste(unique(na.omit(subdf$Subtopic)),      collapse = "; ")
    Details      <- paste(unique(subdf$Details),                collapse = "; ")
    Data_needs   <- paste(unique(na.omit(subdf$`Data needs`)),  collapse = "; ")
    Data_sources <- paste(unique(na.omit(subdf$`Data sources`)),collapse = "; ")
    
    fw_flags <- as.list(colSums(subdf[ , frameworks, drop = FALSE]) > 0)
    names(fw_flags) <- frameworks
    
    cbind(
      data.frame(
        ClusterID        = i,
        Global_Indicator = globals[i],
        Topic            = Topic,
        Subtopic         = Subtopic,
        Details          = Details,
        Data_needs       = Data_needs,
        Data_sources     = Data_sources,
        stringsAsFactors = FALSE
      ),
      as.data.frame(fw_flags, stringsAsFactors = FALSE)
    )
  })
  
  aggregated_df <- do.call(rbind, out_list)
  
  rownames(aggregated_df) <- NULL
  
  # Deal with false positive
  fp_rows <- final_df[final_df$Global_Indicator == false_positive_tag, ]
  if (nrow(fp_rows) > 0) {
    next_id <- nrow(aggregated_df) + 1
    split_fp <- do.call(rbind, lapply(seq_len(nrow(fp_rows)), function(j) {
      r <- fp_rows[j, , drop = FALSE]
      cbind(
        data.frame(
          ClusterID        = next_id + j - 1,
          Global_Indicator = r$Clean_Indicator,
          Topic            = r$Topic,
          Subtopic         = r$Subtopic,
          Details          = r$Raw_Indicator,
          Data_needs       = if ("Data needs"   %in% names(r)) r[["Data needs"]]   else NA_character_,
          Data_sources     = if ("Data sources" %in% names(r)) r[["Data sources"]] else NA_character_,
          stringsAsFactors = FALSE
        ),
        r[ , frameworks, drop = FALSE]
      )
    }))
    aggregated_df <- rbind(aggregated_df, split_fp)
    rownames(aggregated_df) <- NULL
  }
  
  return(aggregated_df)
}

# Main --------------------------------------------------------------------
# Import your data
raw_df <- as.data.frame(readxl::read_excel(file_path, sheet = excel_sheet))
# Run the function
final_clusters <- deduplicate_interactive(
  raw_df, frameworks, question_col, similarity_start, min_cluster_size, max_rounds, false_positive_tag
)
# Enjoy the result
cat("\n🎉 Deduplication complete! Results saved to clusters.csv\n")
# write.csv(final_clusters, file = paste0(format(Sys.time(), "%Y%m"), "_clusters.csv"), row.names = FALSE)

