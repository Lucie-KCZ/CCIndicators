############################################################
#    Survey Question Deduplication – Interactive Workflow  #
############################################################

  # User settings -----------------------------------------------------------
  # Remove all objects from the current R session (start with a clean workspace)
  rm(list = ls())
  
  # Set the working directory for file read/write operations
  setwd("C:/Users/luciek/My Documents/Knowledge/Indicators/Analysis/")
  
  # File and Data Column Settings
  file_path         <- "C:/Users/luciek/Documents/Knowledge/Indicators/052025_indicators.xlsx"  # Path to the Excel file containing your questions
  excel_sheet       <- 1                # Sheet index or name to load (1 = first sheet)
  question_col      <- "Indicator"      # Column name in the Excel file containing the survey questions
  
  # Deduplication & Clustering Parameters
  similarity_start   <- 0.90            # Starting threshold for cosine similarity (higher = stricter, lower = more matches)
  max_rounds         <- 10              # Maximum number of deduplication rounds (to avoid infinite loops)
  min_cluster_size   <- 1               # Minimum size for a question cluster (rarely changed)
  
  # Manual Review Tag
  false_positive_tag <- "FP"            # Value used to tag clusters as false positives (for later reprocessing)
  
  # Framework list
  frameworks <- c(
    "Blue_pacific", "BRS_convention", "CBD", "FDES", "Global_set", "FGS", "HH_survey",
    "Migratory_fish_convention_Pacific", "Minamata_convention", "Montreal_protocol",
    "Noumea_convention", "PIRT", "Ramsar", "Regional_goal", "Rotterdam_convention",
    "Samoa_pathway", "SDG", "Sendai", "SPREP", "Stockholm_convention",
    "UN_fish", "UNCCD", "UNFCCC", "Underwater_cultural_heritage_convention", "Waigani_convention"
  )
  
  # Required Libraries
  library(readxl)
  library(text)
  library(future.apply)
  
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
  
  
# Functions ---------------------------------------------------------------
# -- Clean Questions --
clean_question <- function(txt) {
  txt <- tolower(txt)
  tokens <- strsplit(txt, "\\s+")[[1]]
  if (length(tokens) > 1 && grepl("\\d", tokens[1])) tokens <- tokens[-1]
  txt <- paste(tokens, collapse = " ")
  txt <- gsub("[^a-z0-9% ]", " ", txt)
  txt <- gsub("[[:space:]]+", " ", txt)
  trimws(txt)
}

# -- Embedding & Similarity --
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

# -- Frameworks Extraction for Each Cluster --
get_frameworks <- function(cluster_df, frameworks) {
  frameworks_in_cluster <- unique(na.omit(
    c(cluster_df[, "Source"], cluster_df[, "Link(s) w other frameworks"])
  ))
  sapply(frameworks, function(fw) {
    any(grepl(gsub("_", " ", fw), frameworks_in_cluster, ignore.case = TRUE))
  })
}

# -- Main Interactive Deduplication Loop --
#' Interactive Deduplication and Clustering of Survey Questions
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
  # Initialization
  df <- raw_df
  df$QID <- seq_len(nrow(df))  # Unique identifier for each row/question
  df$Raw_Indicator <- as.character(df[[question_col]])  # Preserve original question wording
  df$Clean_Indicator <- vapply(df$Raw_Indicator, clean_question, character(1))  # Cleaned version for similarity
  all_cluster_results <- list()  # Stores all results from each round
  threshold <- similarity_start  # Set starting similarity threshold
  round <- 1                    # Track the current round
  
  repeat {
    # 1. Compute Embeddings and Similarity Matrix
    cat(sprintf("\n[2/%d] Iteration %d: Computing embeddings and similarity (threshold=%.2f)...\n",
                max_rounds + 2, round, threshold))
    sim_mat <- embed_and_sim(df$Clean_Indicator)
    
    # 2. Hierarchical Clustering of Questions
    #   - Convert similarity to distance (distance = 1 - similarity)
    #   - Use average linkage clustering
    #   - Assign clusters by cutting the tree at (1 - threshold)
    question_dist <- as.dist(1 - sim_mat)
    hclust_obj    <- stats::hclust(question_dist, method = "average")
    clusters      <- stats::cutree(hclust_obj, h = 1 - threshold)
    df$ClusterID  <- clusters
    
    # 3. Cluster Review: Interactive Per-Cluster Assignment
    # For each cluster:
    #   - If all indicators are identical (after cleaning), accept directly
    #   - Otherwise, prompt user for a new global indicator, or to flag as FP
    cluster_results <- list()
    false_positives <- c()
    cat(sprintf("[3/%d] Reviewing clusters (%d clusters found)...\n",
                max_rounds + 2, length(unique(clusters))))
    for (cl_id in sort(unique(clusters))) {
      # Extract all rows/questions for this cluster
      cluster_rows <- df[df$ClusterID == cl_id, ]
      # Find unique cleaned indicators in this cluster
      cluster_cleaned <- unique(cluster_rows$Clean_Indicator)
      
      # Framework membership for the current cluster
      fw_vals <- get_frameworks(cluster_rows, frameworks)
      
      # 3a. All indicators in this cluster are identical: assign automatically
      if (length(cluster_cleaned) == 1) {
        global_indicator <- cluster_cleaned
        cat(sprintf("  ✔️ Cluster %d: All indicators identical ('%s').\n", cl_id, global_indicator))
      } else {
        # 3b. User review: prompt for new global indicator or flag as FP
        cat(sprintf("\n  ⚠️ Cluster %d has %d unique indicators:\n", cl_id, length(cluster_cleaned)))
        cat(paste0("      ", seq_along(cluster_cleaned), ". ", cluster_cleaned, collapse = "\n"), "\n")
        user_input <- readline(
          prompt = sprintf("    → Enter new global indicator, or hit Enter to flag as FP [%s]: ", false_positive_tag)
        )
        if (user_input == "") user_input <- false_positive_tag
        global_indicator <- user_input
      }
      
      # Prepare a Details column that lists all raw indicators in this cluster (for traceability)
      details <- paste(unique(cluster_rows$Raw_Indicator), collapse = "; ")
      
      # Store results for each member of the cluster
      cluster_results[[as.character(cl_id)]] <- data.frame(
        QID = cluster_rows$QID,
        Raw_Indicator = cluster_rows$Raw_Indicator,
        Clean_Indicator = cluster_rows$Clean_Indicator,
        Topic = cluster_rows[["Climate change topic"]],
        Subtopic = cluster_rows[["Subtopic"]],
        ClusterID = cl_id,
        Global_Indicator = global_indicator,
        Details = details,
        as.data.frame(as.list(fw_vals), stringsAsFactors = FALSE),
        stringsAsFactors = FALSE
      )
      
      # Track clusters flagged as "false positive" for possible reprocessing
      if (global_indicator == false_positive_tag) false_positives <- c(false_positives, cl_id)
    }
    
    # 4. Compile and Store This Round's Results
    # Bind all cluster summaries for this round together, then save for later
    cluster_results_df <- do.call(rbind, cluster_results)
    all_cluster_results[[round]] <- cluster_results_df
    
    cat(sprintf("\n[4/%d] This round: %d clusters, %d flagged as FP.\n",
                max_rounds + 2, length(unique(clusters)), length(false_positives)))
    
    # 5. Exit Conditions
    if (length(false_positives) == 0) {
      cat("✅ No false positive clusters flagged. Deduplication complete!\n")
      break
    }
    if (round == max_rounds) {
      cat("🛑 Max number of rounds reached. Stopping.\n")
      break
    }
    
    # 6. Prompt User for Next Threshold
    cat("\n[5] Would you like to tighten the threshold to reprocess FPs?\n")
    cat(sprintf("Current threshold is %.2f.\n", threshold))
    user_choice <- readline("    (1) Use default increment (+0.05)\n    (2) Enter a custom value\n    (3) Stop now\nChoice [1/2/3]: ")
    
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
    
    # 7. Filter: Only Reprocess Flagged Clusters
    #   - Remove old cluster assignments and only retain rows that were in false positive clusters
    cat(sprintf("    Reprocessing %d flagged clusters only...\n", length(false_positives)))
    df <- df[df$ClusterID %in% false_positives, ]
    df$ClusterID <- NULL
    round <- round + 1
  }
  
  # 8. Compile All Results and Return
  # Combine all results, keeping only the *latest* cluster assignment for each QID
  final_df <- do.call(rbind, all_cluster_results)
  final_df <- final_df[!duplicated(final_df$QID, fromLast = TRUE), ]
  final_df <- final_df[order(final_df$QID), ]
  rownames(final_df) <- NULL
  return(final_df)
}


# Main --------------------------------------------------------------------
raw_df <- as.data.frame(readxl::read_excel(file_path, sheet = excel_sheet))
final_clusters <- deduplicate_interactive(
  raw_df, frameworks, question_col, similarity_start, min_cluster_size, max_rounds, false_positive_tag
)
write.csv(final_clusters, file = "final_clusters_wide.csv", row.names = FALSE)
cat("\n🎉 Deduplication complete! Results saved to final_clusters_wide.csv\n")
