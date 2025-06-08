############################################################
#  Detecting Duplicate Survey Questions with MiniLM Embeddings
#  ----------------------------------------------------------
#  This script finds near-duplicate survey questions by:
#    - Cleaning question texts
#    - Generating transformer embeddings
#    - Calculating pairwise similarities
#    - Reporting highly similar pairs
#    - (Optionally) Clustering questions by similarity
############################################################

# Script author: luciek@spc.int
# Created: June 2025
# Last edited: 6 June 2025

# User settings -----------------------------------------------------------
rm(list = ls())
setwd("C:/Users/luciek/My Documents/Knowledge/Indicators/Analysis/")

# File and column info
file_path         <- "C:/Users/luciek/Documents/Knowledge/Indicators/052025_indicators.xlsx"   # Excel file
excel_sheet       <- 1                        # Sheet number or name
question_col_name <- "Indicator"              # Name of column with questions
similarity_cutoff <- 0.90                     # Cosine similarity threshold for "duplicates"
min_cluster_size  <- 1                        # Minimum cluster size (optional clustering)

# Needed packages ---------------------------------------------------------
# Set Java paths (required on some systems)
Sys.setenv(JAVA_HOME = "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot")
Sys.setenv(PATH = paste(Sys.getenv("PATH"),
                        "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot/bin",
                        "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot/bin/server",
                        sep = ";"))

# Load R packages and set up Python/Java (required for transformer models)
library(readxl)
library(rJava)
library(reticulate)
library(text)
library(future.apply)

# Set Python paths (required on some systems)
Sys.setenv(JAVA_HOME = "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot")
Sys.setenv(PATH = paste(Sys.getenv("PATH"),
                        "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot/bin",
                        "C:/Users/luciek/AppData/Local/Programs/Eclipse Adoptium/jdk-17.0.15.6-hotspot/bin/server",
                        sep = ";"))
use_python("C:/Users/luciek/AppData/Local/Programs/Python/Python311/python.exe", required = TRUE)

# Download necessary NLTK data for Python (needed by {text})
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

# Load data ---------------------------------------------------------------
# Read questions from Excel file
survey_df <- as.data.frame(readxl::read_excel(file_path, sheet = excel_sheet))
if (! question_col_name %in% names(survey_df)) {
  stop(paste("Column", question_col_name, "not found in the spreadsheet"))
}
raw_questions <- as.character(survey_df[[question_col_name]])
question_ids  <- seq_along(raw_questions)  # For unique indexing

# Clean question text -----------------------------------------------------
# Standardise and simplify questions for better duplicate detection
clean_question <- function(txt) {
  txt <- tolower(txt)
  # Remove enumeration (e.g. "1.", "(a)", etc.)
  txt <- gsub("^\\s*\\(?[0-9a-z]+(?:[\\.-][0-9a-z]+)*[\\.)-]?\\s+", "", txt, perl = TRUE)
  # Remove punctuation and extra spaces
  txt <- gsub("[[:punct:]]+", " ", txt)
  txt <- gsub("[[:space:]]+", " ", txt)
  trimws(txt)
}
questions_clean <- vapply(raw_questions, clean_question, character(1))

# Create transformer embeddings -------------------------------------------
# Use MiniLM model to create a vector representation for each question
plan(multisession, workers = max(1, parallel::detectCores() - 4))
embeddings_obj <- textEmbed(
  questions_clean,
  model = "C:/Users/luciek/all-MiniLM-L6-v2",  # Path to downloaded MiniLM model
  aggregation_from_tokens_to_texts = "mean",
  batch_size = 128,
  via_parallel = TRUE,
  max_processes = future::nbrOfWorkers()
)
embeddings_matrix <- as.data.frame(embeddings_obj$texts)
embeddings_matrix <- apply(as.matrix(embeddings_matrix), 2, as.numeric)

# Optionally save to disk:
# saveRDS(embeddings_matrix, "question_embeddings.rds")


# Quantify similarity in phrasing -----------------------------------------
# Compare all questions to find pairs that are highly similar
row_norms <- sqrt(rowSums(embeddings_matrix^2))
embeddings_normalized <- embeddings_matrix / row_norms
similarity_matrix <- tcrossprod(embeddings_normalized)  # [n x n] similarity for all pairs

# Extract pairs of questions that are above the similarity threshold
pair_indices <- which(similarity_matrix > similarity_cutoff & upper.tri(similarity_matrix), arr.ind = TRUE)

if (nrow(pair_indices) == 0) {
  cat("No pairs exceed similarity threshold", similarity_cutoff, "\n")
} else {
  duplicates_df <- data.frame(
    question_id_1 = question_ids[pair_indices[, 1]],
    question_id_2 = question_ids[pair_indices[, 2]],
    similarity    = similarity_matrix[pair_indices],
    question_1    = raw_questions[pair_indices[, 1]],
    question_2    = raw_questions[pair_indices[, 2]],
    stringsAsFactors = FALSE
  )
  duplicates_df <- duplicates_df[order(-duplicates_df$similarity), ]  # Most similar pairs first
  write.csv(duplicates_df, "duplicate_pairs.csv", row.names = FALSE)
  cat(nrow(duplicates_df), "likely duplicate pairs written to duplicate_pairs.csv\n")
}

# Create clusters ---------------------------------------------------------
# Group questions into clusters based on their similarity
if (nrow(embeddings_matrix) >= min_cluster_size) {
  cat("Clustering questions by similarity…\n")
  question_dist <- as.dist(1 - similarity_matrix)         # Convert similarity to distance
  hclust_obj    <- stats::hclust(question_dist, method = "average")
  question_clusters <- stats::cutree(hclust_obj, h = 1 - similarity_cutoff)
  cluster_table <- data.frame(
    cluster  = question_clusters,
    question = raw_questions,
    stringsAsFactors = FALSE
  )
  write.csv(cluster_table, "duplicate_clusters.csv", row.names = FALSE)
  cat("Cluster assignments written to duplicate_clusters.csv\n")
}

# Optionally, add cluster labels to main survey data and export
survey_with_clusters <- merge(survey_df, cluster_table, by.x = 'Indicator', by.y = 'question', all.x = TRUE)
survey_with_clusters <- survey_with_clusters[order(survey_with_clusters$cluster), ]

# Clean up question text in the survey data with cluster info
survey_with_clusters$Indicator <- vapply(survey_with_clusters$Indicator, clean_question, character(1))


# Summarize clusters ------------------------------------------------------
# This function summarises topics and frameworks for each cluster.
summarise_cluster_frameworks <- function(cluster_df) {
  # --- Identify unique frameworks in the cluster ---
  frameworks_in_cluster <- unique(na.omit(
    c(cluster_df[, "Source"], cluster_df[, "Link(s) w other frameworks"])
  ))
  
  # --- Summarise topics, subtopics, and frameworks ---
  if (nrow(cluster_df) == 1) {
    cluster_summary <- data.frame(
      Topic1 = cluster_df[, "Climate change topic"], 
      subtopic1 = cluster_df[, "Subtopic"],
      Topic2 = NA, subtopic2 = NA, 
      Topic3 = NA, subtopic3 = NA,
      Indicator = cluster_df[, "Indicator"],
      details = NA,
      Blue_pacific = any(grepl("Bluepacific", frameworks_in_cluster, ignore.case = TRUE)),
      BRS_convention = any(grepl("BRS", frameworks_in_cluster, ignore.case = TRUE)),
      CBD = any(grepl("CBD", frameworks_in_cluster, ignore.case = TRUE)),
      FDES = any(grepl("FDES", frameworks_in_cluster, ignore.case = TRUE)),
      Global_set = any(c(
        grepl("FGS", frameworks_in_cluster, ignore.case = TRUE),
        grepl("Global_set", frameworks_in_cluster, ignore.case = TRUE)
      )),
      HH_survey = any(grepl("HH_survey", frameworks_in_cluster, ignore.case = TRUE)),
      Migratory_fish_convention_Pacific = any(grepl("migratory fish", frameworks_in_cluster, ignore.case = TRUE)),
      Minamata_convention = any(grepl("Minamata", frameworks_in_cluster, ignore.case = TRUE)),
      Montreal_protocol = any(grepl("Montreal", frameworks_in_cluster, ignore.case = TRUE)),
      Noumea_convention = any(grepl("Noumea", frameworks_in_cluster, ignore.case = TRUE)),
      PIRT = any(grepl("PIRT", frameworks_in_cluster, ignore.case = TRUE)),
      Ramsar = any(grepl("Ramsar", frameworks_in_cluster, ignore.case = TRUE)),
      Regional_goal = any(grepl("regional goal", frameworks_in_cluster, ignore.case = TRUE)),
      Rotterdam_convention = any(grepl("Rotterdam", frameworks_in_cluster, ignore.case = TRUE)),
      Samoa_pathway = any(grepl("Samoa", frameworks_in_cluster, ignore.case = TRUE)),
      SDG = any(grepl("SDG", frameworks_in_cluster, ignore.case = TRUE)),
      Sendai = any(grepl("Sendai", frameworks_in_cluster, ignore.case = TRUE)),
      SPREP = any(grepl("SPREP", frameworks_in_cluster, ignore.case = TRUE)),
      Stockholm_convention = any(grepl("Stockholm", frameworks_in_cluster, ignore.case = TRUE)),
      UN_fish = any(grepl("UN fish stocks", frameworks_in_cluster, ignore.case = TRUE)),
      UNCCD = any(grepl("UNCCD", frameworks_in_cluster, ignore.case = TRUE)),
      UNFCCC = any(grepl("UNFCCC", frameworks_in_cluster, ignore.case = TRUE)),
      Underwater_cultural_heritage_convention = any(grepl("underwater cultural heritage", frameworks_in_cluster, ignore.case = TRUE)),
      Waigani_convention = any(grepl("Waigani", frameworks_in_cluster, ignore.case = TRUE)),
      data_needs = cluster_df[, "Data needs"],
      data_sources = cluster_df[, "Data sources"]
    )
  } else {
    top_topics <- names(sort(table(cluster_df[, "Climate change topic"]), decreasing = TRUE))[1:3]
    top_subtopics <- vector("list", 3)
    for (i in 1:3) {
      if (!is.na(top_topics[i])) {
        top_subtopics[[i]] <- names(sort(
          table(cluster_df[cluster_df[, "Climate change topic"] == top_topics[i], "Subtopic"]),
          decreasing = TRUE
        ))[1]
      } else {
        top_subtopics[[i]] <- NA
      }
    }
    print(sort(cluster_df[, "Indicator"]))
    user_indicator <- readline(prompt = "Please enter the new 'global' indicator: ")
    cluster_summary <- data.frame(
      Topic1 = top_topics[1], 
      subtopic1 = top_subtopics[[1]],
      Topic2 = top_topics[2], 
      subtopic2 = top_subtopics[[2]],
      Topic3 = top_topics[3], 
      subtopic3 = top_subtopics[[3]],
      Indicator = user_indicator,
      details = cluster_df[, "Indicator"],
      Blue_pacific = any(grepl("Bluepacific", frameworks_in_cluster, ignore.case = TRUE)),
      BRS_convention = any(grepl("BRS", frameworks_in_cluster, ignore.case = TRUE)),
      CBD = any(grepl("CBD", frameworks_in_cluster, ignore.case = TRUE)),
      FDES = any(grepl("FDES", frameworks_in_cluster, ignore.case = TRUE)),
      Global_set = any(c(
        grepl("FGS", frameworks_in_cluster, ignore.case = TRUE),
        grepl("Global_set", frameworks_in_cluster, ignore.case = TRUE)
      )),
      HH_survey = any(grepl("HH_survey", frameworks_in_cluster, ignore.case = TRUE)),
      Migratory_fish_convention_Pacific = any(grepl("migratory fish", frameworks_in_cluster, ignore.case = TRUE)),
      Minamata_convention = any(grepl("Minamata", frameworks_in_cluster, ignore.case = TRUE)),
      Montreal_protocol = any(grepl("Montreal", frameworks_in_cluster, ignore.case = TRUE)),
      Noumea_convention = any(grepl("Noumea", frameworks_in_cluster, ignore.case = TRUE)),
      PIRT = any(grepl("PIRT", frameworks_in_cluster, ignore.case = TRUE)),
      Ramsar = any(grepl("Ramsar", frameworks_in_cluster, ignore.case = TRUE)),
      Regional_goal = any(grepl("regional goal", frameworks_in_cluster, ignore.case = TRUE)),
      Rotterdam_convention = any(grepl("Rotterdam", frameworks_in_cluster, ignore.case = TRUE)),
      Samoa_pathway = any(grepl("Samoa", frameworks_in_cluster, ignore.case = TRUE)),
      SDG = any(grepl("SDG", frameworks_in_cluster, ignore.case = TRUE)),
      Sendai = any(grepl("Sendai", frameworks_in_cluster, ignore.case = TRUE)),
      SPREP = any(grepl("SPREP", frameworks_in_cluster, ignore.case = TRUE)),
      Stockholm_convention = any(grepl("Stockholm", frameworks_in_cluster, ignore.case = TRUE)),
      UN_fish = any(grepl("UN fish stocks", frameworks_in_cluster, ignore.case = TRUE)),
      UNCCD = any(grepl("UNCCD", frameworks_in_cluster, ignore.case = TRUE)),
      UNFCCC = any(grepl("UNFCCC", frameworks_in_cluster, ignore.case = TRUE)),
      Underwater_cultural_heritage_convention = any(grepl("underwater cultural heritage", frameworks_in_cluster, ignore.case = TRUE)),
      Waigani_convention = any(grepl("Waigani", frameworks_in_cluster, ignore.case = TRUE)),
      data_needs = paste(unique(cluster_df[, "Data needs"]), collapse = "; "),
      data_sources = paste(unique(cluster_df[, "Data sources"]), collapse = "; ")
    )
  }
  return(cluster_summary)
}

# Number of clusters requiring manual input
sum(table(table(cluster_table$cluster))[-1])

# Apply cluster summary function to each cluster
cluster_summaries <- do.call(
  rbind, as.list(by(survey_with_clusters, 
                    as.factor(survey_with_clusters$cluster),
                    summarise_cluster_frameworks)))
cluster_summaries[sort(cluster_summaries$Topic1, 
                       cluster_summaries$Topic2, 
                       cluster_summaries$Topic3, 
                       cluster_summaries$Indicator), ]

# Save cluster summaries
write.csv(cluster_summaries, "clustered_questions.csv", row.names = FALSE)
