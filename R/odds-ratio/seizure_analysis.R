# R/odds-ratio/seizure_analysis.R
# Seizure outcome logistic regression analysis

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "cleaning_utilities.R"))
source(file.path(".", "R", "odds-ratio", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "odds-ratio", "functions", "modeling_functions.R"))
source(file.path(".", "R", "odds-ratio", "functions", "plotting_functions.R"))

library(dplyr)
library(pROC)
library(caret)
library(ggplot2)
library(readr)
library(readxl)

# Import data and classifier
data <- read_supp_data()
classifier <- read_classifier()

# One-hot encode hospitalization columns
data <- one_hot_encode(data, classifier[, grep("seizure_", colnames(classifier))], "seizure_history_type_")

# Recode unique_types and order Onset_group
data$unique_types <- recode_unique_types(data$unique_types)
data$Onset_group <- factor_onset_group(data$Onset_group)

# Define predictors and targets
predictors <- c("bilateral_tc", "Onset_group", "focal", "absence", 
                "infantile", "abnormal_eeg", "age_onset_m")
seizure_types <- c("seizure_focal", "seizure_spasms", "seizure_tonic-clonic", 
                   "seizure_clonic", "seizure_tonic", "seizure_myoclonic", 
                   "seizure_absence", "seizure_history_type_Status epilepticus", 
                   "seizure_history_type_Prolonged seizure (>5 minutes)")

# Create output folder for seizure results
results_folder <- "./output/results/seizure"
dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)

# Run modeling loop and compile results
results_seizure <- compile_model_results(data, predictors, seizure_types, 
                                         analysis_type = "seizure", results_folder = results_folder)
write.csv(results_seizure, file = file.path(results_folder, "seizure_model_results.csv"), row.names = FALSE)

# Create directory for plots
plot_folder <- "./output/figures/seizure"
dir.create(plot_folder, recursive = TRUE, showWarnings = FALSE)

# For each predictor, generate and save an odds-ratio plot
unique_preds <- unique(results_seizure$predictor)
for (pred in unique_preds) {
  predictor_df <- results_seizure[results_seizure$predictor == pred, ]
  if (nrow(predictor_df) == 0 || all(predictor_df$p_value >= 0.05)) next
  
  p <- plot_odds_ratio(predictor_df, pred, data, outcome_col = "target", outcome_label = "Seizure Type")
  ggsave(filename = file.path(plot_folder, paste0(pred, ".pdf")), plot = p)
}