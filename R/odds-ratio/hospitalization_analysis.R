# R/odds-ratio/hospitalization_analysis.R
# Hospitalization events logistic regression analysis

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

data <- read_supp_data()
classifier <- read_classifier()

# One-hot encode hospitalization columns
data <- one_hot_encode(data, classifier[, grep("hospital_", colnames(classifier))], "admission_diagnosis_")

# Recode unique_types and order Onset_group
data$unique_types <- recode_unique_types(data$unique_types)
data$Onset_group <- factor_onset_group(data$Onset_group)

# Define predictors and targets
predictors <- c("bilateral_tc", "Onset_group", "focal", "absence", 
                "infantile", "abnormal_eeg", "unique_types", "age_onset_m")
hospitalization_types <- c("hospital_gi", "hospital_infection", "hospital_pulmonary", 
                           "hospital_respiratory_failure", "hospital_pneumonia", 
                           "hospital_neuro", "hospital_behavior", "hospital_status",
                           "hospital_seizure")

# Create output folder for hospitalization results
results_folder <- "./output/results/hospitalization"
dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)

# Run modeling loop and compile results
results_df <- compile_model_results(data, predictors, hospitalization_types, 
                                    analysis_type = "hospitalization", results_folder = results_folder)

write.csv(results_df, file = file.path(results_folder, "model_results.csv"), row.names = FALSE)
# Save significant results separately
significant_results_df <- results_df[results_df$p_value < 0.05, ]
write.csv(significant_results_df, file = file.path(results_folder, "significant_model_results.csv"), row.names = FALSE)

# Create directory for plots
plot_folder <- "./output/figures/hospitalization"
dir.create(plot_folder, recursive = TRUE, showWarnings = FALSE)

# For each predictor, generate and save an odds-ratio plot
unique_preds <- unique(results_df$predictor)
for (pred in unique_preds) {
  predictor_df <- results_df[results_df$predictor == pred, ]
  if (nrow(predictor_df) == 0 || all(predictor_df$p_value >= 0.05)) next
  
  p <- plot_odds_ratio(predictor_df, pred, data, outcome_col = "target", outcome_label = "Diagnosis Type")
  ggsave(filename = file.path(plot_folder, paste0(pred, ".pdf")), plot = p)
}