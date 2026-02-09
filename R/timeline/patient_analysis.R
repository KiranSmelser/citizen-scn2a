# R/timeline/patient_analysis.R
# Generate individual patient charts

library(dplyr)
library(readxl)
library(ggplot2)
library(patchwork)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "timeline", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "timeline", "functions", "analysis_functions.R"))
source(file.path(".", "R", "timeline", "functions", "plotting_functions.R"))

# Data Cleaning and Setup
df_duration <- clean_medication_data()
censor_ages <- compute_censor_ages(df_duration) %>%
  mutate(censor_age_months = censor_age_days / 30)
df_type <- suppressWarnings(clean_seizure_data()) %>%
  left_join(censor_ages, by = "patient_uuid") %>%
  filter(age_days <= censor_age_days) %>%
  select(-censor_age_days, -censor_age_months)
appointment_summary <- compute_appointment_summary() %>%
  left_join(censor_ages %>% select(patient_uuid, censor_age_months), by = "patient_uuid") %>%
  mutate(
    last_appointment  = pmin(last_appointment,  censor_age_months),
    first_appointment = pmin(first_appointment, censor_age_months)
  ) %>%
  select(-censor_age_months)

# Seizure Index Comparisons
seizures_summary_combined <- calculate_seizure_index_comparisons(
  df_type = df_type,
  df_duration = df_duration,
  appointment_summary = appointment_summary
)

# Import Data and Classifier
df_sz <- read_excel(PATH_CITIZEN_DATA, sheet = "seizure_history")
names(df_sz) <- sub('^seizure_history_', '', names(df_sz))
df_sz <- df_sz %>%
  left_join(censor_ages, by = "patient_uuid") %>%
  filter(age_days <= censor_age_days) %>%
  select(-censor_age_days, -censor_age_months)
classifier <- read_excel(PATH_CLASSIFIER)
timeline_info <- suppressWarnings(timeline_data(df_sz, classifier, censor_ages))


demographics <- read_excel(PATH_CITIZEN_DATA, sheet = "demographics")

# Order Patient List
patient_list <- unique(seizures_summary_combined$patient_uuid) %>%
  as_tibble() %>%
  rename(patient_uuid = value) %>%
  left_join(timeline_info$overlap_patients %>% select(`Patient ID`, `Registry #`), 
            by = c("patient_uuid" = "Patient ID")) %>%
  left_join(timeline_info$genetics %>% filter(gene == "SCN8A") %>% select(patient_uuid, variant_protein), 
            by = "patient_uuid") %>%
  left_join(timeline_info$overlap_patients %>% select(`Patient ID`, `p.`), 
            by = c("patient_uuid" = "Patient ID")) %>%
  distinct(patient_uuid, .keep_all = TRUE) %>%
  mutate(
    protein_mutation = coalesce(variant_protein, `p.`),
    registry_status = if_else(!is.na(`Registry #`), 1, 0),
    mutation_number = as.numeric(str_extract(protein_mutation, "\\d+"))
  ) %>%
  arrange(desc(registry_status), mutation_number) %>%
  pull(patient_uuid)

# Generate patient charts
pdf_file <- "./output/figures/combined_patients_report.pdf"
pdf(pdf_file, width = 16, height = 12)

# Loop through each patient and generate their chart
for (i in seq_along(patient_list)) {
  pt <- patient_list[i]
  patient_data <- prepare_patient_chart_data(
    pt = pt,
    seizures_summary_combined = seizures_summary_combined,
    df_duration = df_duration,
    df_type = df_type,
    timeline_data = timeline_info,
    df_sz = df_sz,
    demographics = demographics
  )
  # Override timeline title to show PDF page number and variant only
  variant <- sub(".*\\(([^\\)]+)\\).*", "\\1", patient_data$timeline_title)
  patient_data$timeline_title <- paste0("#", i, " - ", variant)
  combined_plot <- plot_patient_chart(patient_data)
  suppressWarnings(print(combined_plot))
}

dev.off()