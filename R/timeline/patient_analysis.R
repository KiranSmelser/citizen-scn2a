# R/timeline/patient_analysis.R
# Generate individual patient charts

library(dplyr)
library(readxl)
library(ggplot2)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "timeline", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "timeline", "functions", "plotting_functions.R"))

# Data Cleaning and Setup
df_duration <- clean_medication_data()
censor_ages <- compute_censor_ages(df_duration) %>%
  mutate(censor_age_months = censor_age_days / 30)
df_type <- suppressWarnings(clean_seizure_data()) %>%
  left_join(censor_ages, by = "patient_uuid") %>%
  filter(age_days <= censor_age_days) %>%
  select(-censor_age_days, -censor_age_months)

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
patient_list <- unique(df_duration$patient_uuid) %>%
  as_tibble() %>%
  rename(patient_uuid = value) %>%
  left_join(timeline_info$genetics %>% filter(toupper(gene) == "SCN2A") %>% select(patient_uuid, protein_variant), 
            by = "patient_uuid") %>%
  distinct(patient_uuid, .keep_all = TRUE) %>%
  mutate(
    mutation_number = as.numeric(str_extract(protein_variant, "\\d+"))
  ) %>%
  arrange(mutation_number) %>%
  pull(patient_uuid)

# Generate patient charts
pdf_file <- "./output/figures/combined_patients_report.pdf"
pdf(pdf_file, width = 16, height = 12)

# Loop through each patient and generate their chart
for (i in seq_along(patient_list)) {
  pt <- patient_list[i]
  patient_data <- prepare_patient_chart_data(
    pt = pt,
    df_duration = df_duration,
    df_type = df_type,
    timeline_data = timeline_info,
    demographics = demographics
  )
  combined_plot <- plot_patient_chart(patient_data)
  suppressWarnings(print(combined_plot))
}

dev.off()
