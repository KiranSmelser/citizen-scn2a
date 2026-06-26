# R/descriptive/patient_tracker.R
# Tracks patient inclusion across descriptive, timeline, and cluster analyses.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "cleaning_utilities.R"))
source(file.path(".", "R", "descriptive", "functions", "cleaning_functions.R"))

if (!dir.exists(DATA_PROCESSED)) dir.create(DATA_PROCESSED, recursive = TRUE)

patient_ids <- function(df) {
  df %>%
    filter(!is.na(patient_uuid), patient_uuid != "") %>%
    distinct(patient_uuid) %>%
    pull(patient_uuid)
}

flag_df <- function(ids, col_name) {
  tibble(patient_uuid = unique(ids)) %>%
    mutate("{col_name}" := 1L)
}

cluster_ids <- function() {
  cluster_file <- file.path(DATA_PROCESSED, "all_patients", "3yr_clusters.csv")

  if (!file.exists(cluster_file)) {
    warning("No 3-year cluster file found at ", cluster_file)
    return(character(0))
  }

  read_csv(cluster_file, show_col_types = FALSE) %>%
    select(patient_uuid) %>%
    patient_ids()
}

# Descriptive analysis cohorts
seizure_patients <- clean_seizure_data(include_spasms = TRUE) %>%
  patient_ids()

medication_patients <- clean_medication_data() %>%
  patient_ids()

growth_patients <- clean_growth_data() %>%
  patient_ids()

diagnosis_patients <- clean_diagnoses_data()$diagnoses %>%
  patient_ids()

hospitalization_patients <- clean_hospitalization_data() %>%
  patient_ids()

# Timeline charts are generated for patients with medication-duration data
timeline_patients <- medication_patients

# Cluster inclusion is based on the generated 3-year cluster assignment file
cluster_patients <- cluster_ids()

all_patients <- unique(c(
  seizure_patients,
  medication_patients,
  growth_patients,
  diagnosis_patients,
  hospitalization_patients,
  timeline_patients,
  cluster_patients
))

patient_tracker <- tibble(patient_uuid = sort(all_patients)) %>%
  left_join(flag_df(seizure_patients, "seizure"), by = "patient_uuid") %>%
  left_join(flag_df(medication_patients, "medication"), by = "patient_uuid") %>%
  left_join(flag_df(growth_patients, "growth"), by = "patient_uuid") %>%
  left_join(flag_df(diagnosis_patients, "diagnosis"), by = "patient_uuid") %>%
  left_join(flag_df(hospitalization_patients, "hospitalization"), by = "patient_uuid") %>%
  left_join(flag_df(timeline_patients, "timeline"), by = "patient_uuid") %>%
  left_join(flag_df(cluster_patients, "cluster"), by = "patient_uuid") %>%
  mutate(across(-patient_uuid, ~ if_else(is.na(.x), 0L, as.integer(.x)))) %>%
  distinct(patient_uuid, .keep_all = TRUE)

out_path <- file.path(DATA_PROCESSED, "patient_tracker.csv")
write_csv(patient_tracker, out_path)

message("Patient tracker written to: ", normalizePath(out_path))
