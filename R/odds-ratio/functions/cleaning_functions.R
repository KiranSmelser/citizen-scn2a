# R/odds-ratio/functions/cleaning_functions.R
# Cleaning helpers

library(dplyr)
library(readr)
library(readxl)
library(stringr)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))

clean_onset_data <- function() {
  onset_data <- read_csv(PATH_ONSET_AGES, show_col_types = FALSE)
  
  onset_data <- onset_data %>%
    group_by(UUID) %>%
    summarise(age_onset = min(Age_onset, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Onset_group = case_when(
        age_onset <= 30 ~ "Neonatal",
        age_onset > 30 & age_onset <= 120 ~ "1-4 Months",
        age_onset > 120 & age_onset <= 210 ~ "4-7 Months",
        age_onset > 210 & age_onset <= 365 ~ "7-12 Months",
        TRUE ~ "12+ Months"
      ),
      age_onset_m = age_onset / 30
    )
  return(onset_data)
}

clean_initial_seizure_types <- function() {
  initial_classifier <- read_excel(PATH_INITIAL_CLASSIFIER)
  
  seizure_data <- read_seizure_history() %>%
    select(patient_uuid, seizure_history_type, seizure_history_age_days) %>%
    filter(seizure_history_age_days < AGE_CUTOFF_DAYS) %>%
    rename(UUID = patient_uuid)
  
  seizure_data <- seizure_data %>% mutate(
    focal         = ifelse(seizure_history_type %in% initial_classifier$focal, 1, 0),
    bilateral_tc  = ifelse(seizure_history_type %in% initial_classifier$bilateral_tonic_clonic, 1, 0),
    absence       = ifelse(seizure_history_type %in% initial_classifier$absence, 1, 0),
    infantile     = ifelse(seizure_history_type %in% initial_classifier$infantile_spasms, 1, 0)
  )
  
  initial_seizures <- seizure_data %>% group_by(UUID) %>% summarise(
    focal = ifelse(sum(focal, na.rm = TRUE) > 0, 1, 0),
    bilateral_tc = ifelse(sum(bilateral_tc, na.rm = TRUE) > 0, 1, 0),
    absence = ifelse(sum(absence, na.rm = TRUE) > 0, 1, 0),
    infantile = ifelse(sum(infantile, na.rm = TRUE) > 0, 1, 0),
    .groups = "drop"
  )
  return(initial_seizures)
}

clean_abnormal_eeg <- function() {
  classifier <- read_classifier()
  eeg_data <- read_excel(PATH_CITIZEN_DATA, sheet = "diagnostic_procedures") %>%
    filter(str_detect(procedure, "EEG")) %>%
    select(patient_uuid, procedure_findings, procedure_age_days) %>%
    filter(procedure_age_days < AGE_CUTOFF_DAYS) %>%
    mutate(abnormal_eeg = ave(!(procedure_findings %in% classifier$eeg_normal),
                              patient_uuid,
                              FUN = function(x) as.integer(any(x)))) %>%
    rename(UUID = patient_uuid) %>%
    distinct(UUID, abnormal_eeg)
  return(eeg_data)
}