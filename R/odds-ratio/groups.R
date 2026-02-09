# R/odds-ratio/groups.R
# Generate table for patient groups

library(readxl)
library(readr)
library(data.table)
library(tidyverse)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "odds-ratio", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "odds-ratio", "functions", "analysis_functions.R"))

# Load genetic findings
variant_data <- read_excel(PATH_CITIZEN_DATA, sheet = "genetic_findings") %>%
  filter(gene == "SCN8A") %>%
  rename(UUID = patient_uuid) %>%
  distinct(UUID, .keep_all = TRUE) %>%
  select(UUID, variant_DNA, variant_protein)

# Group by Age of Onset
onset_data <- clean_onset_data()
study <- left_join(variant_data, onset_data, by = "UUID")

# Group by Initial Seizure Types
initial_seizures <- clean_initial_seizure_types()
study <- left_join(study, initial_seizures, by = "UUID")

# Group by Abnormal EEGs
abnormal_eeg <- clean_abnormal_eeg()
study <- left_join(study, abnormal_eeg, by = "UUID")

# Group by Number of Seizure Types
unique_seizure_types <- get_unique_seizure_types()
study <- left_join(study, unique_seizure_types, by = "UUID")

output_path <- file.path(DATA_PROCESSED, "Supplementary_Table - Study Data by Groups.csv")
write.csv(study, file = output_path, row.names = FALSE)