# R/odds-ratio/functions/analysis_functions.R
# Functions for common analysis tasks

library(dplyr)
library(tidyr)
library(lubridate)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "cleaning_utilities.R"))

# Determines unique seizure types for each patient
get_unique_seizure_types <- function() {
  seizure_data <- read_seizure_history() %>%
    rename(UUID = patient_uuid) %>%
    filter(seizure_history_age_days < AGE_CUTOFF_DAYS) %>%
    select(UUID, seizure_history_type)
  
  # Exclude non-specified seizure types
  exclude_types <- c("Seizure", "Unclassified seizure", "Provoked seizure", "Unprovoked seizure")
  seizure_data <- seizure_data %>%
    filter(!seizure_history_type %in% exclude_types)
  
  # Read classifier and extract seizure type groupings
  classifier <- read_excel(PATH_CLASSIFIER)
  types_classifier <- classifier %>% select(8:13)
  
  # Helper to map a seizure type to its classifier group
  find_column_name <- function(value, df) {
    for (col_name in names(df)) {
      if (value %in% df[[col_name]]) {
        return(col_name)
      }
    }
    return(NA)
  }
  
  seizure_data <- seizure_data %>%
    mutate(seizure_group = sapply(seizure_history_type, find_column_name, df = types_classifier))
  
  unique_seizure_types <- seizure_data %>%
    group_by(UUID) %>%
    summarise(unique_types = n_distinct(seizure_group, na.rm = TRUE)) %>%
    ungroup()
  
  return(unique_seizure_types)
}