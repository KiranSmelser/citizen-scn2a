# R/descriptive/dev_regressions.R
# Identify instances of developmental regressions

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(readr)
})

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))

# Load & harmonize development data
dev_raw <- read_development_data() %>%
  dplyr::select(patient_uuid,
                domain_milestone,
                domain_status,
                domain_age_days_firstDate)

# Keep only rows with the necessary statuses and clean strings
dev_clean <- dev_raw %>%
  dplyr::mutate(
    domain_milestone = str_trim(domain_milestone),
    domain_status    = str_to_title(str_trim(as.character(domain_status)))
  ) %>%
  dplyr::filter(!is.na(patient_uuid),
                !is.na(domain_milestone),
                !is.na(domain_age_days_firstDate)) %>%
  dplyr::filter(domain_status %in% c("Able", "Unable")) %>%
  dplyr::distinct() %>%
  dplyr::arrange(patient_uuid, domain_milestone, domain_age_days_firstDate)

# Collapse consecutive duplicate statuses within each series
dev_compact <- dev_clean %>%
  dplyr::group_by(patient_uuid, domain_milestone) %>%
  dplyr::arrange(domain_age_days_firstDate, .by_group = TRUE) %>%
  dplyr::mutate(same_as_prev = domain_status == dplyr::lag(domain_status)) %>%
  dplyr::filter(is.na(same_as_prev) | !same_as_prev) %>%
  dplyr::ungroup()

# Identify regression events
dev_regressions <- dev_compact %>%
  dplyr::group_by(patient_uuid, domain_milestone) %>%
  dplyr::arrange(domain_age_days_firstDate, .by_group = TRUE) %>%
  dplyr::mutate(
    prev_status = dplyr::lag(domain_status),
    prev_age    = dplyr::lag(domain_age_days_firstDate)
  ) %>%
  dplyr::filter(domain_status == "Unable", prev_status == "Able") %>%
  dplyr::mutate(regression_id = dplyr::row_number()) %>%
  dplyr::transmute(
    patient_uuid,
    domain_milestone,
    regression_id,                                    
    able_age_days_firstDate   = prev_age,             
    unable_age_days_firstDate = domain_age_days_firstDate, 
    regression_delay_days     = unable_age_days_firstDate - able_age_days_firstDate
  ) %>%
  dplyr::ungroup()

View(dev_regressions)