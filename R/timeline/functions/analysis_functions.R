# R/timeline/functions/analysis_functions.R
# Functions for data analyses

library(dplyr)
library(tidyr)
library(lubridate)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "cleaning_utilities.R"))

# Prepares common med flags and adjusted start times
prepare_medication_flags <- function(df) {
  df %>%
    # Exclude meds with less than 3 months duration
    filter((end_med_age - start_med_age) > 91) %>%
    mutate(
      # Adjust start age by adding 91 days when possible
      adjusted_start_med_age = if_else(
        (start_med_age + 91) < end_med_age,
        start_med_age + 91,
        start_med_age
      ),
      # Define logical flags for med exposure
      on_med  = (age_days >= adjusted_start_med_age) & (age_days <= end_med_age),
      before_med = age_days < start_med_age,
      after_med  = age_days > end_med_age,
      within_3months_before = age_days >= pmax(start_med_age - 91, 0) & age_days < start_med_age,
      within_3months_after  = age_days > end_med_age & age_days <= (end_med_age + 91)
    )
}

# Handles overlapping med intervals by assigning a flag
assign_overlapping_med <- function(df) {
  df_assigned <- df %>%
    arrange(patient_uuid, age_days, adjusted_start_med_age) %>%
    group_by(patient_uuid, age_days) %>%
    filter(on_med) %>%
    ungroup()
  
  df %>%
    left_join(
      df_assigned %>% 
        select(patient_uuid, age_days, medication) %>% 
        rename(assigned_med = medication),
      by = c("patient_uuid", "age_days"),
      relationship = "many-to-many"
    ) %>%
    group_by(patient_uuid, age_days) %>%
    mutate(on_med_final = medication %in% assigned_med) %>%
    ungroup()
}

# Calculates weighted seizure index comparisons across seizure types
calculate_seizure_index_comparisons <- function(df_type, df_duration, appointment_summary) {
  # Combine all seizure types w/ med duration and process common flags
  df_all <- df_type %>%
    left_join(df_duration, by = "patient_uuid", relationship = "many-to-many") %>%
    prepare_medication_flags() %>%
    assign_overlapping_med()
  
  # Keep medications with at least one seizure on-med or within 3 months
  meds_to_keep <- df_all %>%
    group_by(patient_uuid, medication) %>%
    summarise(
      has_seizure_on_med       = any(on_med_final, na.rm = TRUE),
      has_seizure_before_after = any(within_3months_before | within_3months_after, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(has_seizure_on_med | has_seizure_before_after) %>%
    select(patient_uuid, medication)
  
  seizure_types <- unique(df_type$type)
  
  seizures_summary_list <- lapply(seizure_types, function(s_type) {
    df_type_filtered <- df_type %>% filter(type == s_type)
    
    # Process combined dataset
    df_combined <- df_type_filtered %>%
      left_join(df_duration, by = "patient_uuid", relationship = "many-to-many") %>%
      prepare_medication_flags() %>%
      assign_overlapping_med() %>%
      inner_join(meds_to_keep, by = c("patient_uuid", "medication")) %>%
      left_join(appointment_summary, by = "patient_uuid") %>%
      mutate(
        adjusted_start_med_age_months = adjusted_start_med_age / 30,
        end_med_age_months            = end_med_age / 30,
        duration_before = pmax(adjusted_start_med_age_months - first_appointment, 0),
        duration_on     = pmax(end_med_age_months - adjusted_start_med_age_months, 0),
        duration_after  = pmax(last_appointment - end_med_age_months, 0),
        duration_off    = duration_before + duration_after
      )
    
    # Compute average seizure index values normalized by time durations
    df_summary <- df_combined %>%
      group_by(
        patient_uuid, medication, start_med_age, end_med_age,
        adjusted_start_med_age_months, end_med_age_months,
        first_appointment, last_appointment
      ) %>%
      summarise(
        on_med_avg     = safe_mean(index[on_med_final]),
        before_med_avg = safe_mean(index[before_med]),
        after_med_avg  = safe_mean(index[after_med]),
        off_med_avg    = safe_mean(index[!on_med_final]),
        duration_before = first(duration_before),
        duration_on     = first(duration_on),
        duration_after  = first(duration_after),
        duration_off    = first(duration_off),
        .groups = "drop"
      ) %>%
      mutate(
        weighted_index_med    = if_else(duration_on > 0, on_med_avg / sqrt(duration_on), 0),
        weighted_index_before = if_else(duration_before > 0, before_med_avg / sqrt(duration_before), 0),
        weighted_index_after  = if_else(duration_after > 0, after_med_avg / sqrt(duration_after), 0),
        weighted_index_off    = if_else(duration_off > 0, off_med_avg / sqrt(duration_off), 0),
        diff_on_vs_off    = weighted_index_med - weighted_index_off,
        diff_on_vs_before = weighted_index_med - weighted_index_before,
        diff_on_vs_after  = weighted_index_med - weighted_index_after,
        type = s_type
      )
    
    df_summary
  })
  
  seizures_summary_combined <- bind_rows(seizures_summary_list)
  return(seizures_summary_combined)
}