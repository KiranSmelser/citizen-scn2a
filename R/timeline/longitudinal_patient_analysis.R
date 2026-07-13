# R/timeline/longitudinal_patient_analysis.R
# Generate a patient-level longitudinal seizure and medication table

library(dplyr)
library(stringr)
library(tidyr)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "timeline", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "timeline", "functions", "analysis_functions.R"))

# Use the final medication end as the observation boundary, consistent with
# the patient timeline analysis.
df_duration <- clean_medication_data()
censor_ages <- compute_censor_ages(df_duration) %>%
  mutate(censor_age_months = censor_age_days / 30)

df_type <- suppressWarnings(clean_seizure_data()) %>%
  inner_join(censor_ages, by = "patient_uuid") %>%
  filter(!is.na(age_days), age_days <= censor_age_days) %>%
  select(-censor_age_days, -censor_age_months)

# Bound appointment follow-up to the same observation boundary. Medication
# end dates are then included explicitly so that trailing seizure gaps extend
# through the complete medication timeline.
appointment_summary <- compute_appointment_summary() %>%
  inner_join(censor_ages, by = "patient_uuid") %>%
  mutate(
    first_appointment = pmin(first_appointment, censor_age_months),
    last_appointment = pmin(last_appointment, censor_age_months)
  ) %>%
  select(-censor_age_days, -censor_age_months)

medication_follow_up <- df_duration %>%
  group_by(patient_uuid) %>%
  summarise(last_medication_end = max(end_med_age, na.rm = TRUE) / 30, .groups = "drop")

appointment_summary <- appointment_summary %>%
  left_join(medication_follow_up, by = "patient_uuid") %>%
  mutate(last_appointment = pmax(last_appointment, last_medication_end, na.rm = TRUE)) %>%
  select(-last_medication_end)

df_table_data <- df_type %>%
  select(patient_uuid, type, index, age_days) %>%
  mutate(
    age_in_months = age_days / 30,
    type = recode(type, !!!ABBREVIATIONS_SEIZURES)
  )

seizure_counts <- get_seizure_counts(df_table_data)
seizure_gaps <- get_seizure_gaps(df_table_data, appointment_summary)

seizures_summary_combined <- calculate_seizure_index_comparisons(
  df_type,
  df_duration,
  appointment_summary
)

# Remove interval suffixes from medication names and apply standard
# abbreviations before producing patient-level summaries.
normalize_medication <- function(medication) {
  medication <- str_remove(medication, "\\s+\\d+$")
  medication <- str_trim(medication)
  recode(medication, !!!ABBREVIATIONS_MEDS)
}

df_duration_summary <- df_duration %>%
  mutate(medication = normalize_medication(medication))

medication_summary <- df_duration_summary %>%
  group_by(patient_uuid) %>%
  summarise(
    number_med_types = n_distinct(medication),
    med_types = paste(unique(medication), collapse = ", "),
    .groups = "drop"
  )

current_weaned_summary <- df_duration_summary %>%
  mutate(end_age_months = end_med_age / 30) %>%
  left_join(
    appointment_summary %>% select(patient_uuid, last_appointment),
    by = "patient_uuid"
  ) %>%
  group_by(patient_uuid, medication) %>%
  summarise(
    is_current = any(end_age_months >= last_appointment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(patient_uuid) %>%
  summarise(
    current_medications = paste(medication[is_current], collapse = ", "),
    weened_medications = paste(medication[!is_current], collapse = ", "),
    number_current_medications = sum(is_current),
    number_weened_medications = sum(!is_current),
    .groups = "drop"
  )

seizures_summary_for_gaps <- seizures_summary_combined %>%
  mutate(medication = normalize_medication(medication))

gap_medication_summary <- get_gap_medications(
  seizure_gaps,
  seizures_summary_for_gaps
)

follow_up_summary <- appointment_summary %>%
  transmute(
    patient_uuid,
    follow_up = round(pmax(last_appointment - first_appointment, 0), 2)
  )

combined_df <- seizure_counts %>%
  left_join(seizure_gaps, by = "patient_uuid") %>%
  left_join(medication_summary, by = "patient_uuid") %>%
  left_join(current_weaned_summary, by = "patient_uuid") %>%
  left_join(gap_medication_summary, by = "patient_uuid") %>%
  left_join(follow_up_summary, by = "patient_uuid") %>%
  mutate(
    number_med_types_gap = if_else(
      is.na(number_med_types_gap),
      "None",
      as.character(number_med_types_gap)
    ),
    med_types_gap = replace_na(med_types_gap, "None"),
    current_medications = na_if(current_medications, ""),
    weened_medications = na_if(weened_medications, ""),
    current_medications = replace_na(current_medications, "None"),
    weened_medications = replace_na(weened_medications, "None"),
    gap_period = str_replace_all(gap_period, " - ", " to ")
  ) %>%
  arrange(patient_uuid)

output_path <- file.path(RESULTS, "combined_longitudinal_table.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(combined_df, output_path, row.names = FALSE)

message("Longitudinal patient table written to: ", normalizePath(output_path))
