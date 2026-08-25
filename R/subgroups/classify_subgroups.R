#!/usr/bin/env Rscript

# Classify SCN2A patients according to the v2 clinical decision tree.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(readxl)
  library(stringr)
})

get_project_root <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", command_args, value = TRUE)

  if (length(file_arg) == 1L) {
    script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
    return(normalizePath(file.path(dirname(script_path), "..", ".."), mustWork = TRUE))
  }

  normalizePath(getwd(), mustWork = TRUE)
}

require_columns <- function(data, columns, data_name) {
  missing_columns <- setdiff(columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      data_name, " is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }
}

# A blank frequency accompanies event-only records and is evidence of a seizure.
# A numeric zero (for example, "0.0 Per Year") is not evidence of epilepsy.
is_epilepsy_record <- function(seizure_value) {
  value_text <- str_trim(as.character(seizure_value))
  value_number <- suppressWarnings(parse_number(value_text))

  is.na(value_text) | value_text == "" | (!is.na(value_number) & value_number > 0)
}

is_resolved_diagnosis <- function(diagnosis) {
  str_detect(
    coalesce(as.character(diagnosis), ""),
    regex("seizure[ -]?free|resolved epilepsy|epilepsy in remission", ignore_case = TRUE)
  )
}

is_infantile_spasm <- function(seizure_type) {
  str_detect(
    coalesce(as.character(seizure_type), ""),
    regex("^(infantile|epileptic) spasms?$", ignore_case = TRUE)
  )
}

classify_subgroup <- function(has_infantile_spasms, has_epilepsy, onset_days,
                              seizures_resolved, has_epileptiform_eeg) {
  case_when(
    has_infantile_spasms ~ "IS",
    has_epilepsy & is.na(onset_days) ~ NA_character_,
    has_epilepsy & onset_days < 30 & seizures_resolved ~ "BFNIE",
    has_epilepsy & onset_days < 30 ~ "EO-DEE",
    has_epilepsy & onset_days < 360 ~ "LO-MI",
    has_epilepsy ~ "LO-C",
    !has_epilepsy & has_epileptiform_eeg ~ "ASD-EEG*",
    !has_epilepsy ~ "ASD/ID",
    TRUE ~ NA_character_
  )
}

build_subgroups <- function(demographics, seizure_history, clinical_diagnosis,
                            diagnostic_procedures, classifier) {
  require_columns(demographics, "patient_uuid", "demographics")
  require_columns(
    seizure_history,
    c(
      "patient_uuid", "seizure_history_type", "seizure_history_value",
      "seizure_history_age_days"
    ),
    "seizure_history"
  )
  require_columns(
    clinical_diagnosis,
    c("patient_uuid", "clinical_diagnosis"),
    "clinical_diagnosis"
  )
  require_columns(
    diagnostic_procedures,
    c("patient_uuid", "procedure", "procedure_findings"),
    "diagnostic_procedures"
  )
  require_columns(classifier, "eeg_epileptiform", "classifier")

  patient_ids <- demographics %>%
    transmute(patient_uuid = as.character(patient_uuid)) %>%
    filter(!is.na(patient_uuid), patient_uuid != "") %>%
    distinct()

  epilepsy_summary <- seizure_history %>%
    mutate(
      patient_uuid = as.character(patient_uuid),
      epilepsy_record = is_epilepsy_record(seizure_history_value),
      infantile_spasm = is_infantile_spasm(seizure_history_type),
      onset_days = suppressWarnings(as.numeric(seizure_history_age_days))
    ) %>%
    group_by(patient_uuid) %>%
    summarise(
      has_epilepsy = any(epilepsy_record, na.rm = TRUE),
      has_infantile_spasms = any(infantile_spasm, na.rm = TRUE),
      onset_days = if (any(epilepsy_record & !is.na(onset_days))) {
        min(onset_days[epilepsy_record], na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  resolution_summary <- clinical_diagnosis %>%
    mutate(
      patient_uuid = as.character(patient_uuid),
      resolved = is_resolved_diagnosis(clinical_diagnosis)
    ) %>%
    group_by(patient_uuid) %>%
    summarise(seizures_resolved = any(resolved, na.rm = TRUE), .groups = "drop")

  epileptiform_findings <- classifier$eeg_epileptiform %>%
    as.character() %>%
    str_trim()
  epileptiform_findings <- epileptiform_findings[
    !is.na(epileptiform_findings) & epileptiform_findings != ""
  ]

  eeg_summary <- diagnostic_procedures %>%
    mutate(
      patient_uuid = as.character(patient_uuid),
      is_eeg = str_detect(coalesce(as.character(procedure), ""),
                          regex("EEG|electroencephal", ignore_case = TRUE)),
      epileptiform = is_eeg & procedure_findings %in% epileptiform_findings
    ) %>%
    group_by(patient_uuid) %>%
    summarise(
      has_epileptiform_eeg = any(epileptiform, na.rm = TRUE),
      .groups = "drop"
    )

  patient_ids %>%
    left_join(epilepsy_summary, by = "patient_uuid") %>%
    left_join(resolution_summary, by = "patient_uuid") %>%
    left_join(eeg_summary, by = "patient_uuid") %>%
    mutate(
      has_epilepsy = coalesce(has_epilepsy, FALSE),
      has_infantile_spasms = coalesce(has_infantile_spasms, FALSE),
      seizures_resolved = coalesce(seizures_resolved, FALSE),
      has_epileptiform_eeg = coalesce(has_epileptiform_eeg, FALSE),
      subgroup = classify_subgroup(
        has_infantile_spasms,
        has_epilepsy,
        onset_days,
        seizures_resolved,
        has_epileptiform_eeg
      )
    ) %>%
    select(patient_uuid, subgroup) %>%
    arrange(patient_uuid)
}

main <- function() {
  project_root <- get_project_root()
  citizen_path <- file.path(
    project_root, "data", "raw", "Citizen_SCN2A_UArizona_2025.07.xlsx"
  )
  classifier_path <- file.path(
    project_root, "data", "classifiers", "ciitizen_health_classifier.xlsx"
  )
  output_path <- file.path(
    project_root, "data", "classifiers", "subgroups_v2.csv"
  )

  for (path in c(citizen_path, classifier_path)) {
    if (!file.exists(path)) {
      stop("Input file not found: ", path, call. = FALSE)
    }
  }

  subgroups <- build_subgroups(
    demographics = read_excel(citizen_path, sheet = "demographics"),
    seizure_history = read_excel(citizen_path, sheet = "seizure_history"),
    clinical_diagnosis = read_excel(citizen_path, sheet = "clinical_diagnosis"),
    diagnostic_procedures = read_excel(citizen_path, sheet = "diagnostic_procedures"),
    classifier = read_excel(classifier_path)
  )

  if (anyNA(subgroups$subgroup)) {
    missing_ids <- subgroups$patient_uuid[is.na(subgroups$subgroup)]
    stop(
      "Unable to classify patient(s) with epilepsy but no seizure onset: ",
      paste(missing_ids, collapse = ", "),
      call. = FALSE
    )
  }

  write_csv(subgroups, output_path)
  message("Wrote ", nrow(subgroups), " patient classifications to ", output_path)
  invisible(subgroups)
}

if (sys.nframe() == 0L) {
  main()
}
