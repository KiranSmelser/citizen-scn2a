# R/data_import_functions.R
# Functions to import main datasets

library(readr)
library(readxl)

read_supp_data <- function() {
  data <- read_csv(PATH_SUPP_DATA, show_col_types = FALSE)
  data <- data[,-1]
  data
}

read_citizen_data <- function() {
  read_csv(PATH_CITIZEN_DATA, show_col_types = FALSE)
}

read_classifier <- function() {
  read_excel(PATH_CLASSIFIER)
}

read_medication_aggregate <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "medication_aggregate")
}

read_seizure_history <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "seizure_history")
}

read_clinical_diagnosis <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "clinical_diagnosis")
}

read_development_data <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "development")
}

read_adverse_effects <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "adverse_effects")
}

read_hospitalizations <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "hospital_admission")
}

read_eeg <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "diagnostic_procedures")
}

read_demographics <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "demographics")
}

read_growth <- function() {
  read_excel(PATH_CITIZEN_DATA, sheet = "growth_parameters")
}

read_overlap_patients <- function() {
  read_excel(PATH_OVERLAP_PATIENTS)
}

load_who_lms <- function(parameter) {
  param <- tolower(as.character(parameter))

  .read_lms <- function(path, sex_id) {
    df <- readxl::read_excel(path)

    age_col <- if ("Month" %in% names(df)) "Month" else
      if ("Age" %in% names(df)) "Age" else
      if ("Age (months)" %in% names(df)) "Age (months)" else
      names(df)[1]

    out <- data.frame(
      age = suppressWarnings(as.numeric(df[[age_col]])),
      L   = suppressWarnings(as.numeric(df[["L"]])),
      M   = suppressWarnings(as.numeric(df[["M"]])),
      S   = suppressWarnings(as.numeric(df[["S"]])),
      sex = as.integer(sex_id),
      stringsAsFactors = FALSE
    )

    out <- out[!is.na(out$age) & !is.na(out$L) & !is.na(out$M) & !is.na(out$S), ]
    out
  }

  if (param == "body weight") {
    boys <- .read_lms(PATH_WEIGHT_BOYS, 1L)
    girls <- .read_lms(PATH_WEIGHT_GIRLS, 2L)
    res <- rbind(boys, girls)
  } else if (param == "body height") {
    b_0_2 <- .read_lms(PATH_HEIGHT_BOYS_0_TO_2, 1L)
    b_2_5 <- .read_lms(PATH_HEIGHT_BOYS_2_TO_5, 1L)
    g_0_2 <- .read_lms(PATH_HEIGHT_GIRLS_0_TO_2, 2L)
    g_2_5 <- .read_lms(PATH_HEIGHT_GIRLS_2_TO_5, 2L)
    res <- rbind(b_0_2, b_2_5, g_0_2, g_2_5)
    dup <- duplicated(res[, c("sex", "age")], fromLast = TRUE)
    res <- res[!dup, ]
  } else if (param == "head circumference") {
    boys <- .read_lms(PATH_HEAD_BOYS, 1L)
    girls <- .read_lms(PATH_HEAD_GIRLS, 2L)
    res <- rbind(boys, girls)
  } else {
    stop(sprintf("Unknown parameter '%s' for WHO LMS loading", parameter))
  }

  res$age <- as.integer(round(res$age))
  res
}
