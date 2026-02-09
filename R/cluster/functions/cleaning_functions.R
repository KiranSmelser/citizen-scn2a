# R/cluster/functions/cleaning_functions.R
# Helpers used by clusters.R and age_med_feature_importance.R

library(dplyr)
library(stringr)
library(tibble)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))

# Merge overlapping intervals for medication duration data
merge_intervals <- function(intervals_df) {
  intervals_df <- intervals_df %>% arrange(start_med_age)

  if (nrow(intervals_df) == 0) {
    return(tibble(start_med_age = numeric(0), end_med_age = numeric(0)))
  }

  current_start <- intervals_df$start_med_age[1]
  current_end   <- intervals_df$end_med_age[1]
  merged_list   <- list()

  if (nrow(intervals_df) > 1) {
    for (i in seq(2, nrow(intervals_df))) {
      s <- intervals_df$start_med_age[i]
      e <- intervals_df$end_med_age[i]

      if (s <= current_end) {
        current_end <- max(current_end, e, na.rm = TRUE)
      } else {
        merged_list[[length(merged_list) + 1]] <- tibble(
          start_med_age = current_start,
          end_med_age   = current_end
        )
        current_start <- s
        current_end   <- e
      }
    }
  }

  merged_list[[length(merged_list) + 1]] <- tibble(
    start_med_age = current_start,
    end_med_age   = current_end
  )

  bind_rows(merged_list)
}

# Clean med data
clean_medication_data <- function() {
  df_med <- read_medication_aggregate()

  df_med <- df_med %>%
    mutate(
      medication = ifelse(grepl("ACTH", medication), "ACTH", medication),
      medication = recode(medication,
                          `Epidiolex` = "Epidiolex/CBD",
                          `Cannabidiol` = "Epidiolex/CBD")
    ) %>%
    filter(grepl(MEDS_TO_USE, medication))

  df_duration <- df_med %>%
    transmute(
      patient_uuid,
      medication,
      start_med_age = medication_age_days_firstDate,
      end_med_age   = medication_age_days_lastDate
    ) %>%
    distinct() %>%
    arrange(patient_uuid, medication, start_med_age, end_med_age) %>%
    group_by(patient_uuid, medication) %>%
    group_modify(~ merge_intervals(.x)) %>%
    ungroup() %>%
    filter((end_med_age - start_med_age) > 0) %>%
    group_by(patient_uuid, medication) %>%
    mutate(
      interval_id = row_number(),
      intervals_for_this_med = n()
    ) %>%
    ungroup() %>%
    mutate(
      medication = if_else(intervals_for_this_med > 1,
                           paste0(medication, " ", interval_id),
                           medication)
    )
  return(df_duration)
}