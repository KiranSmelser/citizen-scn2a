# R/timeline/functions/plotting_functions.R
# Functions for plotting

library(ggplot2)
library(dplyr)

# Creates patient charts
plot_patient_chart <- function(patient_data) {
  # Abbreviate seizure types
  patient_data$pt_data_type <- patient_data$pt_data_type %>%
    mutate(type = recode(type, !!!ABBREVIATIONS_SEIZURES))
  
  # Abbreviate med names on timeline plot
  patient_data$pt_data_duration <- patient_data$pt_data_duration %>%
    mutate(medication_base = recode(medication_base, !!!ABBREVIATIONS_MEDS))
  
  # Abbreviate med order vector for y-axis
  patient_data$med_order <- recode(patient_data$med_order, !!!ABBREVIATIONS_MEDS)

  # Keep adverse effects aligned to medication rows after abbreviation
  patient_data$pt_data_adverse <- patient_data$pt_data_adverse %>%
    mutate(medication_base = recode(medication_base, !!!ABBREVIATIONS_MEDS))
  
  # Classify EEG events (normal vs. abnormal)
  classifier_eeg <- read_classifier()
  patient_data$pt_eeg <- patient_data$pt_eeg %>%
    mutate(eeg_status = ifelse(procedure_findings %in% classifier_eeg$eeg_normal, "Normal", "Abnormal"))
  # Mark all hypsarrhythmia as abnormal
  patient_data$pt_hyps <- patient_data$pt_hyps %>%
    mutate(eeg_status = "Abnormal")

  ae_levels <- sort(unique(patient_data$pt_data_adverse$adverse_effect))
  ae_shape_pool <- c(15, 16, 17, 18, 0, 1, 2, 3, 4, 5, 6, 7, 8)
  ae_shape_values <- setNames(
    rep(ae_shape_pool, length.out = length(ae_levels)),
    ae_levels
  )

  has_ispm <- nrow(patient_data$pt_spasm_periods) > 0
  has_se <- nrow(patient_data$pt_data_status) > 0

  y_levels <- unique(c(
    "APPT",
    if (has_se) "SE",
    "EEG",
    if (has_ispm) "ISPM",
    rev(unique(patient_data$pt_data_type$type)),
    rev(patient_data$med_order)
  ))
  y_index <- setNames(seq_along(y_levels), y_levels)
  eeg_row <- unname(y_index[["EEG"]])
  ispm_row <- if (has_ispm) unname(y_index[["ISPM"]]) else NA_real_
  se_row <- if (has_se) unname(y_index[["SE"]]) else NA_real_
  appt_row <- unname(y_index[["APPT"]])

  pt_data_duration_plot <- patient_data$pt_data_duration %>%
    mutate(y_row = unname(y_index[medication_base]))
  pt_data_type_plot <- patient_data$pt_data_type %>%
    mutate(y_row = unname(y_index[type]))
  pt_spasm_periods_plot <- patient_data$pt_spasm_periods
  pt_eeg_plot <- patient_data$pt_eeg
  pt_hyps_plot <- patient_data$pt_hyps
  pt_data_status_plot <- patient_data$pt_data_status
  appointment_data_plot <- patient_data$appointment_data
  pt_data_adverse_plot <- patient_data$pt_data_adverse %>%
    mutate(y_row = unname(y_index[medication_base]))
  
  # Timeline plot
  p_timeline <- ggplot() +
    geom_segment(
      data = pt_data_duration_plot,
      aes(x = start_med_age_months, xend = first_3_months_end,
          y = y_row, yend = y_row),
      linewidth = 2, color = "#8A9197FF"
    ) +
    geom_segment(
      data = pt_data_duration_plot,
      aes(x = first_3_months_end, xend = end_med_age_months,
          y = y_row, yend = y_row),
      linewidth = 2, color = "#709AE1FF"
    ) +
    geom_point(
      data = pt_data_type_plot,
      aes(x = age_months, y = y_row),
      color = "#C80813FF", size = patient_data$pt_data_type$index + 1, alpha = 0.6
    ) +
    geom_segment(
      data = pt_eeg_plot,
      aes(x = age_months, xend = age_months, y = eeg_row, yend = eeg_row + 0.24, color = eeg_status),
      linewidth = 1.1, alpha = 0.95, lineend = "round"
    ) +
    geom_segment(
      data = pt_hyps_plot,
      aes(x = age_months, xend = age_months, y = eeg_row, yend = eeg_row - 0.24, color = eeg_status),
      linewidth = 1.4, alpha = 1, lineend = "round"
    ) +
    scale_color_manual(values = c(
      "Normal"   = "dodgerblue1",
      "Abnormal" = "tomato"
    ), guide = "none") +
    geom_point(
      data = appointment_data_plot,
      aes(x = appointment_age_months, y = appt_row),
      color = "#1A9993FF", size = 3, shape = 17, alpha = 0.6
    ) +
    theme_linedraw() +
    labs(
      title = patient_data$timeline_title,
      x = "Age (months)",
      y = ""
    ) +
    scale_y_continuous(
      breaks = seq_along(y_levels),
      labels = y_levels,
      minor_breaks = NULL
    ) +
    theme(panel.grid.minor.y = element_blank())

  if (has_ispm) {
    p_timeline <- p_timeline +
      geom_segment(
        data = pt_spasm_periods_plot,
        aes(x = spasm_start_age, xend = spasm_end_age, y = ispm_row, yend = ispm_row),
        linewidth = 2, color = "#C80813FF"
      ) +
      geom_point(
        data = pt_spasm_periods_plot %>% filter(is_single_report),
        aes(x = spasm_start_age, y = ispm_row),
        size = 2, color = "#C80813FF", shape = 15
      )
  }

  if (has_se) {
    p_timeline <- p_timeline +
      geom_point(
        data = pt_data_status_plot,
        aes(x = age_months, y = se_row),
        color = "#FED439FF", size = 5, shape = 18, alpha = 0.9
      )
  }

  if (nrow(pt_data_adverse_plot) > 0) {
    p_timeline <- p_timeline +
      geom_point(
        data = pt_data_adverse_plot,
        aes(x = age_months, y = y_row, shape = adverse_effect),
        color = "#FD7446FF", size = 2.5, alpha = 0.85,
        position = position_jitter(width = 0.15, height = 0.08, seed = 1)
      ) +
      scale_shape_manual(
        values = ae_shape_values,
        name = "Adverse effects"
      )
  }

  return(p_timeline)
}
