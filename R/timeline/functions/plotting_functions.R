# R/timeline/functions/plotting_functions.R
# Functions for plotting

library(ggplot2)
library(ggh4x)
library(dplyr)
library(tidyr)

# Constants
HEATMAP_LOW_COLOR  <- "#083681"
HEATMAP_MID_COLOR  <- "#F7F7F7"
HEATMAP_HIGH_COLOR <- "#C80813FF"

# Helper function to create heatmap color scale
heatmap_scale <- function(limits = NULL) {
  scale_fill_gradient2(
    low = HEATMAP_LOW_COLOR,
    mid = HEATMAP_MID_COLOR,
    high = HEATMAP_HIGH_COLOR,
    midpoint = 0,
    na.value = HEATMAP_MID_COLOR,
    limits = limits
  )
}

# Create combined heatmap
create_combined_heatmap_modified <- function(data, limits = NULL) {
  data_long <- data %>%
    pivot_longer(
      cols = c(diff_on_vs_before, diff_on_vs_after),
      names_to = "comparison",
      values_to = "diff_value"
    ) %>%
    mutate(
      comparison = recode(
        comparison,
        diff_on_vs_before = "Before",
        diff_on_vs_after  = "After"
      ),
      comparison = factor(comparison, levels = c("Before", "After"))
    )
  
  # Build combined x-axis
  data_long <- data_long %>%
    mutate(x_axis = paste(comparison, type, sep = "|"))
  
  # Define factor levels
  seizure_types <- unique(data_long$type)
  x_levels <- unlist(lapply(seizure_types, function(t) {
    c(paste("Before", t, sep = "|"), paste("After", t, sep = "|"))
  }))
  data_long$x_axis <- factor(data_long$x_axis, levels = x_levels)
  
  # Positions for vertical divider lines
  n_groups <- length(seizure_types)
  vline_positions <- if (n_groups > 1) sapply(1:(n_groups - 1), function(i) i * 2 + 0.5) else NULL
  
  ggplot(data_long, aes(x = x_axis, y = medication, fill = diff_value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(diff_value, 2)), size = 3, color = "black", na.rm = TRUE) +
    geom_vline(xintercept = vline_positions, linetype = "solid", color = "black", size = 1) +
    heatmap_scale(limits) +
    theme_classic() +
    labs(
      title = "Seizure Index Comparisons",
      x = "Comparison and Seizure Type",
      y = "Medication",
      fill = "Change in Seizure Index"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(guide = ggh4x::guide_axis_nested(delim = "|"))
}

# Creates patient charts
plot_patient_chart <- function(patient_data) {
  # Abbreviate seizure types
  patient_data$pt_data_type <- patient_data$pt_data_type %>%
    mutate(type = recode(type, !!!ABBREVIATIONS_SEIZURES))
  
  # Abbreviate names in heatmap summary
  patient_data$pt_data <- patient_data$pt_data %>%
    mutate(
      type = recode(type, !!!ABBREVIATIONS_SEIZURES),
      base_med = sub(" \\d+$", "", medication),
      id = str_extract(medication, "\\d+$"),
      medication = ifelse(
        !is.na(id),
        paste0(recode(base_med, !!!ABBREVIATIONS_MEDS), " ", id),
        recode(base_med, !!!ABBREVIATIONS_MEDS)
      )
    ) %>%
    select(-base_med, -id)
  
  # Abbreviate med names on timeline plot
  patient_data$pt_data_duration <- patient_data$pt_data_duration %>%
    mutate(medication_base = recode(medication_base, !!!ABBREVIATIONS_MEDS))
  
  # Abbreviate med order vector for y-axis
  patient_data$med_order <- recode(patient_data$med_order, !!!ABBREVIATIONS_MEDS)
  
  # Classify EEG events (normal vs. abnormal)
  classifier_eeg <- read_classifier()
  patient_data$pt_eeg <- patient_data$pt_eeg %>%
    mutate(eeg_status = ifelse(procedure_findings %in% classifier_eeg$eeg_normal, "Normal", "Abnormal"))
  # Mark all hypsarrhythmia as abnormal
  patient_data$pt_hyps <- patient_data$pt_hyps %>%
    mutate(eeg_status = "Abnormal")
  # Line plot for seizure index over time
  p_line <- ggplot(patient_data$pt_data_type, aes(x = age_months, y = index, color = type)) +
    geom_point(size = 2, alpha = 0.8) +
    geom_line(linetype = "dashed", alpha = 0.8) +
    facet_grid(type ~ .) +
    theme_light() +
    labs(x = "Age (months)", y = "Seizure Index", color = "Seizure Type") +
    guides(color = "none")
  
  # Timeline plot
  p_timeline <- ggplot() +
    geom_segment(
      data = patient_data$pt_data_duration,
      aes(x = start_med_age_months, xend = first_3_months_end,
          y = medication_base, yend = medication_base),
      size = 2, color = "#8A9197FF"
    ) +
    geom_segment(
      data = patient_data$pt_data_duration,
      aes(x = first_3_months_end, xend = end_med_age_months,
          y = medication_base, yend = medication_base),
      size = 2, color = "#709AE1FF"
    ) +
    geom_point(
      data = patient_data$pt_data_type,
      aes(x = age_months, y = type),
      color = "#C80813FF", size = patient_data$pt_data_type$index + 1, alpha = 0.6
    ) +
    geom_segment(
      data = patient_data$pt_spasm_periods,
      aes(x = spasm_start_age, xend = spasm_end_age, y = "ISPM", yend = "ISPM"),
      size = 2, color = "#C80813FF"
    ) +
    geom_point(
      data = patient_data$pt_spasm_periods %>% filter(is_single_report),
      aes(x = spasm_start_age, y = "ISPM"),
      size = 2, color = "#C80813FF", shape = 15
    ) +
    geom_point(
      data = patient_data$pt_eeg,
      aes(x = age_months, y = "EEG", color = eeg_status),
      size = 3, shape = 124, position = position_nudge(y = 0.14)
    ) +
    geom_point(
      data = patient_data$pt_hyps,
      aes(x = age_months, y = "EEG", color = eeg_status),
      size = 3, shape = 124, position = position_nudge(y = -0.16)
    ) +
    geom_point(
      data = patient_data$pt_data_adverse,
      aes(x = age_months, y = "AE"),
      color = "#FD7446FF", size = 2, shape = 15, alpha = 0.8
    ) +
    geom_point(
      data = patient_data$pt_data_status,
      aes(x = age_months, y = "SE"),
      color = "#FED439FF", size = 5, shape = 18, alpha = 0.9
    ) +
    scale_color_manual(values = c(
      "Normal"   = "dodgerblue1",
      "Abnormal" = "tomato"
    ), guide = "none") +
    geom_point(
      data = patient_data$appointment_data,
      aes(x = appointment_age_months, y = "APPT"),
      color = "#1A9993FF", size = 3, shape = 17, alpha = 0.6
    ) +
    theme_linedraw() +
    labs(
      title = patient_data$timeline_title,
      x = "Age (months)",
      y = ""
    ) +
    scale_y_discrete(limits = c(
      "APPT", "SE",
      "EEG", "ISPM",
      rev(unique(patient_data$pt_data_type$type)),
      "AE",
      rev(patient_data$med_order)
    ))
  
  # Compute legend limits for heatmap
  legend_limits <- max(abs(patient_data$pt_data$diff_on_vs_after),
                       abs(patient_data$pt_data$diff_on_vs_before), na.rm = TRUE) * c(-1, 1)
  
  combined_heatmap <- create_combined_heatmap_modified(patient_data$pt_data, limits = legend_limits)
  
  combined_plot <- (p_line | combined_heatmap) /
    p_timeline + patchwork::plot_layout(heights = c(1, 1))
  
  return(combined_plot)
}
