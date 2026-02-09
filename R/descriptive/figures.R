# R/descriptive/figures.R
# Generates figures

library(dplyr)
library(tidyr)
library(ggplot2)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "descriptive", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "descriptive", "functions", "plotting_functions.R"))

# Figure 2
# --------

# Figure 2B

# Data Cleaning and Setup
df_seizure <- clean_seizure_data(include_spasms = TRUE)

df_seizure <- df_seizure %>%
  mutate(age_years = age_days / 365)

df_seizure <- df_seizure %>% filter(tolower(type) != "clonic")

# Count unique patients per seizure type at each age
patients_by_type <- df_seizure %>%
  group_by(type, age_year = floor(age_years)) %>%
  summarise(n = n_distinct(patient_uuid), .groups = "drop")

# Calculate total patients at each age
patients_total <- df_seizure %>%
  group_by(age_year = floor(age_years)) %>%
  summarise(total = n_distinct(patient_uuid), .groups = "drop")

# Join counts and compute proportion
normalized_df <- left_join(patients_by_type, patients_total, by = "age_year") %>%
  mutate(prop = n / total)

# Define and adjust factor levels for seizure type
seizure_levels <- c("Tonic-clonic", "Focal", "Tonic", "Myoclonic", "Absence", "Spasms")
normalized_df$type <- factor(normalized_df$type, levels = seizure_levels)

# Generate plot
p_seizure <- plot_seizure_types_over_age(normalized_df)
ggsave(filename = "output/figures/fig_2b.jpeg", plot = p_seizure, width = 10, height = 7, dpi = 600)

# Figure 2C

# Data Cleaning and Setup
hosp_data <- clean_hospitalization_data()

# Calculate frequency counts for hospitalization groups
freq_list <- calculate_hospitalization_frequencies(hosp_data)

# Generate combined bar plot
combined_plot <- plot_hospitalization_barplots(freq_list$other_freq, 
                                               freq_list$seizure_freq, 
                                               freq_list$specific_freq)

ggsave(filename = "output/figures/fig_2c.jpeg", plot = combined_plot, width = 10, height = 7, dpi = 600)

# Figure 2D

# Data Cleaning and Setup
hosp_data <- clean_hospitalization_data(select_cols = c(1:2, 10))

# Generate smooth line plot for hospitalizations
p_line <- plot_hospitalization_lineplot(hosp_data)

ggsave(filename = "output/figures/fig_2d.jpeg", plot = p_line, width = 10, height = 7, dpi = 600)

# Figure 3
# --------

# Data Cleaning and Setup
diagnosis_clean <- clean_diagnoses_data()
diagnoses <- diagnosis_clean$diagnoses
sys_pcts <- diagnosis_clean$sys_pcts
diagnosis_pcts <- diagnosis_clean$diagnosis_pcts

# Generate diagnoses by system figure 
plot_diagnoses_by_system(sys_pcts, diagnosis_pcts, file.path("output", "figures", "fig_3.jpeg"))

# Figure 4
# --------

# Data Cleaning and Setup
df_med_duration <- clean_medication_data()

# Prepare top medications over age
prep_med4 <- prepare_top_medications_over_age(df_med_duration)
normalized_df  <- prep_med4$normalized_df
present_levels <- prep_med4$present_levels

# Align colors with ordered levels
med_colors_map <- setNames(MED_COLORS[seq_along(present_levels)], present_levels)

# Generate plot
p_medications <- plot_medications_over_age(normalized_df,
                                           ylim = c(0, 0.75),
                                           color_vals = med_colors_map)

ggsave(
  filename = file.path("output", "figures", "fig_4.jpeg"),
  plot     = p_medications,
  width    = 10,
  height   = 7,
  dpi      = 600
)

# Figure 5
# --------

df_durations <- prepare_medication_durations(df_med_duration, present_levels)

# Generate box plot
p_med_duration <- plot_medication_duration_boxplot(df_durations,
                                                   color_vals = med_colors_map)

ggsave(
  filename = file.path("output", "figures", "fig_5.jpeg"),
  plot     = p_med_duration,
  width    = 10,
  height   = 7,
  dpi      = 600
)


# Figure 5a

cat_prep <- prepare_medication_category_durations(df_med_duration)
df_plot_cat     <- cat_prep$df_plot_cat
category_levels <- cat_prep$category_levels

# Color palette for 6 categories
library(RColorBrewer)
cat_colors <- setNames(RColorBrewer::brewer.pal(length(category_levels), "Set2"),
                       category_levels)

# Generate box plot
p_med_cat <- plot_medication_duration_boxplot(
  df_plot_cat,
  color_vals = cat_colors
)

ggsave(
  filename = file.path("output", "figures", "fig_5a.jpeg"),
  plot     = p_med_cat,
  width    = 10,
  height   = 7,
  dpi      = 600
)

# Figure 6
# --------
# Medication x adverse effect associations

# Build matrices & Fisher tests
ae_med_res <- build_med_ae_fisher()

# Heat‑map of log2(OR) with significant cells marked
p_ae_med <- plot_med_ae_heatmap(ae_med_res$fisher_df)

ggsave(
  filename = file.path("output", "figures", "fig_6.jpeg"),
  plot     = p_ae_med,
  width    = 12,
  height   = 8,
  dpi      = 600
)

# Figure 11
# ---------
# Δz‑score trajectories centered on WHO 50th percentile

df_growth <- clean_growth_data()

p_delta90 <- plot_growth_delta(df_growth)

ggsave(
  filename = file.path("output", "figures", "fig_11.jpeg"),
  plot     = p_delta90,
  width    = 10,
  height   = 6,
  dpi      = 600
)

# Figure 12
# ---------
# Appointment-interval distribution (1st vs. 2nd half)
apt_clean  <- clean_appointment_data()

p_appt_int <- plot_appointment_interval_boxplot(apt_clean$distance_df)

ggsave(
  filename = file.path("output", "figures", "fig_12.jpeg"),
  plot     = p_appt_int,
  width    = 8,
  height   = 6,
  dpi      = 600
)

# Figure 13
# ---------
# Appointment counts per patient (1st vs. 2nd half)
p_appt_cnt <- plot_appointment_count_boxplot(apt_clean$counts_df)

ggsave(
  filename = file.path("output", "figures", "fig_13.jpeg"),
  plot     = p_appt_cnt,
  width    = 8,
  height   = 6,
  dpi      = 600
)