# R/odds-ratio/functions/plotting_functions.R
# Plotting helpers

library(ggplot2)
library(dplyr)


# Compute number of observations for a given predictor and outcome
compute_n_for_predictor <- function(data, results_df, predictor, outcome_col = "target") {
  n_values <- c()
  for (outcome in results_df[[outcome_col]]) {
    if (grepl("Onset_group", predictor)) {
      if (predictor == "Onset_group_(Intercept)") {
        # Use 1-4 Months as reference level
        subset_data <- data[data[["Onset_group"]] == "1-4 Months" & data[[outcome]] == 1, ]
      } else {
        level_val <- trimws(sub("Onset_group_", "", predictor))
        subset_data <- data[data[["Onset_group"]] == level_val & data[[outcome]] == 1, ]
      }
      n_values <- c(n_values, nrow(subset_data))
    } else if (grepl("unique_types", predictor)) {
      if (predictor == "unique_types_(Intercept)") {
        subset_data <- data[data[["unique_types"]] == "0" & data[[outcome]] == 1, ]
        n_values <- c(n_values, nrow(subset_data))
      } else {
        if (grepl("2\\+", predictor)) {
          subset_data <- data[data[["unique_types"]] == "2+" & data[[outcome]] == 1, ]
          n_values <- c(n_values, nrow(subset_data))
        } else {
          level_val <- trimws(sub("unique_types", "", predictor))
          subset_data <- data[data[["unique_types"]] == level_val & data[[outcome]] == 1, ]
          n_values <- c(n_values, nrow(subset_data))
        }
      }
    } else if (predictor == "age_onset_m") {
      subset_data <- data[data[[outcome]] == 1, ]
      n_values <- c(n_values, nrow(subset_data))
    } else {
      subset_data <- data[data[[predictor]] == 1 & data[[outcome]] == 1, ]
      n_values <- c(n_values, nrow(subset_data))
    }
  }
  results_df$n <- n_values
  return(results_df)
}

# Plot odds ratios for a given predictor across outcomes
plot_odds_ratio <- function(results_df, predictor_name, data, outcome_col = "target", outcome_label = "Outcome Type") {
  results_df <- compute_n_for_predictor(data, results_df, predictor_name, outcome_col)
  results_df$midpoint <- (results_df$ci_lower + results_df$ci_upper) / 2
  # Set color
  results_df$color <- ifelse(results_df$midpoint < 1, "#a30234", "#0076c0")
  results_df$color <- ifelse(results_df$p_value > 0.05, "transparent", results_df$color)

  p <- ggplot(results_df, aes(x = .data[[outcome_col]], y = midpoint, color = color)) +
    geom_point() +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    scale_color_identity() +
    coord_flip(clip = "off") +
    labs(title = predictor_name, x = outcome_label, y = "Odds Ratio") +
    theme_bw() +
    geom_text(aes(y = -Inf, label = paste("n =", as.character(n))),
              vjust = 2.5, hjust = 1.75, size = 3)
  return(p)
}