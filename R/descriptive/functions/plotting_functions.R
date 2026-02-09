# R/functions/plotting_functions.R
# Functions for plotting

library(ggplot2)
library(ggh4x)
library(dplyr)
library(tidyr)
library(stringr)
library(gridExtra)
library(ggpubr)
library(grid)
library(png)
library(RColorBrewer)
library(ggstatsplot)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))

# Constants
MED_COLORS <- RColorBrewer::brewer.pal(10, "Set3")
SEIZURE_TYPE_COLORS <- c("#5698a3", "#ffde76", "#67771a", "#0076c0", "#e37c1d", "#7a5072")

HOSPITAL_LINE_COLORS <- c("Seizure" = "#a30234", 
                          "Status epilepticus" = "#7a5072", 
                          "Pulmonary" = "#67771a", 
                          "Infection" = "#0076c0", 
                          "GI" = "#e37c1d")
HOSPITAL_LINE_TYPES  <- c("Seizure" = "solid", 
                          "Status epilepticus" = "solid", 
                          "Pulmonary" = "dotted", 
                          "Infection" = "dotdash", 
                          "GI" = "dashed")

ANATOMY_IMG_PATH <- file.path("data", "raw", "anatomy.png")

HEATMAP_LOW_COLOR  <- "#083681"
HEATMAP_MID_COLOR  <- "#F7F7F7"
HEATMAP_HIGH_COLOR <- "#C80813FF"

# Plot seizure types over age
plot_seizure_types_over_age <- function(norm_df, xlim = c(0, 10), ylim = c(0, 1)) {
  p <- ggplot(norm_df, aes(x = age_year, y = prop, fill = type)) +
    geom_area(stat = "smooth", method = "loess", position = "identity") +
    geom_line(stat = "smooth", method = "loess", formula = y ~ x, se = FALSE, linetype = 1,
              aes(color = type), show.legend = FALSE) +
    labs(x = "Age (Years)", y = "Proportion", fill = "Seizure Type") +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    scale_x_continuous(breaks = 0:10) +
    theme_classic() +
    theme(legend.justification = c(0.05, 1), legend.position = c(0.05, 1)) +
    scale_fill_manual(values = SEIZURE_TYPE_COLORS) +
    scale_color_manual(values = SEIZURE_TYPE_COLORS)
  return(p)
}

# Plot hospitalization bar plots (Figure 2C)
plot_hospitalization_barplots <- function(other_freq, seizure_freq, specific_freq) {
  p1 <- ggplot(other_freq, aes(x = `Admission Type`, y = n, fill = Subgroup)) +
    geom_bar(stat = "identity") +
    labs(x = "Admission Type", y = "# of Reports", fill = "Subgroup") +
    theme_classic() +
    scale_fill_manual(values = c("#ffde76", "#bacfec", "#8A9197FF", "#f1b682", 
                                 "#e37c1d", "#abb47d", "#a30234", "#67771a", 
                                 "#e4b8b4", "#5698a3")) +
    ggtitle("Other")
  
  p1 <- p1 + 
    geom_text(data = specific_freq, aes(label = label, y = cumulative_n), 
              color = "white", size = 2.5) +
    geom_segment(data = specific_freq, 
                 aes(x = 0, xend = 1.5, y = cumulative_n - 3, yend = cumulative_n - 3),
                 color = "white")
  
  p2 <- ggplot(seizure_freq, aes(x = `Admission Type`, y = n, fill = admission_diagnosis)) +
    geom_bar(stat = "identity") +
    labs(x = "Admission Type", y = "# of Reports", fill = "Type") +
    theme_classic() +
    scale_fill_manual(values = c("#7a5072", "#002F30")) +
    ggtitle("Seizure")
  
  combined <- grid.arrange(p1, p2, ncol = 2)
  return(combined)
}

# Smooth line plot for hospitalization data (Figure 2D)
plot_hospitalization_lineplot <- function(hosp_data) {
  hosp_data <- hosp_data %>%
    mutate(age_years = admission_age_days_firstDate / 365) %>%
    mutate(Subgroup = ifelse(Subgroup == "Seizure" & admission_diagnosis == "Status epilepticus",
                             "Status epilepticus", 
                             ifelse(Subgroup == "Seizure", "Non-status epilepticus", Subgroup))) %>%
    mutate(Subgroup = ifelse(Subgroup == "Non-status epilepticus", "Seizure", Subgroup))
  
  patients_by_subgroup <- hosp_data %>%
    group_by(Subgroup, age_years_floor = floor(age_years)) %>%
    summarise(n = n_distinct(patient_uuid), .groups = "drop")
  
  patients_total <- hosp_data %>%
    group_by(age_years_floor = floor(age_years)) %>%
    summarise(total = n_distinct(patient_uuid), .groups = "drop")
  
  normalized_df <- left_join(patients_by_subgroup, patients_total, by = "age_years_floor") %>%
    mutate(prop = n / total) %>%
    ungroup() %>%
    na.omit()
  
  filtered_df <- normalized_df %>%
    filter(Subgroup %in% c("Seizure", "Status epilepticus", "Pulmonary", "Infection", "GI"))
  
  p <- ggplot(filtered_df, aes(x = age_years_floor, y = prop)) +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE,
                aes(color = Subgroup, linetype = Subgroup), method.args = list(span = 1)) +
    labs(x = "Age (years)", y = "Proportion", color = "Admission Type", linetype = "Admission Type") +
    coord_cartesian(xlim = c(0, 10), ylim = c(0, 1)) +
    scale_x_continuous(breaks = 0:10) +
    theme_classic() +
    theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
    scale_color_manual(values = HOSPITAL_LINE_COLORS) +
    scale_linetype_manual(values = HOSPITAL_LINE_TYPES)
  return(p)
}

# Plot diagnoses by body system (Figure 3)
plot_diagnoses_by_system <- function(sys_pcts, diagnosis_pcts, output_file) {
  colors <- c("#a30234", "#e4b8b4", "#e37c1d", "#bacfec", "#ffde76", lighten("#00545f", 0.25),
              "#0076c0", lighten("#67771a", 0.25), "#abb47d", "#a1c5fb", "#7a5072")
  unique_systems <- c("Musculoskeletal", "Gastrointestinal", "Behavioral", "Neurological", 
                      "Sensory", "Respiratory", "Cardiovascular", "Immunological")
  
  img <- readPNG(ANATOMY_IMG_PATH)
  
  sys_dfs <- list()
  for (i in seq_along(unique_systems)) {
    system <- unique_systems[i]
    system_pct <- sys_pcts %>% filter(System == system) %>% pull(sys_pct)
    system_pct <- round(system_pct, 2)
    system_pct <- paste0(system_pct, "%")
    
    diag_pct <- diagnosis_pcts %>% filter(System == system)
    diag_pct <- subset(diag_pct, select = -c(System))
    colnames(diag_pct) <- c("Diagnosis", "Percentage")
    diag_pct$Percentage <- as.numeric(diag_pct$Percentage)
    diag_pct <- diag_pct[order(-diag_pct$Percentage), ]
    if (nrow(diag_pct) > 7) {
      diag_pct <- diag_pct[1:7, ]
    }
    diag_pct$Percentage <- round(diag_pct$Percentage, 2)
    diag_pct$Percentage <- paste0(diag_pct$Percentage, "%")
    
    df <- data.frame(Diagnosis = system, Percentage = system_pct, stringsAsFactors = FALSE)
    df <- rbind(df, diag_pct)
    colnames(df) <- as.character(unlist(df[1,]))
    df <- df[-1, ]
    sys_dfs[[system]] <- df
  }
  
  jpeg(filename = output_file, width = 12, height = 14, units = "in", res = 600)
  
  pushViewport(viewport(layout = grid.layout(5, 3, 
                                             widths = unit(c(0.325, 0.3, 0.25), "npc"), 
                                             heights = unit(c(0.175, 0.175, 0.175, 0.175, 0.135), "npc"))))
  
  for (i in seq_along(unique_systems)) {
    system <- unique_systems[i]
    
    if (system %in% c("Neurological", "Sensory", "Gastrointestinal")) {
      table <- ggtexttable(sys_dfs[[system]], rows = NULL, theme = ttheme(
        colnames.style = colnames_style(color = "black", fill = colors[i]),
        tbody.style = tbody_style(color = "black", fill = lighten(colors[i], 0.5))
      ))
    } else {
      table <- ggtexttable(sys_dfs[[system]], rows = NULL, theme = ttheme(
        colnames.style = colnames_style(color = "white", fill = colors[i]),
        tbody.style = tbody_style(color = "black", fill = lighten(colors[i], 0.5))
      ))
    }
    
    if (system == "Neurological") {
      row <- 1; col <- 3
    } else if (system == "Immunological") {
      row <- 1; col <- 2
    } else if (system == "Cardiovascular") {
      row <- 2; col <- 3
    } else if (system == "Respiratory") {
      row <- 3; col <- 3
    } else if (system == "Sensory") {
      row <- 4; col <- 3
    } else {
      row <- ((i - 1) %% 3) + 2
      col <- ceiling(i / 3)
      if (col == 2) {
        col <- 3
      } else if (col == 3) {
        col <- 1
      }
    }
    
    if (!(row == 1 && col == 1)) {
      print(table, vp = viewport(layout.pos.row = row, layout.pos.col = col))
    }
  }
  
  grid.raster(img, width = unit(1, "npc"), height = unit(1, "npc"),
              vp = viewport(layout.pos.row = 2:4, layout.pos.col = 2))
  dev.off()
}

# Plot proportion of top medications over age
plot_medications_over_age <- function(norm_df,
                                      xlim = c(0, 10),
                                      ylim = c(0, 1),
                                      color_vals = MED_COLORS) {
  p <- ggplot(norm_df,
              aes(x = age_year,
                  y = prop,
                  fill = medication)) +
    geom_area(stat = "smooth",
              method = "loess",
              position = "identity") +
    geom_line(stat = "smooth",
              method = "loess",
              formula = y ~ x,
              se = FALSE,
              aes(color = medication),
              show.legend = FALSE) +
    labs(x = "Age (Years)",
         y = "Proportion",
         fill = "Medication") +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    scale_x_continuous(breaks = 0:10) +
    theme_classic() +
    theme(legend.justification = c(1, 1),
          legend.position = c(1, 1)) +
    scale_fill_manual(values = color_vals) +
    scale_color_manual(values = color_vals)
  return(p)
}

# Box plot of medication durations
plot_medication_duration_boxplot <- function(df,
                                             color_vals = MED_COLORS) {
  {
    require(ggstatsplot)
    
    # Base plot
    p <- suppressWarnings(
      ggbetweenstats(
        data                 = df,
        x                    = medication,
        y                    = duration_months,
        type                 = "robust",
        ggtheme              = ggplot2::theme_classic()
      )
    )
    
    # Override fill/color w/ custom mapping
    p <- p +
      scale_fill_manual(values = color_vals, drop = FALSE) +
      scale_color_manual(values = color_vals, drop = FALSE) +
      labs(
        x = "Medication",
        y = "Duration on Medication (months)"
      ) +
      coord_cartesian(ylim = c(0, NA))
    
    return(p)
  }
}

# Helper function to create common color scale for heatmaps
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

# Heatmap for med and adverse effect associations
plot_med_ae_heatmap <- function(fisher_df,
                                sig_level   = 0.05,
                                fill_limits = NULL) {
  
  required <- c("medication", "adverse_effect", "log_or", "p_adj")
  stopifnot(all(required %in% names(fisher_df)))
  
  heat_df <- fisher_df %>%
    dplyr::mutate(
      p_label = ifelse(
        p_adj < sig_level,
        paste("p =", format(round(p_adj, 2), nsmall = 2)),
        ""
      )
    )
  
  if (is.null(fill_limits)) {
    finite_vals <- heat_df$log_or[is.finite(heat_df$log_or)]
    max_abs     <- max(abs(finite_vals), na.rm = TRUE)
    fill_limits <- c(-max_abs, max_abs)
  }
  
  ggplot2::ggplot(heat_df,
                  ggplot2::aes(x = adverse_effect,
                               y = medication,
                               fill = log_or)) +
    ggplot2::geom_tile(color = "white") +
    heatmap_scale(limits = fill_limits) +
    ggplot2::geom_text(
      data  = subset(heat_df, p_label != ""),
      ggplot2::aes(label = p_label),
      size  = 4,
      vjust = 0.5,
      color = "black"
    ) +
    ggplot2::labs(x = "Adverse Effect",
                  y = "Medication",
                  fill = "OR") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.ticks  = ggplot2::element_blank()
    )
}

# Δ z‑score trajectories centered on the WHO 50th percentile
plot_growth_delta <- function(df_growth,
                              palette_lines = c(male = "#0076c0",
                                                female = "#a30234"),
                              who_z50 = 0) {
  
  # Helper that computes LMS z‑score
  compute_z <- function(x, L, M, S) {
    ifelse(L == 0,
           log(x / M) / S,
           ((x / M)^L - 1) / (L * S))
  }
  
  # Add int month & year age variables
  df_growth <- df_growth %>%
    dplyr::mutate(
      age_month = round(age_days / 30.4375), 
      age_years = age_month / 12
    )
  
  # Join WHO LMS tables and calculate Δz (z – z50) for each growth param
  delta_df <- purrr::map_dfr(unique(df_growth$parameter), function(param) {
    lms_tbl <- load_who_lms(param) %>% 
      dplyr::rename(age_month = age) %>% 
      dplyr::mutate(
        sex = dplyr::if_else(sex == 1, "male", "female")  
      ) %>% 
      dplyr::distinct(sex, age_month, .keep_all = TRUE)
    
    df_param <- df_growth %>%
      dplyr::filter(parameter == param) %>%
      dplyr::left_join(lms_tbl, by = c("age_month", "sex"), relationship = "many-to-one") %>%
      dplyr::mutate(
        z       = compute_z(value, L, M, S),
        delta_z = z - who_z50,
        parameter = param
      )
    return(df_param)
  })
  
  # Aggregate within each age‑month, sex, and parameter
  summary_df <- delta_df %>%
    dplyr::group_by(sex, parameter, age_month, age_years) %>%
    dplyr::summarise(
      median = median(delta_z,  na.rm = TRUE),
      p25    = quantile(delta_z, 0.25, na.rm = TRUE),
      p75    = quantile(delta_z, 0.75, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Facet labels
  param_labels <- c(
    "Body weight"        = "Body weight Δz",
    "Body height"        = "Body height Δz",
    "Head circumference" = "Head circumference Δz"
  )
  
  ggplot2::ggplot(summary_df,
                  ggplot2::aes(x = age_years,
                               y = median,
                               colour = sex,
                               linetype = parameter)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_smooth(size = 1.2, se = FALSE) +
    ggplot2::scale_linetype_manual(
      values = c("Body weight"        = "solid",
                 "Body height"        = "dashed",
                 "Head circumference" = "dotted")
    ) +
    ggplot2::labs(x = "Age (years)",
                  y = "Δ z‑score (vs WHO 50th pct)",
                  colour = "Sex",
                  linetype = "Parameter") +
    ggplot2::scale_colour_manual(values = palette_lines,
                                 breaks = c("male", "female"),
                                 labels = c("Male", "Female")) +
    ggplot2::guides(linetype = ggplot2::guide_legend(override.aes = list(colour = "black"))) +
    ggplot2::theme_classic(base_size = 20) +
    ggplot2::theme(
      legend.position      = c(0.075, 0.01),
      legend.justification = c(0, 0),
      legend.text          = ggplot2::element_text(size = 12),
      legend.title         = ggplot2::element_text(size = 12),
      legend.spacing.y     = grid::unit(-0.5, "cm")
    )
}

# Boxplot for appointment-to-appointment gaps
plot_appointment_interval_boxplot <- function(distance_df) {
  {
    # Remove outliers
    df_clean <- distance_df %>%
      dplyr::group_by(half) %>%
      dplyr::mutate(
        Q1    = stats::quantile(dist_months, 0.25, na.rm = TRUE),
        Q3    = stats::quantile(dist_months, 0.75, na.rm = TRUE),
        IQR_v = Q3 - Q1,
        lower = Q1 - 1.5 * IQR_v,
        upper = Q3 + 1.5 * IQR_v
      ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(dist_months >= lower, dist_months <= upper) %>%
      dplyr::select(-Q1, -Q3, -IQR_v, -lower, -upper)
    
    ggbetweenstats(
      data                 = df_clean,
      x                    = half,
      y                    = dist_months,
      type                 = "nonparametric",
      pairwise.comparisons = FALSE,
      pairwise.display     = "none",
      mean.ci              = TRUE,
      mean.plotting        = FALSE,
      point.args           = list(alpha = 0),
      ggtheme              = theme_classic()
    ) +
      labs(
        x = "Dataset half",
        y = "Interval between appointments (months)"
      )
  }
}

# Boxplot for appointment counts per patient
plot_appointment_count_boxplot <- function(counts_df) {
  ggbetweenstats(
    data                 = counts_df,
    x                    = half,
    y                    = count,
    type                 = "nonparametric",
    pairwise.comparisons = FALSE,
    pairwise.display     = "none",
    mean.ci              = TRUE,
    ggtheme              = theme_classic()
  ) +
    labs(
      x = "Dataset half",
      y = "Number of appointments per patient"
    )
}