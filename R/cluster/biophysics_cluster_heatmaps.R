# R/cluster/biophysics_cluster_heatmaps.R
# Build age-cluster vs biophysics correlation heatmaps (Neonatal and Adult).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readxl)
  library(ggplot2)
})

source(file.path(".", "R", "config.R"))

run_suffix <- "all_patients"
time_labels <- c("1yr", "3yr", "5yr", "8yr", "10yr")

cluster_files <- setNames(
  file.path(DATA_PROCESSED, run_suffix, paste0(time_labels, "_clusters.csv")),
  time_labels
)

fig_dir <- file.path(FIGS, "clusters", run_suffix)

dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

biophys_properties <- c(
  "Current Density",
  "Voltage Dependence of Activation",
  "Voltage Dependence of Inactivation",
  "Recovery from Inactivation tau",
  "Use-dependent Run-down",
  "Inactivation Kinetics",
  "Ramp Current",
  "Persistent Current",
  "Window Current"
)

# Match shared heatmap palette used elsewhere in this codebase.
HEATMAP_LOW_COLOR  <- "#C80813FF"
HEATMAP_MID_COLOR  <- "#F7F7F7"
HEATMAP_HIGH_COLOR <- "#083681"

metric_to_key <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", x))
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}

clean_numeric <- function(x) {
  x <- trimws(as.character(x))
  x[x == "" | toupper(x) == "ND"] <- NA_character_
  suppressWarnings(as.numeric(x))
}

safe_cor_test <- function(indicator, values, method) {
  ok <- !is.na(indicator) & !is.na(values)
  indicator <- indicator[ok]
  values <- values[ok]

  if (length(values) < 4 || length(unique(indicator)) < 2 || stats::sd(values) == 0) {
    return(c(estimate = NA_real_, p_value = NA_real_, n = length(values)))
  }

  test <- suppressWarnings(
    tryCatch(
      {
        if (method == "spearman") {
          stats::cor.test(indicator, values, method = "spearman", exact = FALSE)
        } else {
          stats::cor.test(indicator, values, method = "pearson")
        }
      },
      error = function(e) NULL
    )
  )

  if (is.null(test)) {
    return(c(estimate = NA_real_, p_value = NA_real_, n = length(values)))
  }

  c(
    estimate = unname(test$estimate),
    p_value = as.numeric(test$p.value),
    n = length(values)
  )
}

read_biophys_sheet <- function(sheet_name, assay_label) {
  df <- readxl::read_excel(PATH_BIOPHYSICS_DATA, sheet = sheet_name)
  names(df) <- trimws(names(df))

  if (!"Patient" %in% names(df)) {
    stop("Missing 'Patient' column in sheet: ", sheet_name)
  }

  missing_cols <- setdiff(biophys_properties, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Missing expected biophysics columns in ", sheet_name, ": ",
      paste(missing_cols, collapse = ", ")
    )
  }

  df %>%
    transmute(
      patient_uuid = as.character(Patient),
      across(all_of(biophys_properties), clean_numeric)
    ) %>%
    filter(!is.na(patient_uuid), patient_uuid != "") %>%
    group_by(patient_uuid) %>%
    summarise(
      across(
        all_of(biophys_properties),
        ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    pivot_longer(
      cols = all_of(biophys_properties),
      names_to = "feature",
      values_to = "value"
    ) %>%
    mutate(
      feature_key = metric_to_key(feature),
      assay = assay_label
    )
}

biophys_long <- bind_rows(
  read_biophys_sheet("SCN2A Neonatal", "Neonatal"),
  read_biophys_sheet("SCN2A Adult", "Adult")
)

read_cluster_file <- function(csv_path, age_label) {
  if (!file.exists(csv_path)) {
    warning("Missing cluster file: ", csv_path)
    return(tibble())
  }

  df <- read.csv(csv_path, stringsAsFactors = FALSE)
  required_cols <- c("patient_uuid", "cluster")
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in ", csv_path, ": ",
      paste(missing_cols, collapse = ", ")
    )
  }

  df_clean <- df %>%
    transmute(
      patient_uuid = as.character(patient_uuid),
      age = age_label,
      cluster = as.integer(cluster)
    ) %>%
    filter(!is.na(patient_uuid), patient_uuid != "", !is.na(cluster))

  conflicts <- df_clean %>%
    group_by(patient_uuid, age) %>%
    summarise(n_clusters = n_distinct(cluster), .groups = "drop") %>%
    filter(n_clusters > 1)

  if (nrow(conflicts) > 0) {
    warning(
      "Found patient(s) with conflicting cluster labels in ", csv_path,
      ". Keeping first occurrence per patient."
    )
  }

  df_clean %>%
    distinct(patient_uuid, age, .keep_all = TRUE)
}

cluster_data <- purrr::imap_dfr(cluster_files, read_cluster_file)

if (nrow(cluster_data) == 0) {
  stop("No cluster rows loaded from files in: ", file.path(DATA_PROCESSED, run_suffix))
}

age_cluster_levels <- cluster_data %>%
  distinct(age, cluster) %>%
  mutate(age = factor(age, levels = time_labels)) %>%
  arrange(age, cluster) %>%
  transmute(age_cluster = paste0(as.character(age), "_C", cluster)) %>%
  pull(age_cluster) %>%
  unique()

age_cluster_pairs <- cluster_data %>%
  distinct(age, cluster)

correlations <- purrr::pmap_dfr(
  age_cluster_pairs,
  function(age_label, cluster_id) {
    membership <- cluster_data %>%
      filter(.data$age == age_label) %>%
      transmute(
        patient_uuid,
        in_cluster = as.integer(.data$cluster == cluster_id)
      )

    joined <- membership %>%
      left_join(biophys_long, by = "patient_uuid", relationship = "many-to-many")

    joined %>%
      group_by(assay, feature, feature_key) %>%
      summarise(
        n_total_age = dplyr::n(),
        n_non_missing = sum(!is.na(value)),
        n_cluster = sum(in_cluster),
        n_cluster_non_missing = sum(in_cluster == 1L & !is.na(value)),
        spearman = {
          out <- safe_cor_test(in_cluster, value, method = "spearman")
          out[["estimate"]]
        },
        p_spearman = {
          out <- safe_cor_test(in_cluster, value, method = "spearman")
          out[["p_value"]]
        },
        point_biserial = {
          out <- safe_cor_test(in_cluster, value, method = "pearson")
          out[["estimate"]]
        },
        p_point_biserial = {
          out <- safe_cor_test(in_cluster, value, method = "pearson")
          out[["p_value"]]
        },
        .groups = "drop"
      ) %>%
      mutate(
        age = age_label,
        cluster = cluster_id,
        age_cluster = paste0(age_label, "_C", cluster_id)
      )
  }
)

correlations <- correlations %>%
  group_by(assay) %>%
  mutate(
    q_spearman = p.adjust(p_spearman, method = "fdr"),
    q_point_biserial = p.adjust(p_point_biserial, method = "fdr")
  ) %>%
  ungroup()

clamp_corr <- function(x, range = c(-1, 1)) {
  pmax(range[1], pmin(range[2], x))
}

plot_assay_heatmap <- function(assay_label) {
  plot_df <- correlations %>%
    filter(assay == assay_label) %>%
    mutate(
      age_cluster = factor(age_cluster, levels = age_cluster_levels),
      feature = factor(feature, levels = rev(biophys_properties))
    )

  p <- ggplot(plot_df, aes(x = age_cluster, y = feature, fill = spearman)) +
    geom_tile(aes(color = !is.na(spearman)), linewidth = 0.35) +
    geom_text(
      aes(label = ifelse(is.na(spearman), "", sprintf("%.2f", spearman))),
      size = 3.1,
      color = "black"
    ) +
    scale_fill_gradient2(
      low = HEATMAP_LOW_COLOR,
      mid = HEATMAP_MID_COLOR,
      high = HEATMAP_HIGH_COLOR,
      midpoint = 0,
      limits = c(-1, 1),
      oob = clamp_corr,
      na.value = "white",
      name = "Spearman\nrho"
    ) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = NA), guide = "none") +
    labs(
      title = paste0(assay_label, ": Cluster vs. Biophysics Correlation"),
      x = "Age Cluster",
      y = "Biophysical Feature"
    ) +
    theme_classic(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.ticks = element_blank(),
      plot.title.position = "plot"
    )

  width_in <- max(7, 0.8 * length(age_cluster_levels))
  out_path <- file.path(
    fig_dir,
    paste0("biophysics_cluster_heatmap_", tolower(assay_label), ".pdf")
  )

  ggsave(filename = out_path, plot = p, width = width_in, height = 5.5, units = "in")
}

walk(c("Neonatal", "Adult"), plot_assay_heatmap)

message("Saved heatmaps to: ", fig_dir)
