# R/cluster/cluster_feature_importance.R
# Determine feature importance across clusters and time periods

suppressPackageStartupMessages({
  library(glmnet)      
  library(dplyr)       
  library(tidyr)       
  library(purrr)       
})

source(file.path(".", "R", "config.R"))

time_labels <- c("3yr", "5yr", "8yr", "10yr")
run_suffixes <- c("unknown_excluded", "lof_excluded")

# Loop over each exclusion scenario
for (run_suffix in run_suffixes) {

  cluster_files <- file.path(DATA_PROCESSED, run_suffix,
                             paste0(time_labels, "_clusters.csv"))

  coef_dir <- file.path(RESULTS, "clusters", run_suffix,
                        "feature_importance")
  fig_dir  <- file.path(FIGS, "clusters", run_suffix)

  dir.create(coef_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(fig_dir,  showWarnings = FALSE, recursive = TRUE)

# Helper for elastic‑net model
run_glmnet <- function(x, y, alpha = 0.5, nfolds = 10) {
  cv.glmnet(
    x        = x,
    y        = y,
    family   = "multinomial",
    alpha    = alpha,          
    nfolds   = nfolds,
    parallel = TRUE
  )
}

# Loop over each age
  set.seed(123)  

  walk2(cluster_files, time_labels, function(csv_path, lbl) {
    df <- read.csv(csv_path, stringsAsFactors = FALSE)

    y <- factor(df$cluster)
    x <- as.matrix(df %>% select(-patient_uuid, -cluster))

    # Fit model
    cls_sizes <- table(y)
    tiny_cls  <- names(cls_sizes[cls_sizes <= 2])

    if (length(tiny_cls) > 0) {
      keep <- !(y %in% tiny_cls)
      x <- x[keep, , drop = FALSE]
      y <- droplevels(y[keep])

      if (nlevels(y) <= 2) {
        return(invisible(NULL))
      }
    }

    fit <- run_glmnet(x, y)

    coef_list <- coef(fit, s = "lambda.1se")

    coef_df <- imap_dfr(coef_list, function(mat, cls) {
      idx <- which(mat != 0 & rownames(mat) != "(Intercept)")
      tibble(
        cluster     = cls,
        feature     = rownames(mat)[idx],
        coefficient = as.numeric(mat[idx]),
        abs_coef    = abs(coefficient)
      )
    })

    # Normalize |β| within each cluster and assign ranks
    coef_df <- coef_df %>%
      group_by(cluster) %>%
      mutate(
        norm_coef = abs_coef / max(abs_coef, na.rm = TRUE),
        rank      = rank(-abs_coef, ties.method = "min")
      ) %>%
      ungroup() %>%
      arrange(cluster, rank)

    write.csv(coef_df,
              file = file.path(coef_dir,
                               paste0(lbl, "_coefs.csv")),
              row.names = FALSE)

  })

  # Combine all coefficient files and plot feature importance across ages
  suppressPackageStartupMessages({
    library(ggplot2)
    library(forcats)
  })

  # Read and combine
  coef_files <- list.files(path = coef_dir,
                           pattern = "_coefs\\.csv$",
                           full.names = TRUE)

  coef_all <- purrr::map_dfr(coef_files, function(f) {
    df <- read.csv(f, stringsAsFactors = FALSE)
    df$time_period <- gsub("(.+)/(\\w+)_coefs\\.csv$", "\\2", f)
    df
  })

  # Factor time_period
  coef_all <- coef_all %>%
    mutate(time_period = factor(time_period, levels = c("3yr", "5yr", "8yr", "10yr")))

  # Order features within each facet
  coef_all <- coef_all %>%
    group_by(time_period, cluster) %>%
    mutate(feature = fct_reorder(feature, norm_coef)) %>%
    ungroup() %>%
    mutate(feature_id = as.numeric(feature))

  # Create horizontal bar plot
  p_feature_imp <- ggplot(coef_all, aes(x = norm_coef, y = feature_id, fill = coefficient > 0)) +
    geom_col(orientation = "y") +
    scale_y_continuous(
      breaks   = seq_along(levels(coef_all$feature)),
      labels   = levels(coef_all$feature),
      sec.axis = dup_axis(name = NULL),
      expand   = expansion(add = 0)
    ) +
    facet_grid(
      cluster ~ time_period,
      scales  = "free_y",
      switch  = "y",
      labeller = labeller(cluster = function(x) paste("Cluster", x))
    ) +
    scale_fill_manual(values = c("TRUE" = "#197EC0FF", "FALSE" = "#C80813FF")) +
    labs(
      title = "Feature Importance",
      x     = "Normalized Coefficient",
      y     = "Feature"
    ) +
    theme_bw() +
    theme(
      strip.placement   = "outside",
      legend.position   = "none",
      axis.text.y.left  = element_text(),
      axis.ticks.y.left = element_line(),
      axis.text.y.right = element_text(),
      axis.ticks.y.right= element_line()
    )

  ggsave(filename = file.path(fig_dir, "feature_importance.pdf"),
         plot = p_feature_imp,
         width = 28, height = 26, units = "in")

}