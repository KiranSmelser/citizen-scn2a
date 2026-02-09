# R/cluster/age_feature_importance.R
# Analyze how features changes between age periods.

suppressPackageStartupMessages({
  library(glmnet)
  library(dplyr)
  library(purrr)
})

source(file.path(".", "R", "config.R"))

time_labels  <- c("3yr", "5yr", "8yr", "10yr")
age_pairs    <- list(c("3yr", "5yr"),
                     c("5yr", "8yr"),
                     c("8yr", "10yr"))
run_suffixes <- c("unknown_excluded", "lof_excluded")

# Helper for elastic‑net model
run_glmnet <- function(x, y, alpha = 0.5, nfolds = 10) {
  cv.glmnet(
    x        = x,
    y        = y,
    family   = "binomial",
    alpha    = alpha,
    nfolds   = nfolds,
    parallel = TRUE
  )
}

# Main loop
for (run_suffix in run_suffixes) {
  
  cluster_paths <- setNames(
    file.path(DATA_PROCESSED, run_suffix, paste0(time_labels, "_clusters.csv")),
    time_labels
  )
  
  out_dir <- file.path(RESULTS, "clusters", run_suffix, "feature_importance")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Iterate over adjacent age‑pairs
  walk(age_pairs, function(tp) {
    
    t1 <- tp[1]
    t2 <- tp[2]
    pair_label <- paste0(t1, "_vs_", t2)
    
    if (!file.exists(cluster_paths[[t1]]) || !file.exists(cluster_paths[[t2]])) {
      message("Skipping ", pair_label, " (missing files)")
      return(invisible(NULL))
    }
    
    df1 <- read.csv(cluster_paths[[t1]], stringsAsFactors = FALSE)
    df2 <- read.csv(cluster_paths[[t2]], stringsAsFactors = FALSE)
    
    # Keep only patients present at both time‑points
    common <- intersect(df1$patient_uuid, df2$patient_uuid)
    if (length(common) < 5) {                         
      message("Skipping ", pair_label, " (<5 overlapping patients)")
      return(invisible(NULL))
    }
    
    # Align data frames by patient_uuid
    df1 <- df1[match(common, df1$patient_uuid), ]
    df2 <- df2[match(common, df2$patient_uuid), ]
    
    # Harmonize feature columns across the two age‑periods
    ids   <- c("patient_uuid", "cluster")
    feat1 <- setdiff(colnames(df1), ids)
    feat2 <- setdiff(colnames(df2), ids)
    feats <- union(feat1, feat2)
    
    # Remove unwanted features
    unwanted <- c("sexmale", "age_onset_m", grep("^variant_", feats, value = TRUE))
    feats <- setdiff(feats, unwanted)

    missing_in_df1 <- setdiff(feats, feat1)
    missing_in_df2 <- setdiff(feats, feat2)
    if (length(missing_in_df1) > 0) df1[missing_in_df1] <- 0
    if (length(missing_in_df2) > 0) df2[missing_in_df2] <- 0

    # Ensure identical column ordering
    df1 <- df1[, c(ids, feats)]
    df2 <- df2[, c(ids, feats)]

    x1 <- as.matrix(df1[, feats])
    x2 <- as.matrix(df2[, feats])
    
    delta <- x2 - x1                                      
    
    # Build modelling matrix
    X <- rbind(
      matrix(0, nrow = nrow(delta), ncol = ncol(delta), dimnames = list(NULL, feats)),
      delta
    )
    
    # Drop zero‑variance columns
    nzv <- apply(X, 2, var) > 0
    X   <- X[ , nzv, drop = FALSE]
    if (ncol(X) == 0) {
      message("Skipping ", pair_label, " (all Δ‑features zero)")
      return(invisible(NULL))
    }
    
    y <- factor(c(rep(t1, nrow(delta)), rep(t2, nrow(delta))),
                levels = c(t1, t2))
    
    # Fit penalized model
    set.seed(123)
    fit <- run_glmnet(X, y)
    
    beta <- coef(fit, s = "lambda.1se")
    idx  <- which(beta != 0 & rownames(beta) != "(Intercept)")
    if (length(idx) == 0) {
      message("No non‑zero coefficients for ", pair_label)
      return(invisible(NULL))
    }
    
    coef_df <- tibble(
      period_pair = pair_label,
      feature     = rownames(beta)[idx],
      coefficient = as.numeric(beta[idx]),
      abs_coef    = abs(coefficient)
    ) %>%
      mutate(
        norm_coef = abs_coef / max(abs_coef, na.rm = TRUE),
        rank      = rank(-abs_coef, ties.method = "min")
      ) %>%
      arrange(rank)
    
    write.csv(
      coef_df,
      file = file.path(out_dir, paste0(pair_label, "_delta_coefs.csv")),
      row.names = FALSE
    )
    
    message("Completed ", pair_label, " (", run_suffix, ") with ",
            nrow(coef_df), " features")
  })
}