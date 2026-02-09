# R/cluster/age_med_feature_importance.R
# Analyze how medication usage changes between age periods.

suppressPackageStartupMessages({
  library(glmnet)      
  library(dplyr)       
  library(purrr)       
  library(stringr)     
  library(tidyr)       
})

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "cluster", "functions", "cleaning_functions.R"))

time_labels <- c("3yr", "5yr", "8yr", "10yr")
names(CLUSTER_CUTOFFS) <- time_labels       

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

# Helper for determining active medications at a given age cut‑off
get_med_presence <- function(cutoff_days, df_duration) {
  df_duration %>%
    filter(start_med_age <= cutoff_days,
           end_med_age   >= cutoff_days) %>%
    distinct(patient_uuid, medication_base) %>%
    mutate(value = 1) %>%
    pivot_wider(
      names_from  = medication_base,
      values_from = value,
      values_fill = list(value = 0),
      names_prefix = "med_"
    )
}

# Clean & pre‑process medication data
df_duration <- clean_medication_data() %>%
  mutate(
    medication_base = str_replace(medication, "\\s+\\d+$", ""),
    medication_base = str_trim(medication_base),
    medication_base = recode(medication_base, !!!ABBREVIATIONS_MEDS),
    # Treat missing end dates as ongoing
    end_med_age     = ifelse(is.na(end_med_age), Inf, end_med_age)
  )

med_presence_all <- imap(
  CLUSTER_CUTOFFS,
  ~ get_med_presence(.x, df_duration)
)              

# Per‑group analysis
set.seed(123)

for (run_suffix in run_suffixes) {

  exclude_vec <- if (run_suffix == "unknown_excluded") UNKNOWN else LOF

  med_presence_grp <- map(
    med_presence_all,
    ~ .x %>% filter(!patient_uuid %in% exclude_vec)
  )

  out_dir <- file.path(RESULTS, "clusters", run_suffix, "feature_importance")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # Compare adjacent age periods
  walk(age_pairs, function(tp) {

    t1 <- tp[1]; t2 <- tp[2]
    pair_label <- paste0(t1, "_vs_", t2)

    df1 <- med_presence_grp[[t1]]
    df2 <- med_presence_grp[[t2]]

    # Patients present at both time‑points
    common <- intersect(df1$patient_uuid, df2$patient_uuid)
    if (length(common) < 5) {
      message("Skipping ", pair_label, " (<5 overlapping patients)")
      return(invisible(NULL))
    }

    df1 <- df1[match(common, df1$patient_uuid), ]
    df2 <- df2[match(common, df2$patient_uuid), ]

    id_col <- "patient_uuid"
    feat1  <- setdiff(colnames(df1), id_col)
    feat2  <- setdiff(colnames(df2), id_col)
    feats  <- union(feat1, feat2)

    # Add missing medication columns as zeros
    if (length(setdiff(feats, feat1)) > 0) df1[setdiff(feats, feat1)] <- 0
    if (length(setdiff(feats, feat2)) > 0) df2[setdiff(feats, feat2)] <- 0

    df1 <- df1[, c(id_col, feats)]
    df2 <- df2[, c(id_col, feats)]

    X1 <- as.matrix(df1[, feats])
    X2 <- as.matrix(df2[, feats])

    delta <- X2 - X1

    # Build modelling matrix
    X <- rbind(
      matrix(0, nrow = nrow(delta), ncol = ncol(delta),
             dimnames = list(NULL, feats)),
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
      file = file.path(out_dir, paste0(pair_label, "_med_delta_coefs.csv")),
      row.names = FALSE
    )

    message("Completed ", pair_label, " (", run_suffix, ") with ",
            nrow(coef_df), " features")
  })
}