# R/descriptive/growth_analysis.R
# Analyses for patient weight, height, and head circumference 

library(dplyr)
library(purrr)
library(lme4)          
library(broom.mixed)   
library(emmeans)       
library(readr)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "descriptive", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "descriptive", "functions", "plotting_functions.R"))

if (!dir.exists(RESULTS)) dir.create(RESULTS, recursive = TRUE)

# Helper that computes z-scores
compute_zscores <- function(df_growth) {
  df_growth <- df_growth %>%
    mutate(
      age_months = round(age_days / 30),
      sex_code   = ifelse(sex == "male", 1, 2)
    )

  lms_refs <- list(
    "Body weight"        = load_who_lms("Body weight"),
    "Body height"        = load_who_lms("Body height"),
    "Head circumference" = load_who_lms("Head circumference")
  )

  bind_rows(lapply(names(lms_refs), function(p) {
    ref_tbl <- lms_refs[[p]] %>% rename(age_months = age)
    df_growth %>%
      filter(parameter == p) %>%
      left_join(ref_tbl,
                by = c("age_months", "sex_code" = "sex")) %>%
      mutate(
        z = ifelse(
          L == 0,
          log(value / M) / S,
          (((value / M) ^ L) - 1) / (L * S)
        )
      )
  }))
}

# Load & prepare data
df_growth <- clean_growth_data()
df_z      <- compute_zscores(df_growth) %>%
  filter(!is.na(z)) %>%
  mutate(sex = factor(sex, levels = c("female", "male")))

# Use all measurements between birth and 5yrs
df_model <- df_z %>%
  filter(age_months >= 0, age_months <= 60) %>%
  mutate(
    age_years = age_months / 12     
  )

# Fit models
get_contrasts <- function(mdl) {

  # Predicted means at 0yr and 5yrs for each sex
  emm <- emmeans(
    mdl, ~ sex | age_years,
    at = list(age_years = c(0, 5))
  )

  # Within‑sex change from 5yrs to 0yr
  within <- contrast(
    emm, interaction = "pairwise", by = "sex"
  ) |>
    tidy(conf.int = TRUE) |>
    mutate(
      contrast = paste0(sex, "_age5_vs_age0")
    ) |>
    select(
      contrast, estimate, std.error,
      conf.low, conf.high, statistic, p.value
    )

  # Between‑sex difference at each age
  between <- contrast(
    emm, "revpairwise", by = "age_years"
  ) |>
    tidy(conf.int = TRUE) |>
    mutate(
      contrast = paste0("male_vs_female_age", age_years)
    ) |>
    select(
      contrast, estimate, std.error,
      conf.low, conf.high, statistic, p.value
    )

  # Difference‑in‑differences (Δmale) to (Δfemale)
  diffdiff <- contrast(
    emtrends(mdl, ~ sex, var = "age_years"),
    "revpairwise"
  ) |>
    tidy(conf.int = TRUE) |>
    mutate(
      contrast = "diff_in_change_male_vs_female"
    ) |>
    select(
      contrast, estimate, std.error,
      conf.low, conf.high, statistic, p.value
    )

  # Overall main effect of sex (average offset)
  main_sex <- {
    fixed_df <- tidy(mdl, effects = "fixed")

    # Keep only sex coefficient
    fixed_df <- fixed_df %>% dplyr::filter(term == "sexmale")

    if (!"p.value" %in% names(fixed_df)) {
      fixed_df <- fixed_df %>%
        dplyr::mutate(p.value = 2 * stats::pnorm(-abs(statistic)))
    }

    fixed_df %>%
      dplyr::mutate(
        conf.low  = estimate - 1.96 * std.error,
        conf.high = estimate + 1.96 * std.error
      ) %>%
      dplyr::transmute(
        contrast  = "overall_sex_effect",
        estimate, std.error,
        conf.low, conf.high,
        statistic, p.value
      )
  }

  bind_rows(within, between, diffdiff, main_sex)
}

results <- df_model %>%
  group_by(parameter) %>%
  nest() %>%
  mutate(
    model     = map(data, ~ lmer(z ~ sex * age_years + (age_years | patient_uuid), data = .x)),
    contrasts = map(model, get_contrasts)
  ) %>%
  select(parameter, contrasts) %>%
  unnest(contrasts)

out_path <- file.path(RESULTS, "growth_zscore_mixed_model_contrasts.csv")
write_csv(results, out_path)

message("✓ Mixed‑effects contrasts written to: ",
        normalizePath(out_path))