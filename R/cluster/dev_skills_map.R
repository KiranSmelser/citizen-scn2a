# R/cluster/dev_skills_map.R
# Build patient-level developmental milestone matrices from clustered patient data

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(tidyr)
})

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))

time_labels <- c("1yr", "3yr", "5yr", "8yr", "10yr")
cluster_cutoffs <- setNames(c(365.25, CLUSTER_CUTOFFS), time_labels)

run_suffix <- "all_patients"

subgroup_map <- read.csv(PATH_SUBGROUP_CLASSIFIER,
                         stringsAsFactors = FALSE,
                         fileEncoding = "UTF-8-BOM") %>%
  rename_with(tolower) %>%
  select(patient_uuid, subgroup) %>%
  distinct(patient_uuid, .keep_all = TRUE)

# Denver skills mapping is intentionally disabled for now.
# map_path_csv <- file.path(DATA_CLASSIFIERS, "denver_skills_map.csv")
# skill_map <- read.csv(map_path_csv, stringsAsFactors = FALSE) %>%
#   mutate(
#     milestone_col = paste0("dev_", make.names(domain_milestone)),
#     denver_col    = make.names(denver_skill)
#   )

create_development_milestone_df <- function(cluster_df, cutoff_days) {
  cluster_assignments <- cluster_df %>%
    select(patient_uuid, any_of("subgroup"), cluster)

  if (!"subgroup" %in% names(cluster_assignments)) {
    cluster_assignments <- cluster_assignments %>%
      left_join(subgroup_map, by = "patient_uuid") %>%
      relocate(subgroup, .after = patient_uuid)
  }

  df_dev <- read_development_data() %>%
    filter(
      patient_uuid %in% cluster_assignments$patient_uuid,
      domain_age_days_firstDate < cutoff_days,
      domain_status == "Able"
    )

  if (nrow(df_dev) == 0) {
    return(cluster_assignments)
  }

  dev_wide <- model.matrix(~ domain_milestone - 1, data = df_dev) %>%
    as.data.frame() %>%
    mutate(patient_uuid = df_dev$patient_uuid) %>%
    group_by(patient_uuid) %>%
    summarise(across(where(is.numeric), max), .groups = "drop") %>%
    rename_with(~ paste0("dev_", sub("^domain_milestone", "", .x)), -patient_uuid)

  cluster_assignments %>%
    left_join(dev_wide, by = "patient_uuid") %>%
    relocate(patient_uuid, subgroup, cluster) %>%
    mutate(across(starts_with("dev_"), ~ replace_na(., 0)))
}

for (lbl in time_labels) {
  cluster_csv <- file.path(DATA_PROCESSED, run_suffix, paste0(lbl, "_clusters.csv"))

  if (!file.exists(cluster_csv)) {
    warning("Missing cluster file: ", cluster_csv)
    next
  }

  df_clusters <- read.csv(cluster_csv, stringsAsFactors = FALSE)
  dev_df <- create_development_milestone_df(df_clusters, cluster_cutoffs[[lbl]])

  out_dir <- file.path(RESULTS, "clusters", run_suffix, "dev")
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  write.csv(
    dev_df,
    file.path(out_dir, paste0(lbl, "_dev_milestones.csv")),
    row.names = FALSE
  )
}
