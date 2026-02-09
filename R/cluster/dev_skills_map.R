# R/cluster/dev_skills_map.R
# Build Denver‑skills feature matrices from clustered patient data

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readxl)
  library(stringr)
})

source(file.path(".", "R", "config.R"))

time_labels  <- c("3yr", "5yr", "8yr", "10yr")
run_suffixes <- c("unknown_excluded", "lof_excluded")

# Mapping file
map_path_csv  <- file.path(DATA_CLASSIFIERS, "denver_skills_map.csv")

skill_map <- read.csv(map_path_csv, stringsAsFactors = FALSE)

# Prepare helper columns
skill_map <- skill_map %>%
  mutate(
    milestone_col = paste0("dev_", make.names(domain_milestone)),
    denver_col    = make.names(denver_skill)
  )


# Patient‑level metadata
pdf_map_path      <- file.path(DATA_CLASSIFIERS, "uuid_to_pdf_id.xlsx")
subgroup_map_path <- file.path(DATA_CLASSIFIERS, "subgroups.csv")

pdf_map <- readxl::read_excel(pdf_map_path, .name_repair = "universal") %>%
  rename_with(tolower) %>%
  rename(patient_uuid = patient_uuid, ID = id) %>%
  distinct(patient_uuid, .keep_all = TRUE)

subgroup_map <- read.csv(subgroup_map_path, stringsAsFactors = FALSE) %>%
  rename_with(tolower) %>%
  rename(patient_uuid = patient_uuid, subgroup = subgroup) %>%
  distinct(patient_uuid, .keep_all = TRUE)

# Helper that builds a Denver‑skills matrix from a cluster data frame
create_denver_df <- function(cluster_df, map_df) {

  # Attach PDF ID and subgroup metadata
  cluster_df <- cluster_df %>%
    left_join(pdf_map,      by = "patient_uuid") %>%
    left_join(subgroup_map, by = "patient_uuid")

  # Ensure every mapped milestone column exists
  for (mc in map_df$milestone_col) {
    if (!mc %in% names(cluster_df)) {
      cluster_df[[mc]] <- 0
    }
  }

  # Base frame with identifying columns
  denver_df <- cluster_df %>%
    select(patient_uuid, ID, subgroup, cluster)

  # Aggregate milestone flags
  map_split <- split(map_df, map_df$denver_col)

  for (dc in names(map_split)) {
    rows      <- map_split[[dc]]
    score_val <- rows$score[1]                 
    mcols     <- rows$milestone_col

    # Logical OR across all corresponding milestone columns
    achieved <- Reduce(`|`, lapply(cluster_df[mcols], `==`, 1))

    denver_df[[dc]] <- ifelse(achieved, score_val, 0)
  }

  denver_df
}

# Main loop over runs and periods
for (run_suffix in run_suffixes) {
  for (lbl in time_labels) {

    cluster_csv <- file.path(DATA_PROCESSED, run_suffix,
                             paste0(lbl, "_clusters.csv"))

    if (!file.exists(cluster_csv)) {
      warning("Missing cluster file: ", cluster_csv)
      next
    }

    df_clusters <- read.csv(cluster_csv, stringsAsFactors = FALSE)

    denver_df <- create_denver_df(df_clusters, skill_map)

    out_dir <- file.path(RESULTS, "clusters/", run_suffix, "dev")
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

    write.csv(denver_df,
              file.path(out_dir, paste0(lbl, "_denver_skills.csv")),
              row.names = FALSE)
  }
}