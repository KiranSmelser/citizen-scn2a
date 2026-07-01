# R/cluster/clusters.R
# Generate patient clusters

library(dplyr)
library(readxl)
library(tidyr)
library(stringr)
library(mclust)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "cleaning_utilities.R"))
source(file.path(".", "R", "cluster", "functions", "cleaning_functions.R"))

read_subgroup_map <- function() {
  read.csv(PATH_SUBGROUP_CLASSIFIER,
           stringsAsFactors = FALSE,
           fileEncoding = "UTF-8-BOM") %>%
    rename_with(tolower) %>%
    select(patient_uuid, subgroup) %>%
    distinct(patient_uuid, .keep_all = TRUE)
}

# Single-run wrapper
produce_clusters <- function(cutoff_days, label, run_suffix = "all_patients") {
  # Make the chosen age cut‑off visible to all downstream filters
  assign("AGE_CUTOFF_DAYS", cutoff_days, envir = .GlobalEnv)

  dir.create(file.path(DATA_PROCESSED, run_suffix), recursive = TRUE,
             showWarnings = FALSE)
  dir.create(file.path(FIGS, "clusters", run_suffix), recursive = TRUE,
             showWarnings = FALSE)

classifier <- read_classifier()
subgroup_map <- read_subgroup_map()


# Demographic and variant features
demographics <- read_excel(PATH_CITIZEN_DATA, sheet = "demographics") %>%
  select(patient_uuid, sex, most_recent_records_age_days) %>%
  filter(
    !is.na(sex),                                 
    most_recent_records_age_days >= AGE_CUTOFF_DAYS  
  ) %>%
  select(patient_uuid, sex)   

genetics <- read_excel(PATH_CITIZEN_DATA, sheet = "genetic_findings") %>%
  filter(gene == "SCN2A") %>%
  select(patient_uuid, variant_protein) %>%
  mutate(variant_protein = ifelse(is.na(variant_protein) | variant_protein == "", "unknown", variant_protein)) %>%
  distinct(patient_uuid, .keep_all = TRUE)

# Combine features
cluster_data <- demographics %>%
  inner_join(genetics, by = "patient_uuid")

cluster_data <- convert_to_factor(cluster_data, cols = c("sex", "variant_protein"))

# One-hot encode categorical variables
cluster_data_encoded <- model.matrix(~ sex + variant_protein - 1, data = cluster_data) %>%
  as.data.frame() %>%
  select(-sexfemale) %>%
  mutate(patient_uuid = cluster_data$patient_uuid)

# Construct seizure type features
df_sz <- read_seizure_history() %>%
  filter(seizure_history_age_days < AGE_CUTOFF_DAYS)
names(df_sz) <- make.names(sub('^seizure_history_', '', names(df_sz)), unique = TRUE)

sz_classifier <- classifier[grepl("seizure_", names(classifier))]
names(sz_classifier) <- sub('^seizure_', '', names(sz_classifier))

sz_type <- df_sz %>% 
  filter(type %in% unlist(sz_classifier)) %>%
  mutate(type = case_when(
    type %in% sz_classifier$focal ~ "focal",
    type %in% sz_classifier$spasms ~ "spasms",
    type %in% sz_classifier$`tonic-clonic` ~ "tonic-clonic",
    type %in% sz_classifier$clonic ~ "clonic",
    type %in% sz_classifier$tonic ~ "tonic",
    type %in% sz_classifier$myoclonic ~ "myoclonic",
    type %in% sz_classifier$absence ~ "absence",
    TRUE ~ type
  ))

# One‑hot encode seizure type
sz_features <- sz_type %>%
  distinct(patient_uuid, type) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from  = type,
    values_from = value,
    values_fill = list(value = 0)
  ) %>%
  rename_with(~ paste0("sz_", .), -patient_uuid)

# Construct diagnosis features
df_diagnosis <- read_clinical_diagnosis() %>%
  filter(clinical_diagnosis_age_days_firstDate < AGE_CUTOFF_DAYS)
names(df_diagnosis) <- make.names(sub('^clinical_', '', names(df_diagnosis)), unique = TRUE)

diagnosis_classifier <- classifier[grepl("diagnosis_", names(classifier))]
names(diagnosis_classifier) <- sub('^diagnosis_', '', names(diagnosis_classifier))

diagnosis_type <- df_diagnosis %>% 
  filter(diagnosis %in% unlist(diagnosis_classifier)) %>%
  mutate(type = case_when(
    diagnosis %in% diagnosis_classifier$behavioral ~ "behavioral",
    diagnosis %in% diagnosis_classifier$muscoloskeletal ~ "muscoloskeletal",
    diagnosis %in% diagnosis_classifier$gastro ~ "gastro",
    diagnosis %in% diagnosis_classifier$immune ~ "immune",
    diagnosis %in% diagnosis_classifier$neuro ~ "neuro",
    diagnosis %in% diagnosis_classifier$sensory ~ "sensory",
    diagnosis %in% diagnosis_classifier$respiratory ~ "respiratory",
    diagnosis %in% diagnosis_classifier$cardio ~ "cardio",
    TRUE ~ diagnosis
  ))

# One‑hot encode diagnosis type
diagnosis_features <- diagnosis_type %>%
  distinct(patient_uuid, type) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from  = type,
    values_from = value,
    values_fill = list(value = 0)
  ) %>%
  rename_with(~ paste0("diag_", .), -patient_uuid)

# Construct hospital features
df_hospital <- read_hospitalizations() %>%
  filter(admission_age_days_firstDate < AGE_CUTOFF_DAYS)
names(df_hospital) <- make.names(sub('^admission_', '', names(df_hospital)), unique = TRUE)

hospital_classifier <- classifier[grepl("hospital_", names(classifier))]
names(hospital_classifier) <- sub('^hospital_', '', names(hospital_classifier))

hospital_type <- df_hospital %>% 
  filter(diagnosis %in% unlist(hospital_classifier)) %>%
  mutate(type = case_when(
    diagnosis %in% hospital_classifier$gi ~ "gi",
    diagnosis %in% hospital_classifier$infection ~ "infection",
    diagnosis %in% hospital_classifier$pulmonary ~ "pulmonary",
    diagnosis %in% hospital_classifier$respiratory_failure ~ "respiratory_failure",
    diagnosis %in% hospital_classifier$pneumonia ~ "pneumonia",
    diagnosis %in% hospital_classifier$neuro ~ "neuro",
    diagnosis %in% hospital_classifier$behavior ~ "behavior",
    diagnosis %in% hospital_classifier$status ~ "status",
    diagnosis %in% hospital_classifier$seizure ~ "sz",
    TRUE ~ diagnosis
  ))

# One‑hot encode hospitalization type
hospital_features <- hospital_type %>%
  distinct(patient_uuid, type) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from  = type,
    values_from = value,
    values_fill = list(value = 0)
  ) %>%
  rename_with(~ paste0("hosp_", .), -patient_uuid)

# Construct EEG features
df_eeg <- read_eeg() %>%
  filter(str_detect(procedure, "EEG")) %>%
  filter(procedure_age_days < AGE_CUTOFF_DAYS)

eeg_classifier <- classifier[grepl("eeg_", names(classifier))]
names(eeg_classifier) <- sub('^eeg_', '', names(eeg_classifier))

eeg_type <- df_eeg %>% 
  filter(procedure_findings %in% unlist(eeg_classifier)) %>%
  mutate(type = case_when(
    procedure_findings %in% eeg_classifier$normal ~ "normal",
    procedure_findings %in% eeg_classifier$epileptiform ~ "epileptiform",
    procedure_findings %in% eeg_classifier$other_paroxysmal_abnormalities ~ "paroxysmal_abnormalities",
    procedure_findings %in% eeg_classifier$background ~ "background",
    TRUE ~ procedure_findings
  ))

# One‑hot encode EEG type
eeg_features <- eeg_type %>%
  distinct(patient_uuid, type) %>%
  mutate(value = 1) %>%
  pivot_wider(
    names_from  = type,
    values_from = value,
    values_fill = list(value = 0)
  ) %>%
  rename_with(~ paste0("eeg_", .), -patient_uuid)

# Construct Development features
df_dev <- read_development_data() %>%
  filter(domain_age_days_firstDate < AGE_CUTOFF_DAYS & domain_status == "Able")

dev_data_encoded <- model.matrix(~ domain_milestone - 1, data = df_dev) %>%
  as.data.frame() %>%
  mutate(patient_uuid = df_dev$patient_uuid) %>%
  group_by(patient_uuid) %>%
  summarise(across(where(is.numeric), max), .groups = "drop") %>%
  rename_with(~ paste0("dev_", sub("^domain_milestone", "", .x)), -patient_uuid)

# Construct Medication features
# df_med <- read_medication_aggregate() %>%
#   filter(medication_age_days_firstDate < AGE_CUTOFF_DAYS)
# 
# df_med <- df_med %>%
#   mutate(
#     medication = ifelse(grepl("ACTH", medication), "ACTH", medication),
#     medication = recode(medication,
#                         `Epidiolex` = "Epidiolex/CBD",
#                         `Cannabidiol` = "Epidiolex/CBD")
#   ) %>%
#   filter(grepl(MEDS_TO_USE, medication))
# 
# med_data_encoded <- model.matrix(~ medication - 1, data = df_med) %>%
#   as.data.frame() %>%
#   mutate(patient_uuid = df_med$patient_uuid) %>%
#   group_by(patient_uuid) %>%
#   summarise(across(where(is.numeric), max), .groups = "drop") %>%
#   rename_with(~ paste0("med_", sub("^medication", "", .x)), -patient_uuid)

# Construct Age of Onset feature
df_onset <- read_seizure_history() %>%
  filter(!is.na(seizure_history_age_days)) %>%
  group_by(patient_uuid) %>%
  summarise(age_onset_m = min(seizure_history_age_days, na.rm = TRUE) / 30,
            .groups = "drop")

# Construct biophysics features (Neonatal + Adult)
#biophys_features <- read_biophysics_data()

# Combine all encoded features
cluster_data_final <- cluster_data_encoded %>%
  left_join(sz_features, by = "patient_uuid") %>%
  left_join(diagnosis_features, by = "patient_uuid") %>%
  left_join(hospital_features, by = "patient_uuid") %>%
  left_join(eeg_features, by = "patient_uuid") %>%
  left_join(dev_data_encoded, by = "patient_uuid") %>%
  # left_join(med_data_encoded, by = "patient_uuid") %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  left_join(df_onset, by = "patient_uuid")

# Exclude specified diagnosis and hospitalization features
cluster_data_final <- cluster_data_final %>%
  select(-diag_neuro, -diag_respiratory, -diag_behavioral,
         -diag_sensory, -diag_immune,
         -hosp_neuro, -hosp_gi, -hosp_pulmonary,
         -hosp_infection, -hosp_respiratory_failure,
         -hosp_behavior)

# Exclude patients w/ more than 25 NA values
cluster_data_final <- cluster_data_final %>%
  filter(rowSums(is.na(.)) <= 25)

# Join biophysics features after non-biophysics NA filtering
# cluster_data_final <- cluster_data_final %>%
#   left_join(biophys_features, by = c("patient_uuid" = "Patient"))

# Preprocessing

# Drop patient identifier
cluster_data_pre <- cluster_data_final %>% select(-patient_uuid)

# Impute missing onset ages and create missing indicator
cluster_data_pre <- cluster_data_pre %>%
  mutate(
    onset_missing = is.na(age_onset_m),
    age_onset_m   = ifelse(onset_missing,
                           median(age_onset_m, na.rm = TRUE),
                           age_onset_m)
  ) %>%
  select(-onset_missing)

# Create missingness indicators and median-impute biophysics z-score features
bio_cols <- names(cluster_data_pre)[grepl("^bio_", names(cluster_data_pre))]
for (col_name in bio_cols) {
  missing_col <- paste0(col_name, "_missing")
  cluster_data_pre[[missing_col]] <- as.integer(is.na(cluster_data_pre[[col_name]]))

  median_value <- median(cluster_data_pre[[col_name]], na.rm = TRUE)
  if (is.na(median_value)) {
    median_value <- 0
  }

  cluster_data_pre[[col_name]][is.na(cluster_data_pre[[col_name]])] <- median_value
}

# Preserve an un‑scaled version for downstream feature summaries
cluster_data_pre_unscaled <- cluster_data_pre

# Scale continuous onset age so that mean=0, sd=1
cluster_data_pre$age_onset_m <- scale(cluster_data_pre$age_onset_m)

# Clustering

# Fit model and select optimal number of clusters by BIC
mclust_model <- Mclust(cluster_data_pre)
cat("mclust selected", mclust_model$G, "clusters\n")

cluster_feature_data <- cluster_data_pre_unscaled %>%
  mutate(
    patient_uuid = cluster_data_final$patient_uuid,
    cluster      = mclust_model$classification
  ) %>%
  left_join(subgroup_map, by = "patient_uuid") %>%
  relocate(patient_uuid, subgroup, cluster)

write.csv(cluster_feature_data,
          file.path(DATA_PROCESSED, run_suffix, paste0(label, "_clusters.csv")),
          row.names = FALSE)

# PCA visualization of cluster assignments
library(ggplot2)

# Perform PCA on preprocessed data
pca_res <- prcomp(cluster_data_pre, center = TRUE, scale. = FALSE)
pca_df <- data.frame(pca_res$x[, 1:2]) %>%
  setNames(c("PC1", "PC2")) %>%
  dplyr::mutate(patient_uuid = cluster_data_final$patient_uuid,
                cluster      = factor(mclust_model$classification))

# Plot the first two PCs 
p <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.8) +
  labs(title = paste(label, "Patient Clusters"),
       x = "PC1",
       y = "PC2",
       color = "Cluster") +
  theme_classic()


ggsave(filename = file.path(FIGS, "clusters", run_suffix, paste0("clusters_", label, ".pdf")),
       plot = p, width = 6, height = 5, units = "in")
}

# Execute clustering runs
# Generate 1-year clusters as an additional artifact without changing the
# downstream age labels that depend on the 3/5/8/10-year analysis set.
produce_clusters(365.25, "1yr", "all_patients")

time_labels <- c("3yr", "5yr", "8yr", "10yr")            

for (i in seq_along(CLUSTER_CUTOFFS)) {
  produce_clusters(CLUSTER_CUTOFFS[i], time_labels[i], "all_patients")
}
