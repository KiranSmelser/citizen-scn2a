# R/config.R
# Global configuration for file paths and parameters

# Data directories
DATA_RAW <- "./data/raw/"
DATA_PROCESSED <- "./data/processed/"
DATA_CLASSIFIERS <- "./data/classifiers/"

# Data files
PATH_SUPP_DATA <- file.path(DATA_RAW, "Supplementary_Table - Study Data by Groups.csv")
PATH_CITIZEN_DATA <- file.path(DATA_RAW, "Ciitizen_SCN8A_UArizona_2024.02.09.xlsx")
PATH_ONSET_AGES <- file.path(DATA_RAW, "onset_ages.csv")
PATH_CLUSTER_ONSETS <- file.path(DATA_RAW, "onset_ages_clustering.csv")

# WHO Data
PATH_HEAD_BOYS <- file.path(DATA_RAW, "hcfa-boys-0-5-zscores.xlsx")
PATH_HEAD_GIRLS <- file.path(DATA_RAW, "hcfa-girls-0-5-zscores.xlsx")
PATH_WEIGHT_BOYS <- file.path(DATA_RAW, "wfa_boys_0-to-5-years_zscores.xlsx")
PATH_WEIGHT_GIRLS <- file.path(DATA_RAW, "wfa_girls_0-to-5-years_zscores.xlsx")
PATH_HEIGHT_BOYS_0_TO_2 <- file.path(DATA_RAW, "lhfa_boys_0-to-2-years_zscores.xlsx")
PATH_HEIGHT_BOYS_2_TO_5 <- file.path(DATA_RAW, "lhfa_boys_2-to-5-years_zscores.xlsx")
PATH_HEIGHT_GIRLS_0_TO_2 <- file.path(DATA_RAW, "lhfa_girls_0-to-2-years_zscores.xlsx")
PATH_HEIGHT_GIRLS_2_TO_5 <- file.path(DATA_RAW, "lhfa_girls_2-to-5-years_zscores.xlsx")

# Output directories
FIGS <- "./output/figures/"
RESULTS <- "./output/results/"

# Classifier files
PATH_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "ciitizen_health_classifier.xlsx")
PATH_HOSPITALIZATION_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "Grouping Hospitalizations.xlsx")
PATH_OVERLAP_PATIENTS <- file.path(DATA_CLASSIFIERS, "Overlap Patients corrected.xlsx")
PATH_EFFECTS_SEVERITY <- file.path(DATA_CLASSIFIERS, "effects_severity.xlsx")
PATH_INITIAL_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "initial_seizure_types_classifier.xlsx")
PATH_SUBGROUP_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "subgroups.csv")
PATH_MED_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "med_categories.csv")
PATH_IDS <- file.path(DATA_CLASSIFIERS, "uuid_to_pdf_id.xlsx")

# Additional index files
PATH_TC_INDEX <- file.path(DATA_RAW, "tonic-clonic_index.xlsx")
PATH_FOCAL_INDEX <- file.path(DATA_RAW, "focal_index.xlsx")
PATH_MYOCLONIC_INDEX <- file.path(DATA_RAW, "myoclonic_index.xlsx")
PATH_ABSENCE_INDEX <- file.path(DATA_RAW, "absence_index.xlsx")
PATH_TONIC_INDEX <- file.path(DATA_RAW, "tonic_index.xlsx")

# Global parameters
AGE_CUTOFF_DAYS <- 1095  

# Cluster age cutoffs (days)
CLUSTER_CUTOFFS <- c(1095, 1826.25, 2922, 3652.5)

# UNKNOWN patients
UNKNOWN <- c("56d3dc1e-63d5-4ad6-9493-57de0fd7ef0e", "b17a5dff-ebfb-4621-97b9-a0072da41851",
         "47c27adc-27bd-41df-b39c-ae70495716cb", "5c8bb15f-1f87-41a1-8932-ff18afdba917", 
         "ec227c21-6fdc-46c2-95ef-4ecb6603083c", "9527087a-40c6-48e4-993c-e3f12f746ee2", 
         "a6a301ea-bdf5-4111-be04-78a0fb73960a", "fb67ec01-0ced-4abb-a71e-07621dc999ce")
         # "1e5d723d-eba3-4b9a-b732-b6a2dae83ced", "52928f64-eb12-44b7-a63c-7276af7a9c71",
         # "54d7c124-efac-4738-bbcc-f8139fe0d808", "b9a324cc-affd-4302-8949-12988004bfb7", 
         # "bd3d0985-311e-4a42-a40b-ff295cd0b64e", "b633f077-df16-4c7c-ab40-aa83fa7608d6",
         # "1e66ce47-b9fe-460f-9346-796a9e2bf5f3", "6f7d6b24-2596-4245-a7bb-046dbe3fdbe2",
         # "85ec6265-8a3f-48e4-b1fb-16661647d977", "6acf6198-f2b4-4468-9234-7d2b4710e14f",
         # "d3b527d5-0662-42a1-9e0c-67d4cc908595", "944f05de-9853-4cc5-9fcf-d47ee7a8dc0f")

# LOF patients
LOF <- c("ec227c21-6fdc-46c2-95ef-4ecb6603083c", "9527087a-40c6-48e4-993c-e3f12f746ee2",
         "2e335784-6f3a-4982-a800-dd125ed7fa31", "9d63ed35-ec7c-4ea1-ba03-fcfdfdf0ee55",
         "a6a301ea-bdf5-4111-be04-78a0fb73960a", "5c8bb15f-1f87-41a1-8932-ff18afdba917",
         "fb67ec01-0ced-4abb-a71e-07621dc999ce", "5979a566-cb58-460e-932f-af259f152547",
         "b17a5dff-ebfb-4621-97b9-a0072da41851", "1e66ce47-b9fe-460f-9346-796a9e2bf5f3",
         "54d7c124-efac-4738-bbcc-f8139fe0d808", "6acf6198-f2b4-4468-9234-7d2b4710e14f",
         "6f7d6b24-2596-4245-a7bb-046dbe3fdbe2", "85ec6265-8a3f-48e4-b1fb-16661647d977",
         "944f05de-9853-4cc5-9fcf-d47ee7a8dc0f", "b9a324cc-affd-4302-8949-12988004bfb7",
         "d3b527d5-0662-42a1-9e0c-67d4cc908595")

# Medication names
MEDS_TO_USE <- paste(
  c("Adrenocorticotropin (ACTH 1-18),I-125 (TYR)", "Clonazepam", "Levetiracetam", 
    "Phenytoin", "Oxcarbazepine", "Carbamazepine", "Phenobarbital", 
    "Lamotrigine", "Briveracetam", "Cannabidiol", "Clobazam", "Epidiolex", 
    "Eslicarbazepine", "Ethosuximide", "Felbamate", "Gabapentin", 
    "Prednisolone", "Lacosamide", "Primidone", "Rufinamide", "Topiramate", 
    "Valproate", "Vigabatrin", "Zonisamide", "Stiripentol", "Tiagabine", 
    "Perampanel"),
  collapse = "|"
)

# Abbreviations for meds and seizures
ABBREVIATIONS_MEDS <- c(
  "Oxcarbazepine" = "OXC",
  "Lacosamide" = "LCM",
  "Phenytoin" = "PHT", 
  "Valproate" = "VPA", 
  "Lamotrigine" = "LTG", 
  "Carbamazepine" = "CBZ", 
  "Rufinamide" = "RFM",
  "Eslicarbazepine" = "ESL", 
  "Clonazepam" = "CLZ",
  "Clobazam" = "CLB",
  "Phenobarbital" = "PBT", 
  "Vigabatrin" = "VBG", 
  "Felbamate" = "FBM",
  "Primidone" = "PRM", 
  "Stiripentol" = "STP", 
  "Tiagabine" = "TGB", 
  "Zonisamide" = "ZNS",
  "Gabapentin" = "GBP",
  "Ethosuximide" = "ETX", 
  "Levetiracetam" = "LEV", 
  "Briveracetam" = "BRV", 
  "ACTH" = "ACTH",
  "Epidiolex/CBD" = "CBD",
  "Topiramate" = "TPM",
  "Prednisolone" = "PRD",
  "Perampanel" = "PER",
  "None" = "None"
)
ABBREVIATIONS_SEIZURES <- c(
  "Bilateral Tonic-clonic" = "BTC",
  "Tonic-clonic" = "BTC",
  "Focal" = "FOC",
  "Absence" = "ABS",
  "Tonic" = "TON",
  "Myoclonic" = "MYO",
  "Infantile spasms" = "SPM",
  "Epileptic spasms" = "SPM",
  "Spasms" = "SPM",
  "Clonic" = "CLO",
  "Status Epilepticus" = "SE",
  "Prolonged Seizure (>5 Minutes)" = "Prolonged Sz"
)