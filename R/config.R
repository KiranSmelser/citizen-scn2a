# R/config.R
# Global configuration for file paths and parameters

# Data directories
DATA_RAW <- "./data/raw/"
DATA_PROCESSED <- "./data/processed/"
DATA_CLASSIFIERS <- "./data/classifiers/"

# Data files
PATH_CITIZEN_DATA <- file.path(DATA_RAW, "Citizen_SCN2A_UArizona_2025.07.xlsx")
PATH_BIOPHYSICS_DATA <- file.path(DATA_RAW, "SCN2A Biophysics Labeling.xlsx")

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
PATH_EFFECTS_SEVERITY <- file.path(DATA_CLASSIFIERS, "effects_severity.xlsx")
PATH_INITIAL_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "initial_seizure_types_classifier.xlsx")
PATH_MED_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "med_categories.csv")
PATH_SUBGROUP_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "subgroups.csv")

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

# Epilepsia-friendly cluster palette shared by PCA and alluvial figures
CLUSTER_COLORS <- c(
  "1" = "#0067B9",
  "2" = "#D06012",
  "3" = "#005B56"
)

# Medication names
MEDS_TO_USE <- paste(
  c("Adrenocorticotropin (ACTH 1-18),I-125 (TYR)", "ACTH", "Clonazepam", "Levetiracetam", 
    "Phenytoin", "Oxcarbazepine", "Carbamazepine", "Phenobarbital", 
    "Lamotrigine", "Briveracetam", "Brivaracetam", "Cannabidiol", "Clobazam", "Epidiolex", 
    "Eslicarbazepine", "Ethosuximide", "Felbamate", "Gabapentin", 
    "Prednisolone", "Lacosamide", "Primidone", "Rufinamide", "Topiramate", 
    "Valproate", "Vigabatrin", "Zonisamide", "Stiripentol", "Tiagabine", 
    "Perampanel", "Pregabalin", "Cannabinol", "Cenobamate", "Fenfluramine",
    "Tetrahydrocannabinol", "Acetazolamide", "Methylprednisolone", "Prednisone"),
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
  "Brivaracetam" = "BRV", 
  "ACTH" = "ACTH",
  "Epidiolex/CBD" = "CBD",
  "Topiramate" = "TPM",
  "Prednisolone" = "PRD",
  "Perampanel" = "PER",
  "Pregabalin" = "PGB",
  "Cenobamate" = "CNB",
  "Fenfluramine" = "FFA",
  "Tetrahydrocannabinol" = "THC",
  "Acetazolamide" = "AZM",
  "Methylprednisolone" = "MP",
  "Prednisone" = "PDN",
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
