# R/functions/cleaning_functions.R
# Functions for common data cleaning tasks

library(dplyr)
library(readr)
library(readxl)
library(stringr)
library(tibble)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "data_import_functions.R"))
source(file.path(".", "R", "cleaning_utilities.R"))

# Clean seizure data
clean_seizure_data <- function(include_spasms = FALSE) {
  df_sz <- read_seizure_history()
  
  # Apply conversions for inequality symbols
  df_sz$value[is.na(df_sz$seizure_history_value)] <- 1
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_greater_than)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_greater_than_equal_to)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_less_than)
  df_sz$seizure_history_value <- sapply(df_sz$seizure_history_value, convert_less_than_equal_to)
  df_sz$seizure_history_value <- as.numeric(str_trim(df_sz$seizure_history_value))
  
  names(df_sz) <- make.names(sub('^seizure_history_', '', names(df_sz)), unique = TRUE)
  
  classifier <- read_classifier()
  classifier <- classifier[grepl("seizure_", names(classifier))]
  names(classifier) <- sub('^seizure_', '', names(classifier))
  
  valid_types <- c(classifier$`tonic-clonic`, classifier$focal, classifier$absence,
                   classifier$tonic, classifier$myoclonic)
  if (include_spasms) {
    valid_types <- c(valid_types, classifier$spasms)
  }
  
  df_type <- df_sz %>% 
    filter(type %in% valid_types) %>%
    mutate(type = case_when(
      type %in% classifier$`tonic-clonic` ~ "Tonic-clonic",
      type %in% classifier$focal ~ "Focal",
      type %in% classifier$absence ~ "Absence",
      type %in% classifier$tonic ~ "Tonic",
      type %in% classifier$myoclonic ~ "Myoclonic",
      include_spasms & type %in% classifier$spasms ~ "Spasms",
      TRUE ~ type
    ))
  
  # Read and combine index sheets
  tc_index        <- read_excel(PATH_TC_INDEX)      %>% mutate(type = "Tonic-clonic")
  focal_index     <- read_excel(PATH_FOCAL_INDEX)     %>% mutate(type = "Focal")
  absence_index   <- read_excel(PATH_ABSENCE_INDEX)   %>% mutate(type = "Absence")
  tonic_index     <- read_excel(PATH_TONIC_INDEX)     %>% mutate(type = "Tonic")
  myoclonic_index <- read_excel(PATH_MYOCLONIC_INDEX) %>% mutate(type = "Myoclonic")
  
  index <- bind_rows(tc_index, focal_index, absence_index, tonic_index, myoclonic_index)
  names(index) <- sub('^seizure_history_', '', names(index))
  names(index) <- sub('^seizure_', '', names(index))
  index <- subset(index, select = -c(3:5, 7))
  index$value <- as.numeric(index$value)
  
  df_type <- left_join(
    df_type,
    unique(index),
    by = c("type", "value", "unit"),
    relationship = "many-to-many"
  )
  df_type$index[is.na(df_type$index)] <- 1
  
  if ("subgroup_lof" %in% names(classifier)) {
    lof_patients <- classifier$subgroup_lof
    df_type <- df_type %>% filter(!patient_uuid %in% lof_patients)
  }
  
  return(df_type)
}

# Clean hospitalization data
clean_hospitalization_data <- function(select_cols = c(1:2)) {
  hosp <- read_hospitalizations()
  hosp <- subset(hosp, select = select_cols)
  
  hosp_classifier <- read_excel(PATH_HOSPITALIZATION_CLASSIFIER)
  hosp <- left_join(hosp, hosp_classifier, by = c("admission_diagnosis" = "Hospitalization Events"))
  hosp <- hosp %>% filter(`Admission Type` %in% c("Emergency", "Incidental"))
  
  return(hosp)
}

# Calculate hospitalization frequencies
calculate_hospitalization_frequencies <- function(hosp_data) {
  freq_df <- hosp_data %>% group_by(`Admission Type`, Subgroup, admission_diagnosis) %>%
    summarise(n = n(), .groups = "drop") %>% ungroup() %>% na.omit()
  
  seizure_freq <- freq_df %>% filter(Subgroup == "Seizure") %>%
    mutate(admission_diagnosis = if_else(admission_diagnosis == "Status epilepticus", "Status epilepticus", "Seizure"))
  
  other_freq <- freq_df %>% filter(Subgroup != "Seizure") %>% mutate(Facet = "Other")
  
  hosp_data_mod <- hosp_data
  hosp_data_mod$admission_diagnosis[hosp_data_mod$admission_diagnosis == "Aspiration pneumonia"] <- "Pneumonia"
  hosp_data_mod$admission_diagnosis[hosp_data_mod$admission_diagnosis == "Acute respiratory failure"] <- "Respiratory failure"
  
  specific_types <- c("Respiratory failure", "Pneumonia")
  
  specific_hosp <- hosp_data_mod %>% filter(`Admission Type` == "Emergency", admission_diagnosis %in% specific_types) %>%
    group_by(`Admission Type`, Subgroup, admission_diagnosis) %>%
    summarise(n_specific = n(), .groups = "drop") %>% ungroup() %>%
    arrange(`Admission Type`, Subgroup, admission_diagnosis) %>% group_by(`Admission Type`, Subgroup) %>%
    mutate(cumulative_n = cumsum(n_specific)) %>% ungroup() %>% mutate(label = paste(admission_diagnosis, n_specific, sep = ": "))
  
  return(list(
    other_freq = other_freq,
    seizure_freq = seizure_freq,
    specific_freq = specific_hosp
  ))
}

# Clean diagnoses data
clean_diagnoses_data <- function() {
  diagnoses <- read_excel(PATH_CITIZEN_DATA, sheet = "clinical_diagnosis_features")
  diagnoses <- subset(diagnoses, select = c(1:2, 9))
  
  PATH_DIAGNOSIS_CLASSIFIER <- file.path(DATA_CLASSIFIERS, "Grouping diagnoses.xlsx")
  diagnosis_classifier <- read_excel(PATH_DIAGNOSIS_CLASSIFIER)
  diagnosis_classifier <- subset(diagnosis_classifier, select = -c(5))
  
  diagnoses <- left_join(diagnoses, diagnosis_classifier, by = c("diagnosis" = "Clinical Diagnoses"))
  diagnoses <- unique(na.omit(subset(diagnoses, select = c(1:2, 6))))
  diagnoses <- diagnoses %>% filter(!System %in% c("Endocrine", "Excretory", "Integumentary"))
  
  sys_pcts <- diagnoses %>% group_by(System) %>% summarise(
    sys_pct = n() / nrow(diagnoses) * 100,
    sys_n = n(),
    .groups = "drop"
  )
  
  diagnosis_pcts <- diagnoses %>% group_by(System, diagnosis) %>% summarise(diagnosis_n = n(), .groups = "drop")
  
  diagnosis_pcts <- left_join(diagnosis_pcts, sys_pcts, by = "System")
  diagnosis_pcts <- diagnosis_pcts %>% mutate(diagnosis_pct = (diagnosis_n / sys_n) * 100) %>% ungroup()
  
  diagnosis_pcts <- subset(diagnosis_pcts, select = c("System", "diagnosis", "diagnosis_pct"))
  
  return(list(
    diagnoses = diagnoses,
    sys_pcts = sys_pcts,
    diagnosis_pcts = diagnosis_pcts
  ))
}

# Clean medication data
clean_medication_data <- function() {
  df_med <- read_medication_aggregate()
  
  df_med <- df_med %>%
    mutate(
      medication = ifelse(grepl("ACTH", medication), "ACTH", medication),
      medication = recode(medication,
                          `Epidiolex` = "Epidiolex/CBD",
                          `Cannabidiol` = "Epidiolex/CBD")
    ) %>%
    filter(grepl(MEDS_TO_USE, medication))
  
  df_duration <- df_med %>%
    transmute(
      patient_uuid,
      medication,
      start_med_age = medication_age_days_firstDate,
      end_med_age   = medication_age_days_lastDate
    ) %>%
    distinct() %>%
    arrange(patient_uuid, medication, start_med_age, end_med_age) %>%
    group_by(patient_uuid, medication) %>%
    group_modify(~ merge_intervals(.x)) %>%
    ungroup() %>%
    filter((end_med_age - start_med_age) > 0) %>%
    group_by(patient_uuid, medication) %>%
    mutate(
      interval_id = row_number(),
      intervals_for_this_med = n()
    ) %>%
    ungroup() %>%
    mutate(
      medication = if_else(intervals_for_this_med > 1,
                           paste0(medication, " ", interval_id),
                           medication)
    )
  return(df_duration)
}

# Merge overlapping intervals for medication duration data
merge_intervals <- function(intervals_df) {
  intervals_df <- intervals_df %>% arrange(start_med_age)
  
  if (nrow(intervals_df) == 0) {
    return(tibble(start_med_age = numeric(0), end_med_age = numeric(0)))
  }
  
  current_start <- intervals_df$start_med_age[1]
  current_end   <- intervals_df$end_med_age[1]
  merged_list   <- list()
  
  if (nrow(intervals_df) > 1) {
    for (i in seq(2, nrow(intervals_df))) {
      s <- intervals_df$start_med_age[i]
      e <- intervals_df$end_med_age[i]
      
      if (s <= current_end) {
        current_end <- max(current_end, e, na.rm = TRUE)
      } else {
        merged_list[[length(merged_list) + 1]] <- tibble(
          start_med_age = current_start,
          end_med_age   = current_end
        )
        current_start <- s
        current_end   <- e
      }
    }
  }
  
  merged_list[[length(merged_list) + 1]] <- tibble(
    start_med_age = current_start,
    end_med_age   = current_end
  )
  
  bind_rows(merged_list)
}


# Prepare top medications over age
prepare_top_medications_over_age <- function(df_med_duration,
                                             top_n = 10,
                                             desired_levels = c("OXC", "LCM", "CLB", "LTG", "CLZ",
                                                                "LEV", "CBD", "PHT", "TPM", "PBT")) {
  df_med_duration <- df_med_duration %>%
    mutate(medication_base = sub(" \\d+$", "", medication))

  top_meds <- df_med_duration %>%
    distinct(patient_uuid, medication_base) %>%
    count(medication_base, name = "patient_count") %>%
    arrange(desc(patient_count)) %>%
    slice_head(n = top_n) %>%
    pull(medication_base)

  df_top_meds <- df_med_duration %>%
    filter(medication_base %in% top_meds)

  df_expanded <- df_top_meds %>%
    rowwise() %>%
    mutate(age_year = list(seq(floor(start_med_age / 365),
                               floor(end_med_age   / 365)))) %>%
    tidyr::unnest(cols = c(age_year)) %>%
    ungroup() %>%
    distinct(patient_uuid, medication_base, age_year)

  patients_by_med <- df_expanded %>%
    group_by(medication_base, age_year) %>%
    summarise(n = n_distinct(patient_uuid), .groups = "drop")

  patients_total <- df_expanded %>%
    group_by(age_year) %>%
    summarise(total = n_distinct(patient_uuid), .groups = "drop")

  normalized_df <- patients_by_med %>%
    left_join(patients_total, by = "age_year") %>%
    mutate(prop       = n / total,
           medication = recode(medication_base, !!!ABBREVIATIONS_MEDS)) %>%
    select(age_year, medication, prop)

  present_levels <- desired_levels[desired_levels %in% normalized_df$medication]
  normalized_df$medication <- factor(normalized_df$medication,
                                     levels  = present_levels,
                                     ordered = TRUE)

  return(list(
    normalized_df  = normalized_df,
    present_levels = present_levels
  ))
}

# Prepare med durations for plotting
prepare_medication_durations <- function(df_med_duration,
                                         present_levels) {
  df_durations <- df_med_duration %>%
    mutate(
      medication_base = sub(" \\d+$", "", medication),
      medication      = recode(medication_base, !!!ABBREVIATIONS_MEDS),
      duration_months = (end_med_age - start_med_age) / 30
    ) %>%
    filter(medication %in% present_levels)

  df_durations$medication <- factor(df_durations$medication,
                                    levels  = present_levels,
                                    ordered = TRUE)
  return(df_durations)
}

# Prepare durations by med categories for plotting
prepare_medication_category_durations <- function(df_med_duration,
                                                  exclude_categories = c("Steroids"),
                                                  category_levels = c("Sodium Channel Blockers", "GABAergic",
                                                                      "Calcium Channel Blockers", "SVP2A",
                                                                      "Other/Multiple")) {
  med_classifier <- readr::read_csv(PATH_MED_CLASSIFIER, show_col_types = FALSE)

  med_map <- med_classifier %>%
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to  = "category",
                        values_to = "medication") %>%
    dplyr::filter(!is.na(medication) & medication != "") %>%
    dplyr::mutate(medication = stringr::str_trim(medication))

  df_durations_cat <- df_med_duration %>%
    dplyr::mutate(
      medication_base = sub(" \\d+$", "", medication),
      duration_months = (end_med_age - start_med_age) / 30
    ) %>%
    dplyr::left_join(med_map, by = c("medication_base" = "medication")) %>%
    dplyr::filter(!is.na(category), !(category %in% exclude_categories)) %>%
    dplyr::select(-medication)

  df_durations_cat$category <- factor(df_durations_cat$category,
                                      levels  = category_levels,
                                      ordered = TRUE)

  df_plot_cat <- df_durations_cat %>%
    dplyr::rename(medication = category)

  return(list(
    df_plot_cat     = df_plot_cat,
    category_levels = category_levels
  ))
}

# Build med x adverse effect Fisher tests (for Figure 6)
build_med_ae_fisher <- function() {
  # Read and clean medication data
  df_med <- read_medication_aggregate() %>%
    dplyr::mutate(
      medication = ifelse(grepl("ACTH", medication), "ACTH", medication),
      medication = dplyr::recode(medication,
                                 `Epidiolex`   = "Epidiolex/CBD",
                                 `Cannabidiol` = "Epidiolex/CBD")
    ) %>%
    dplyr::filter(grepl(MEDS_TO_USE, medication)) %>%
    dplyr::transmute(patient_uuid, medication_base = medication) %>%
    dplyr::distinct()

  # Wide matrix
  med_wide <- df_med %>%
    dplyr::mutate(present = 1L) %>%
    tidyr::pivot_wider(names_from = medication_base,
                       values_from = present,
                       values_fill = list(present = 0L))

  # Restrict medications
  allowed_abbr <- c("ZNS", "VPA", "VBG", "TPM", "RFM", "PRD", "PHT", "PER",
                    "PBT", "OXC", "LTG", "LEV", "LCM", "GBP", "CLZ", "CLB",
                    "CBD", "ACTH")
  med_cols_all <- setdiff(colnames(med_wide), "patient_uuid")
  if (length(med_cols_all) > 0) {
    med_abbr_map <- dplyr::recode(med_cols_all, !!!ABBREVIATIONS_MEDS, .default = NA_character_)
    keep_meds <- med_cols_all[med_abbr_map %in% allowed_abbr]
    med_wide  <- med_wide %>% dplyr::select(patient_uuid, dplyr::all_of(keep_meds))
  }

  # Filter adverse effects for Moderate/Severe only
  adverse_effects_raw <- read_adverse_effects()
  adverse_severity <- readxl::read_excel(PATH_EFFECTS_SEVERITY)
  # adverse_effects_raw <- adverse_effects_raw %>%
  #   dplyr::inner_join(adverse_severity %>%
  #                       dplyr::filter(severity_score %in% c("Moderate", "Severe")),
  #                     by = "adverse_effect")

  # Restrict to top 10 adverse effects
  top_ae <- adverse_effects_raw %>%
    dplyr::count(adverse_effect, name = "n") %>%
    dplyr::arrange(dplyr::desc(n)) %>%
    dplyr::slice_head(n = 10) %>%
    dplyr::pull(adverse_effect)

  # Reduce to patient x AE presence for top AEs
  adverse_effects <- adverse_effects_raw %>%
    dplyr::filter(adverse_effect %in% top_ae) %>%
    dplyr::select(patient_uuid, adverse_effect) %>%
    dplyr::distinct()

  # Wide matrix
  ae_wide <- adverse_effects %>%
    dplyr::mutate(present = 1L) %>%
    tidyr::pivot_wider(names_from = adverse_effect,
                       values_from = present,
                       values_fill = list(present = 0L))

  # Build unified patient list and align matrices
  all_pts <- union(med_wide$patient_uuid, ae_wide$patient_uuid)
  full_df <- tibble::tibble(patient_uuid = all_pts) %>%
    dplyr::left_join(med_wide, by = "patient_uuid") %>%
    dplyr::left_join(ae_wide,  by = "patient_uuid") %>%
    dplyr::mutate(dplyr::across(-patient_uuid, ~ tidyr::replace_na(., 0L)))

  # Identify column groups
  med_cols <- setdiff(colnames(med_wide), "patient_uuid")
  ae_cols  <- setdiff(colnames(ae_wide),  "patient_uuid")

  # Handle edge cases
  if (length(med_cols) == 0 || length(ae_cols) == 0) {
    return(list(
      fisher_df = tibble::tibble(
        medication     = character(0),
        adverse_effect = character(0),
        log_or         = numeric(0),
        p_adj          = numeric(0)
      )
    ))
  }

  # Convert to integer matrices
  med_mat <- as.matrix(full_df[, med_cols, drop = FALSE])
  ae_mat  <- as.matrix(full_df[, ae_cols,  drop = FALSE])
  storage.mode(med_mat) <- "integer"
  storage.mode(ae_mat)  <- "integer"

  # Compute 2x2 cell counts for all pairs
  P <- nrow(full_df)
  a_mat <- t(med_mat) %*% ae_mat                               # both present
  med_tot <- matrix(colSums(med_mat), nrow = length(med_cols), ncol = length(ae_cols))
  ae_tot  <- matrix(colSums(ae_mat),  nrow = length(med_cols), ncol = length(ae_cols), byrow = TRUE)
  b_mat <- med_tot - a_mat                                     # med only
  c_mat <- ae_tot  - a_mat                                     # AE only
  d_mat <- P - a_mat - b_mat - c_mat                           # neither

  # Assemble long dataframe of all pairs
  combos <- expand.grid(medication_base = med_cols,
                        adverse_effect  = ae_cols,
                        stringsAsFactors = FALSE)
  combos$m_idx <- match(combos$medication_base, med_cols)
  combos$e_idx <- match(combos$adverse_effect,  ae_cols)

  # Extract counts per pair
  get_cell <- function(M, i, j) as.integer(M[i, j])
  combos$a <- mapply(function(i, j) get_cell(a_mat, i, j), combos$m_idx, combos$e_idx)
  combos$b <- mapply(function(i, j) get_cell(b_mat, i, j), combos$m_idx, combos$e_idx)
  combos$c <- mapply(function(i, j) get_cell(c_mat, i, j), combos$m_idx, combos$e_idx)
  combos$d <- mapply(function(i, j) get_cell(d_mat, i, j), combos$m_idx, combos$e_idx)

  # Haldane-Anscombe correction
  combos$or_ha  <- ((combos$a + 0.5) * (combos$d + 0.5)) / ((combos$b + 0.5) * (combos$c + 0.5))
  combos$log_or <- log2(combos$or_ha)

  # Fisher exact test p-values
  combos$p_val <- mapply(function(a, b, c, d) {
    stats::fisher.test(matrix(c(a, b, c, d), nrow = 2))$p.value
  }, combos$a, combos$b, combos$c, combos$d)

  # Apply Benjamini–Hochberg correction column-wise
  combos <- combos %>%
    dplyr::group_by(adverse_effect) %>%
    dplyr::mutate(p_adj = p.adjust(p_val, method = "BH")) %>%
    dplyr::ungroup()

  # Recode med names to abbreviations
  combos$medication <- dplyr::recode(combos$medication_base, !!!ABBREVIATIONS_MEDS)

  # Order
  combos$medication    <- factor(combos$medication, levels = allowed_abbr, ordered = TRUE)
  combos$adverse_effect<- factor(combos$adverse_effect, levels = top_ae, ordered = TRUE)

  fisher_df <- combos %>%
    dplyr::select(medication, adverse_effect, log_or, p_adj)

  return(list(fisher_df = fisher_df))
}

# Clean growth data
clean_growth_data <- function() {
  growth_raw <- read_growth()

  # Select only relevant columns
  wanted <- c(
    "patient_uuid",
    "growth_parameter",
    "growth_parameter_value",
    "growth_parameter_unit",
    "growth_parameter_age_days"
  )
  present <- intersect(wanted, names(growth_raw))
  growth <- growth_raw %>% dplyr::select(dplyr::all_of(present))

  # Normalize units
  growth <- growth %>%
    dplyr::mutate(
      parameter = as.character(growth_parameter),
      unit      = tolower(stringr::str_trim(as.character(growth_parameter_unit))),
      value_raw = suppressWarnings(as.numeric(growth_parameter_value)),
      age_days  = suppressWarnings(as.numeric(growth_parameter_age_days))
    ) %>%
    dplyr::mutate(
      value = dplyr::case_when(
        parameter == "Body weight"        & unit == "g"  ~ value_raw / 1000,
        parameter == "Body weight"        & unit == "kg" ~ value_raw,
        parameter == "Body height"        & unit == "m"  ~ value_raw * 100,
        parameter == "Body height"        & unit == "cm" ~ value_raw,
        parameter == "Head circumference" & unit == "m"  ~ value_raw * 100,
        parameter == "Head circumference" & unit == "cm" ~ value_raw,
        TRUE ~ as.numeric(NA_real_)
      )
    ) %>%
    dplyr::filter(parameter %in% c("Body weight", "Body height", "Head circumference"))

  # Add sex
  demo <- read_demographics() %>% dplyr::select(patient_uuid, sex)
  growth <- growth %>%
    dplyr::left_join(demo, by = "patient_uuid") %>%
    dplyr::mutate(
      sex = stringr::str_trim(tolower(as.character(sex))),
      sex = dplyr::case_when(
        sex %in% c("male", "female") ~ sex,
        TRUE ~ NA_character_
      )
    )

  df_growth <- growth %>%
    dplyr::select(patient_uuid, sex, parameter, value, age_days) %>%
    dplyr::filter(!is.na(value), !is.na(age_days), !is.na(sex))

  return(df_growth)
}

# Compute censor ages for patients
compute_censor_ages <- function(df_duration) {
  df_duration %>%
    group_by(patient_uuid) %>%
    summarise(
      censor_age_days = max(end_med_age, na.rm = TRUE),
      .groups = "drop"
    )
}

# Clean appointment data
clean_appointment_data <- function() {
  
  # Aggregate timestamps
  df_med_apts <- read_medication_aggregate() %>%
    select(patient_uuid,
           appointment_age_days = medication_age_days_firstDate)
  
  df_duration_end <- clean_medication_data() %>%
    select(patient_uuid,
           appointment_age_days = end_med_age)
  
  df_sz_apts <- read_seizure_history() %>%
    select(patient_uuid,
           appointment_age_days = seizure_history_age_days)
  
  df_diagnosis_apts <- readxl::read_excel(
    PATH_CITIZEN_DATA,
    sheet = "clinical_diagnosis"
  ) %>%
    select(patient_uuid,
           appointment_age_days = clinical_diagnosis_age_days_firstDate)
  
  appointments <- bind_rows(
    df_med_apts,
    df_duration_end,
    df_sz_apts,
    df_diagnosis_apts
  ) %>%
    filter(!is.na(appointment_age_days)) %>%
    distinct() %>%
    arrange(patient_uuid, appointment_age_days)
  
  # Apply censoring at last medication end
  censor_df <- compute_censor_ages(clean_medication_data())    
  
  appointments <- appointments %>%
    left_join(censor_df, by = "patient_uuid") %>%
    filter(appointment_age_days <= censor_age_days) %>%
    mutate(
      appointment_age_months = appointment_age_days / 30,
      censor_age_months      = censor_age_days   / 30
    )
  
  # Gaps between consecutive appointments
  distance_df <- appointments %>%
    group_by(patient_uuid) %>%
    arrange(appointment_age_months, .by_group = TRUE) %>%
    mutate(
      dist_months = appointment_age_months - lag(appointment_age_months),
      half        = ifelse(appointment_age_months <= censor_age_months / 2,
                           "First half", "Second half")
    ) %>%
    filter(!is.na(dist_months), dist_months <= 12) %>%
    ungroup()
  
  # Appointment counts per half
  counts_df <- appointments %>%
    mutate(
      half = ifelse(appointment_age_months <= censor_age_months / 2,
                    "First half", "Second half")
    ) %>%
    group_by(patient_uuid, half) %>%
    summarise(count = n(), .groups = "drop")
  
  list(
    distance_df = distance_df,
    counts_df   = counts_df
  )
}