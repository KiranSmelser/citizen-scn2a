# R/cleaning_utilities.R
# Helper functions for data cleaning tasks

library(dplyr)
library(tibble)
library(stringr)

source(file.path(".", "R", "config.R"))

# Helper function for inequality conversion
.convert_inequality <- function(x, symbol, adjustment = 0) {
  if (grepl(symbol, x)) {
    return(as.numeric(sub(symbol, "", x)) + adjustment)
  }
  return(x)
}

# Wrapper functions for specific inequality symbols
convert_greater_than <- function(x) {
  .convert_inequality(x, ">", 1)
}

convert_less_than <- function(x) {
  .convert_inequality(x, "<", -1)
}

convert_greater_than_equal_to <- function(x) {
  .convert_inequality(x, "≥", 0)
}

convert_less_than_equal_to <- function(x) {
  .convert_inequality(x, "≤", 0)
}

# Recoding unique types
recode_unique_types <- function(x) {
  x <- as.character(x)
  x[x %in% c("2", "3", "4", "5")] <- "2+"
  factor(x, levels = c("0", "1", "2+"))
}

# Factor onset groups
factor_onset_group <- function(x) {
  factor(x, levels = c("1-4 Months", "Neonatal", "4-7 Months", "7-12 Months", "12+ Months"))
}

# Factor specified columns
convert_to_factor <- function(df, cols) {
  for (col in cols) {
    df[[col]] <- as.factor(df[[col]])
  }
  return(df)
}

# One-hot encode columns based on classifier
one_hot_encode <- function(df, classifier, prefix) {
  for (col_name in colnames(classifier)) {
    corresponding_cols <- paste0(prefix, classifier[[col_name]])
    existing_cols <- corresponding_cols %in% colnames(df)
    if (any(existing_cols)) {
      df[[col_name]] <- ifelse(rowSums(df[corresponding_cols[existing_cols]]) > 0, 1, 0)
    }
  }
  return(df)
}

# Safe mean that returns 0 if there are no valid values
safe_mean <- function(x) {
  if (length(x) == 0) return(0)
  m <- mean(x, na.rm = TRUE)
  if (is.nan(m)) return(0) else return(m)
}