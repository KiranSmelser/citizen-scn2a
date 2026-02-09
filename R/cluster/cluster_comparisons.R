# R/cluster/cluster_comparisons.R
# Compare patient clusters across age cut‑offs

library(dplyr)
library(tidyr)
library(ggalluvial)
library(ggplot2)
library(forcats)

source(file.path(".", "R", "config.R"))

time_labels   <- c("3yr", "5yr", "8yr", "10yr")
run_suffixes <- c("unknown_excluded", "lof_excluded")

for (run_suffix in run_suffixes) {
  cluster_files <- file.path(DATA_PROCESSED, run_suffix,
                             paste0(time_labels, "_clusters.csv"))

  dir.create(file.path(RESULTS, "clusters", run_suffix),
             showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(FIGS,    "clusters", run_suffix),
             showWarnings = FALSE, recursive = TRUE)

compute_feature_summary <- function(df) {
  df %>%
    group_by(cluster) %>%
    summarise(
      cluster_size = n(),
      across(
        where(is.numeric),
        list(
          mean = ~ mean(.x, na.rm = TRUE),
          n    = ~ sum(!is.na(.x) & .x != 0)
        ),
        .names = "{fn}_{.col}"
      ),
      .groups = "drop"
    )
}

# Cluster assignments for each period
cluster_dfs <- lapply(seq_along(cluster_files), function(i) {
  tp <- time_labels[i]
  read.csv(cluster_files[i], stringsAsFactors = FALSE) %>%
    rename(!!tp := cluster)
})

cluster_all <- Reduce(function(x, y) full_join(x, y, by = "patient_uuid"), cluster_dfs)

# Pair‑wise contingency tables
pairwise_periods <- combn(time_labels, 2, simplify = FALSE)

for (pp in pairwise_periods) {
  df_pair <- cluster_all %>%
    select(patient_uuid, all_of(pp)) %>%
    drop_na()

  contingency <- df_pair %>%
    count(!!sym(pp[1]), !!sym(pp[2])) %>%
    pivot_wider(names_from = pp[2], values_from = n, values_fill = list(n = 0))

  write.csv(contingency,
            file.path(RESULTS, "clusters", run_suffix,
                      paste0("contingency_", pp[1], "_", pp[2], ".csv")),
            row.names = FALSE)
}

# Alluvial diagram across all periods
alluvial_df <- cluster_all %>%
  drop_na(all_of(time_labels)) %>%
  mutate(across(all_of(time_labels), as.factor))

p_alluvial <- ggplot(alluvial_df,
                     aes(axis1 = !!sym(time_labels[1]),
                         axis2 = !!sym(time_labels[2]),
                         axis3 = !!sym(time_labels[3]),
                         axis4 = !!sym(time_labels[4]),
                         y     = 1)) +
  geom_alluvium(aes(fill = !!sym(time_labels[1])),
                alpha = 0.7, width = 1 / 12) +
  geom_stratum(width = 1 / 4, fill = "white",
               color = "black", show.legend = FALSE) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3) +
  scale_x_discrete(limits = time_labels) +
  labs(title = "Change in Cluster Membership Across Age Cut-offs",
       x = NULL, y = "Number of Patients", fill = "3‑year Cluster") +
  theme_classic() +
  theme(axis.line.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_text(vjust = 6))

ggsave(file.path(FIGS, "clusters", run_suffix, "cluster_change_alluvial.pdf"),
       p_alluvial, width = 8, height = 5, units = "in")

# Feature summaries for each period
for (i in seq_along(cluster_files)) {
  df <- read.csv(cluster_files[i], stringsAsFactors = FALSE)
  summary_df <- compute_feature_summary(df)
  summary_long <- summary_df %>%
    pivot_longer(
      cols = -c(cluster, cluster_size),
      names_to = c(".value", "feature"),
      names_pattern = "^(mean|n)_(.*)$"
    )

  write.csv(summary_long,
            file.path(RESULTS, "clusters", run_suffix,
                      paste0(time_labels[i], "_cluster_summary.csv")),
            row.names = FALSE)
}
}