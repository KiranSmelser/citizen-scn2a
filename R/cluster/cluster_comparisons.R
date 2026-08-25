# R/cluster/cluster_comparisons.R
# Compare patient clusters across age cut‑offs

library(dplyr)
library(tidyr)
library(ggalluvial)
library(ggplot2)
library(forcats)

source(file.path(".", "R", "config.R"))

time_labels   <- c("1yr", "3yr", "5yr", "8yr", "10yr")
run_suffix <- "all_patients"

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
    select(patient_uuid, cluster) %>%
    rename(!!tp := cluster)
})

cluster_all <- Reduce(function(x, y) full_join(x, y, by = "patient_uuid"), cluster_dfs)

subgroup_map <- read.csv(PATH_SUBGROUP_CLASSIFIER, stringsAsFactors = FALSE) %>%
  select(patient_uuid, subgroup)

subgroup_order_levels <- c(
  "EO-DEE", "IS", "BFNIE", "LO-MI", "LO-C", "ASD/ID", "ASD-EEG*"
)
cluster_bar_width <- 1 / 16
subgroup_colors <- c(
  # Adapted from the historic Epilepsia figure palette. The seventh hue is
  # violet, and neighboring ribbon groups use deliberately contrasting hues.
  "EO-DEE" = "#005B56",
  "IS" = "#D06012",
  "BFNIE" = "#0067B9",
  "LO-MI" = "#E5B700",
  "LO-C" = "#9E1B4D",
  "ASD/ID" = "#6F7622",
  "ASD-EEG*" = "#76528B"
)

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

# Original complete-case alluvial diagram colored by 3-year cluster
original_alluvial_df <- cluster_all %>%
  drop_na(all_of(time_labels)) %>%
  mutate(across(all_of(time_labels), as.factor))

original_alluvial_axes <- setNames(
  rlang::syms(time_labels),
  paste0("axis", seq_along(time_labels))
)

p_cluster_alluvial <- ggplot(original_alluvial_df,
                             aes(!!!original_alluvial_axes,
                                 y = 1)) +
  geom_alluvium(aes(fill = !!sym("3yr")),
                alpha = 0.7, width = 1 / 12) +
  geom_stratum(width = 1 / 4, fill = "white",
               color = "black", show.legend = FALSE) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            size = 3) +
  scale_fill_manual(values = CLUSTER_COLORS) +
  scale_x_discrete(limits = time_labels) +
  labs(title = "Change in Cluster Membership Across Age Cut-offs",
       x = NULL, y = "Number of Patients", fill = "3-year Cluster") +
  theme_classic() +
  theme(axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(vjust = 6))

ggsave(file.path(FIGS, "clusters", run_suffix, "cluster_change_alluvial.pdf"),
       p_cluster_alluvial, width = 8, height = 5, units = "in")

# Subgroup-colored alluvial diagram across all available periods
# Keep every available assignment; missing later periods make an alluvium end.
alluvial_df <- cluster_all %>%
  left_join(subgroup_map, by = "patient_uuid") %>%
  filter(!is.na(.data[["3yr"]])) %>%
  pivot_longer(
    cols = all_of(time_labels),
    names_to = "age_cutoff",
    values_to = "cluster"
  ) %>%
  drop_na(cluster) %>%
  mutate(
    age_cutoff = factor(age_cutoff, levels = time_labels),
    cluster = factor(cluster, levels = sort(unique(cluster))),
    subgroup = as.factor(subgroup),
    subgroup_order = match(as.character(subgroup), subgroup_order_levels)
  )

# Invisible weighted strata create genuine vertical gaps while keeping the
# flow endpoints aligned with the visible cluster bars.
cluster_spacers <- tidyr::crossing(
  patient_uuid = c("cluster_spacer_a", "cluster_spacer_b"),
  age_cutoff = time_labels
) %>%
  mutate(
    cluster = if_else(
      age_cutoff == "3yr" & patient_uuid == "cluster_spacer_b",
      "spacer_2",
      "spacer_1"
    ),
    subgroup = "cluster_spacer",
    subgroup_order = 0L,
    plot_weight = if_else(age_cutoff == "3yr", 1.5, 0.75),
    is_spacer = TRUE
  )

axis_layout_totals <- bind_rows(
  alluvial_df %>%
    count(age_cutoff, name = "plot_weight"),
  cluster_spacers %>%
    group_by(age_cutoff) %>%
    summarise(plot_weight = sum(plot_weight), .groups = "drop")
) %>%
  group_by(age_cutoff) %>%
  summarise(plot_weight = sum(plot_weight), .groups = "drop")

centered_plot_height <- max(axis_layout_totals$plot_weight) + 1

plot_padding <- tidyr::crossing(
  patient_uuid = c("plot_padding_top", "plot_padding_bottom"),
  age_cutoff = time_labels
) %>%
  left_join(axis_layout_totals, by = "age_cutoff") %>%
  mutate(
    cluster = if_else(
      patient_uuid == "plot_padding_top",
      "padding_top",
      "padding_bottom"
    ),
    subgroup = "plot_padding",
    subgroup_order = 0L,
    plot_weight = (centered_plot_height - plot_weight) / 2,
    is_spacer = TRUE
  )

alluvial_plot_df <- bind_rows(
  alluvial_df %>%
    mutate(
      cluster = as.character(cluster),
      plot_weight = 1,
      is_spacer = FALSE
    ),
  cluster_spacers,
  plot_padding
) %>%
  mutate(
    age_cutoff = factor(age_cutoff, levels = time_labels),
    cluster = factor(
      cluster,
      levels = c(
        "padding_top", "1", "spacer_1", "2", "spacer_2", "3",
        "padding_bottom"
      )
    ),
    subgroup = factor(
      subgroup,
      levels = c(
        levels(alluvial_df$subgroup), "cluster_spacer", "plot_padding"
      )
    )
  )

p_subgroup_alluvial <- ggplot(alluvial_plot_df,
                              aes(x = age_cutoff,
                                  stratum = cluster,
                                  alluvium = patient_uuid,
                                  order = subgroup_order,
                                  y = plot_weight)) +
  geom_flow(aes(fill = subgroup, alpha = is_spacer),
            aes.bind = "flows", na.rm = TRUE,
            width = cluster_bar_width) +
  geom_stratum(width = cluster_bar_width, fill = "white",
               color = NA, show.legend = FALSE) +
  geom_stratum(aes(color = after_stat(ifelse(
                 grepl("^(spacer_|padding_)", stratum), NA_character_, "black"
               ))),
               width = cluster_bar_width, fill = NA, linewidth = 0.6,
               show.legend = FALSE) +
  geom_text(stat = "stratum",
            aes(label = after_stat(ifelse(
              grepl("^(spacer_|padding_)", stratum), "", as.character(stratum)
            ))),
            size = 3) +
  scale_color_identity() +
  scale_alpha_manual(
    values = c("FALSE" = 0.7, "TRUE" = 0),
    guide = "none"
  ) +
  scale_fill_manual(
    values = c(
      subgroup_colors,
      "cluster_spacer" = "white",
      "plot_padding" = "white"
    ),
    breaks = names(subgroup_colors)
  ) +
  scale_x_discrete(
    limits = time_labels,
    expand = expansion(add = 0.2)
  ) +
  labs(title = "Change in Cluster Membership Across Age Cut-offs",
       x = NULL, y = NULL, fill = "Subgroup") +
  theme_classic() +
  theme(axis.line.x  = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x  = element_text(vjust = 6),
        axis.line.y  = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank())

ggsave(file.path(FIGS, "clusters", run_suffix, "subgroup_change_alluvial.pdf"),
       p_subgroup_alluvial, width = 12, height = 6, units = "in")

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
