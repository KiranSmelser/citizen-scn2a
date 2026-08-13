# R/timeline/fig_16.R
# Recreate Figure 16 as a four-patient timeline panel.

library(dplyr)
library(ggplot2)
library(patchwork)
library(readxl)

source(file.path(".", "R", "config.R"))
source(file.path(".", "R", "timeline", "functions", "cleaning_functions.R"))
source(file.path(".", "R", "timeline", "functions", "plotting_functions.R"))

# The order is row-wise and matches the supplied reference figure:
# dc9... | fe01...
# ff11... | eb13...
figure_16_patients <- c(
  "dc9acdff-9b86-45f9-bedd-bc35a8aeb937",
  "fe01039d-a0cb-4102-a086-28462d55e843",
  "ff11182e-4d24-4c45-b4f3-f9a586601ec4",
  "eb13d2ff-7e89-4d70-ad08-25d9da87bb56"
)

# Build the same censored timeline inputs used by patient_analysis.R.
df_duration <- clean_medication_data()
censor_ages <- compute_censor_ages(df_duration) %>%
  mutate(censor_age_months = censor_age_days / 30)

df_type <- suppressWarnings(clean_seizure_data()) %>%
  left_join(censor_ages, by = "patient_uuid") %>%
  filter(age_days <= censor_age_days) %>%
  select(-censor_age_days, -censor_age_months)

df_sz <- read_excel(PATH_CITIZEN_DATA, sheet = "seizure_history")
names(df_sz) <- sub("^seizure_history_", "", names(df_sz))
df_sz <- df_sz %>%
  left_join(censor_ages, by = "patient_uuid") %>%
  filter(age_days <= censor_age_days) %>%
  select(-censor_age_days, -censor_age_months)

classifier <- read_excel(PATH_CLASSIFIER)
timeline_info <- suppressWarnings(timeline_data(df_sz, classifier, censor_ages))
demographics <- read_excel(PATH_CITIZEN_DATA, sheet = "demographics")

missing_patients <- setdiff(figure_16_patients, df_duration$patient_uuid)
if (length(missing_patients) > 0) {
  stop(
    "Figure 16 patient IDs missing from medication data: ",
    paste(missing_patients, collapse = ", ")
  )
}

make_figure_16_panel <- function(patient_uuid) {
  patient_data <- prepare_patient_chart_data(
    pt = patient_uuid,
    df_duration = df_duration,
    df_type = df_type,
    timeline_data = timeline_info,
    demographics = demographics
  )

  panel_plot <- plot_patient_chart(patient_data)

  # The reference was assembled from reduced-size patient charts. Scale fixed
  # geom dimensions so points and intervals retain those proportions in a 2x2
  # patchwork rather than appearing as full-page chart marks.
  for (layer_index in seq_along(panel_plot$layers)) {
    if (!is.null(panel_plot$layers[[layer_index]]$aes_params$size)) {
      panel_plot$layers[[layer_index]]$aes_params$size <-
        panel_plot$layers[[layer_index]]$aes_params$size * 0.48
    }
    if (!is.null(panel_plot$layers[[layer_index]]$aes_params$linewidth)) {
      panel_plot$layers[[layer_index]]$aes_params$linewidth <-
        panel_plot$layers[[layer_index]]$aes_params$linewidth * 0.48
    }
  }

  panel_plot +
    guides(shape = "none") +
    theme(
      plot.title = element_text(size = 4.2, hjust = 0),
      axis.title.x = element_text(size = 5.5),
      axis.text.x = element_text(size = 4.5),
      axis.text.y = element_text(size = 7, color = "#222222"),
      axis.ticks = element_line(linewidth = 0.25),
      panel.grid.major = element_line(color = "#8C8C8C", linewidth = 0.22),
      panel.grid.minor = element_line(color = "#CFCFCF", linewidth = 0.18),
      panel.border = element_rect(linewidth = 0.35),
      plot.margin = margin(12, 18, 12, 18)
    )
}

figure_16_plots <- lapply(figure_16_patients, make_figure_16_panel)

figure_16_patchwork <- wrap_plots(figure_16_plots, ncol = 2, byrow = TRUE) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(26, 32, 26, 32)
    )
  ) &
  theme(legend.position = "none")

# Build the patchwork grob on a null device so sourcing this script does not
# leave an incidental Rplots.pdf in the project root.
grDevices::pdf(NULL)
figure_16_patchwork_grob <- patchwork::patchworkGrob(figure_16_patchwork)
grDevices::dev.off()

# Add the outer frame and quadrant dividers visible in the reference page.
figure_16 <- grid::grobTree(
  figure_16_patchwork_grob,
  grid::segmentsGrob(
    x0 = grid::unit(0.5, "npc"), x1 = grid::unit(0.5, "npc"),
    y0 = grid::unit(0, "npc"), y1 = grid::unit(1, "npc"),
    gp = grid::gpar(col = "black", lwd = 1.2)
  ),
  grid::segmentsGrob(
    x0 = grid::unit(0, "npc"), x1 = grid::unit(1, "npc"),
    y0 = grid::unit(0.5, "npc"), y1 = grid::unit(0.5, "npc"),
    gp = grid::gpar(col = "black", lwd = 1.2)
  ),
  grid::rectGrob(
    gp = grid::gpar(col = "black", fill = NA, lwd = 1.2)
  )
)

dir.create(FIGS, recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = file.path(FIGS, "fig_16.jpeg"),
  plot = figure_16,
  width = 13.33,
  height = 7.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(FIGS, "fig_16.pdf"),
  plot = figure_16,
  width = 13.33,
  height = 7.5,
  units = "in",
  device = cairo_pdf,
  bg = "white"
)

message(
  "Figure 16 written to: ",
  normalizePath(file.path(FIGS, "fig_16.jpeg")),
  " and ",
  normalizePath(file.path(FIGS, "fig_16.pdf"))
)
