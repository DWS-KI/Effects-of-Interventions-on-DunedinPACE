# =============================================================================
# STEP 19: HISTOGRAM OF CONCURRENT GEROPROTECTIVE INTERVENTIONS
# =============================================================================
# This script:
#   1. Calculates the total number of concurrent interventions per participant
#   2. Creates a supplementary histogram of the intervention count distribution
#   3. Calculates the 90th percentile of the intervention count distribution
#   4. Creates a summary table for concurrent intervention counts
#   5. Saves the figure, table, and percentile for manuscript use
#
# Output:
#   - Output/concurrent_treatments_histogram.png
#   - Output/concurrent_treatments_summary_df
#   - Output/total_treatments_90th_percentile
# =============================================================================

library(here)
library(dplyr)
library(ggplot2)

message("Step 19: Creating histogram of concurrent geroprotective interventions")

intervention_cols <- c(
  "Metformin",
  "Antioxidants",
  "Fish.oil.omega",
  "Workout.supplements",
  "Herbal.supplements",
  "Vitamins",
  "Minerals",
  "TA.65",
  "NR.NAD",
  "Sulphoraphane.broccoli.extract",
  "Rapamycin",
  "DHEA",
  "Resveratrol",
  "Antiaging.Exosome",
  "Antiaging.Hormone.Replacement",
  "Antiaging.Senolytics",
  "Antiaging.Stem.Cell",
  "Antiaging.NAD"
)

if (!exists("data_after_step4")) {
  stop("`data_after_step4` is required in memory for step 19 but was not found.")
}

missing_cols <- setdiff(intervention_cols, names(data_after_step4))
if (length(missing_cols) > 0) {
  stop(
    "The following intervention columns are missing from `data_after_step4`: ",
    paste(missing_cols, collapse = ", ")
  )
}

df <- data_after_step4 |>
  mutate(
    across(
      all_of(intervention_cols),
      ~ {
        if (is.factor(.x)) {
          as.numeric(as.character(.x))
        } else {
          as.numeric(.x)
        }
      }
    ),
    total_treatments = rowSums(pick(all_of(intervention_cols)), na.rm = TRUE)
  )

if (nrow(df) == 0) {
  stop("No participants were found in `data_after_step4`.")
}

total_treatments_90th_percentile <- quantile(
  df$total_treatments,
  probs = 0.90,
  na.rm = TRUE
)

concurrent_treatments_summary_df <- df |>
  count(total_treatments, name = "Number of Participants") |>
  mutate(
    `Concurrent Interventions` = total_treatments,
    `Percentage of All Participants` = round(`Number of Participants` / nrow(df) * 100, 2)
  ) |>
  select(
    `Concurrent Interventions`,
    `Percentage of All Participants`,
    `Number of Participants`
  )

message(
  "90th percentile of total_treatments: ",
  unname(total_treatments_90th_percentile)
)

histogram_breaks <- seq(
  -0.5,
  max(df$total_treatments, na.rm = TRUE) + 0.5,
  by = 1
)

concurrent_treatments_histogram <- ggplot(df, aes(x = total_treatments)) +
  geom_histogram(
    aes(y = after_stat(count / nrow(df) * 100)),
    breaks = histogram_breaks,
    fill = "#2C7FB8",
    color = "white",
    linewidth = 0.3,
    na.rm = TRUE
  ) +
  scale_x_continuous(
    breaks = seq(0, max(df$total_treatments, na.rm = TRUE), by = 1),
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Number of Concurrent Interventions",
    y = "Percentage of All Participants"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black")
  )

ggsave(
  filename = here::here("Output", "concurrent_treatments_histogram.png"),
  plot = concurrent_treatments_histogram,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

save(
  concurrent_treatments_summary_df,
  file = here::here("Output", "concurrent_treatments_summary_df")
)

save(
  total_treatments_90th_percentile,
  file = here::here("Output", "total_treatments_90th_percentile")
)

message("Saved concurrent treatment histogram to: Output/concurrent_treatments_histogram.png")
message("Saved concurrent treatment summary table to: Output/concurrent_treatments_summary_df")
message("Saved 90th percentile to: Output/total_treatments_90th_percentile")
