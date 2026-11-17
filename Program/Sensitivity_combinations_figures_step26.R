# Presentation only: exact combinations and their constituent main-analysis rows.
local({
  library(here)
  library(dplyr)
  source(here::here("Program", "Functions", "forest_plot_functions.R"), local = TRUE)
  source(here::here("Program", "Functions", "intervention_combination_functions.R"), local = TRUE)

  load(here::here("Output", "IPW_results_df"))
  load(here::here("Output", "Standardization_results_df"))
  load(here::here("Output", "IPW_sensitivity_combinations_df"))
  load(here::here("Output", "Standardization_sensitivity_combinations_df"))
  selection <- readRDS(here::here("Output", "combination_selection.rds"))
  comparison <- prepare_combination_comparison(
    IPW_results_df, Standardization_results_df,
    IPW_sensitivity_combinations_df, Standardization_sensitivity_combinations_df,
    selection
  )
  forest_data <- comparison |>
    transmute(
      Intervention = sub("^Exact:[[:space:]]*", "", label),
      `ATE (IPW)` = sprintf("%.2f%%", ate_estimate_ipw * 100),
      `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", ci_lower_ipw * 100, ci_upper_ipw * 100),
      `ATE (G-comp)` = sprintf("%.2f%%", ate_estimate_std * 100),
      `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", ci_lower_std * 100, ci_upper_std * 100)
    )
  forest_dir <- here::here("Output", "Forest_plots")
  dir.create(forest_dir, recursive = TRUE, showWarnings = FALSE)
  sensitivity_combinations_forest_plot <- create_comparison_forest_plot(
    results_table = forest_data, label_col = "Intervention", x_label = " ",
    label_aware_spacing = TRUE,
    output_path = file.path(forest_dir, "sensitivity_combinations_forest_plot.png")
  )
  save(sensitivity_combinations_forest_plot,
       file = file.path(forest_dir, "sensitivity_combinations_forest_plot"))
  # The manuscript and Excel export consume exactly the same sorted rows as the plot.
  saveRDS(list(selection = selection, results = comparison),
          here::here("Output", "sensitivity_combinations_summary.rds"))
  message("Step 26 complete: saved combination figure and supplementary-table inputs.")
})
