# =============================================================================
# STEP 24: MUTUAL-ADJUSTMENT SENSITIVITY FIGURE VS MAIN ANALYSIS
# =============================================================================
# Presentation only: load results from steps 7, 8 and 23 and reuse the vitamin
# sensitivity figure's two-panel forest-plot layout. No models are fitted here.

local({
  library(here)
  library(dplyr)
  source(here::here("Program", "Functions", "forest_plot_functions.R"), local = TRUE)

  load(here::here("Output", "IPW_results_df"))
  load(here::here("Output", "Standardization_results_df"))
  load(here::here("Output", "IPW_sensitivity_mutual_adjustment_df"))
  load(here::here("Output", "Standardization_sensitivity_mutual_adjustment_df"))

  # Step 23 is the single source of the fixed intervention set.
  treatments <- IPW_sensitivity_mutual_adjustment_df$treatment
  for (results in list(IPW_sensitivity_mutual_adjustment_df,
                       Standardization_sensitivity_mutual_adjustment_df)) {
    if (nrow(results) != 12L || anyNA(results$treatment) ||
        anyDuplicated(results$treatment) || !setequal(results$treatment, treatments) ||
        anyNA(results$outcome) || !all(results$outcome == "DunedinPACE")) {
      stop("Step 23 must provide one DunedinPACE result per intervention in both methods.",
           call. = FALSE)
    }
  }

  prepare_forest_data <- function(ipw_df, std_df) {
    ipw_df <- filter(ipw_df, outcome == "DunedinPACE", treatment %in% treatments)
    std_df <- filter(std_df, outcome == "DunedinPACE", treatment %in% treatments)
    for (results in list(ipw_df, std_df)) {
      if (nrow(results) != length(treatments) || anyDuplicated(results$treatment) ||
          !setequal(results$treatment, treatments)) {
        stop("Main and sensitivity results must cover the same 12 interventions in both methods.",
             call. = FALSE)
      }
      estimates <- results[c("ate_estimate", "ci_lower", "ci_upper")]
      if (!all(vapply(estimates, function(x) is.numeric(x) && all(is.finite(x)), logical(1))) ||
          any(results$ci_lower > results$ci_upper)) {
        stop("Cannot plot missing, nonfinite, or reversed mutual-adjustment comparison estimates.",
             call. = FALSE)
      }
    }

    ipw_df |>
      transmute(
        Treatment = treatment, outcome,
        `ATE (IPW)` = sprintf("%.2f%%", ate_estimate * 100),
        `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100),
        ipw_ate = ate_estimate
      ) |>
      left_join(
        std_df |>
          transmute(
            Treatment = treatment, outcome,
            `ATE (G-comp)` = sprintf("%.2f%%", ate_estimate * 100),
            `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100)
          ),
        by = c("Treatment", "outcome")
      ) |>
      arrange(ipw_ate)
  }

  sensitivity_forest_data <- prepare_forest_data(
    IPW_sensitivity_mutual_adjustment_df, Standardization_sensitivity_mutual_adjustment_df
  )
  main_forest_data <- prepare_forest_data(IPW_results_df, Standardization_results_df)
  forest_dir <- here::here("Output", "Forest_plots")
  dir.create(forest_dir, recursive = TRUE, showWarnings = FALSE)

  sensitivity_mutual_adjustment_forest_plot <- create_overlay_forest_plot(
    results_table_1 = sensitivity_forest_data,
    results_table_2 = main_forest_data,
    dataset1_name = "Mutually Adjusted",
    dataset2_name = "Main Analysis",
    label_col = "Treatment",
    x_label = " ",
    title = "Effects of Geroprotective Interventions on DunedinPACE",
    subtitle = "Sensitivity analysis: Mutual adjustment vs Main Analysis",
    output_path = file.path(forest_dir, "sensitivity_mutual_adjustment_forest_plot.png")
  )
  save(sensitivity_mutual_adjustment_forest_plot,
       file = file.path(forest_dir, "sensitivity_mutual_adjustment_forest_plot"))
  message("Step 24 complete: saved mutual-adjustment sensitivity figure.")
})
