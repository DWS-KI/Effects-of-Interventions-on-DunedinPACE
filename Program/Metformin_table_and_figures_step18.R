# =============================================================================
# Metformin_table_and_figures_step18.R
# =============================================================================
# This script creates:
#   1. Supplementary Table S5: Metformin Results Across All Analyses
#   2. Forest plot for anti-aging interventions comparison
#   3. Forest plot for Metformin across analyses
#   4. Forest plots for sensitivity analyses (diet, exercise type, vitamins)
#   5. Forest plots for subgroup analyses (sex, age, ethnicity)
#
# It loads IPW and Standardization results from various analyses and extracts
# Metformin-specific results into a summary table.
#
# Note: Each plot is saved in two formats:
#   1. PNG file (.png) - standalone image for direct viewing or external use
#   2. R object (no extension) - ggplot/patchwork object loaded by Manuscript.qmd for
#      rendering with Quarto's figure sizing/resolution controls
#
# Dependencies: Requires outputs from steps 7, 8, and 10-17; the vitamins
# sensitivity figure also uses step 22 when its outputs are available.
# =============================================================================

# -----------------------------------------------------------------------------
# LOAD PACKAGES
# -----------------------------------------------------------------------------

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

message("Step 18: Creating Metformin summary table and forest plots")

# -----------------------------------------------------------------------------
# LOAD HELPER FUNCTIONS
# -----------------------------------------------------------------------------

source(here::here("Program", "Functions", "forest_plot_functions.R"))

# -----------------------------------------------------------------------------
# LOAD ALL REQUIRED DATASETS
# -----------------------------------------------------------------------------

load(here::here("Output", "IPW_results_df"))
load(here::here("Output", "Standardization_results_df"))
load(here::here("Output", "IPW_subgroup_male_df"))
load(here::here("Output", "Standardization_subgroup_male_df"))
load(here::here("Output", "IPW_subgroup_female_df"))
load(here::here("Output", "Standardization_subgroup_female_df"))
load(here::here("Output", "IPW_subgroup_younger_df"))
load(here::here("Output", "Standardization_subgroup_younger_df"))
load(here::here("Output", "IPW_subgroup_older_df"))
load(here::here("Output", "Standardization_subgroup_older_df"))
load(here::here("Output", "IPW_subgroup_european_df"))
load(here::here("Output", "Standardization_subgroup_european_df"))
load(here::here("Output", "IPW_subgroup_noneuropean_df"))
load(here::here("Output", "Standardization_subgroup_noneuropean_df"))
load(here::here("Output", "IPW_subgroup_exercise_freq_df"))
load(here::here("Output", "Standardization_subgroup_exercise_freq_df"))
load(here::here("Output", "IPW_sensitivity_diet_df"))
load(here::here("Output", "Standardization_sensitivity_diet_df"))
load(here::here("Output", "IPW_sensitivity_exercise_type_df"))
load(here::here("Output", "Standardization_sensitivity_exercise_type_df"))

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# Function to extract and format Metformin row
extract_metformin <- function(ipw_df, std_df, analysis_name) {
  ipw_met <- ipw_df |> filter(grepl("Metformin", treatment, ignore.case = TRUE))
  std_met <- std_df |> filter(grepl("Metformin", treatment, ignore.case = TRUE))
  
  if (nrow(ipw_met) == 0 || nrow(std_met) == 0) return(NULL)
  
  tibble(
    Analysis = analysis_name,
    `ATE (IPW)` = sprintf("%.2f%%", ipw_met$ate_estimate * 100),
    `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", ipw_met$ci_lower * 100, ipw_met$ci_upper * 100),
    SMD = ipw_met$SMD,
    `ATE (G-comp)` = sprintf("%.2f%%", std_met$ate_estimate * 100),
    `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", std_met$ci_lower * 100, std_met$ci_upper * 100),
    `N Users` = ipw_met$n_treated,
    `N Non-users` = ipw_met$n_control
  )
}

# Function to extract exercise frequency subgroups (uses 'subgroup' column)
extract_exercise_freq <- function(ipw_df, std_df) {
  results <- list()
  for (i in seq_len(nrow(ipw_df))) {
    ipw_row <- ipw_df[i, ]
    std_row <- std_df[i, ]
    results[[i]] <- tibble(
      Analysis = ipw_row$subgroup,
      `ATE (IPW)` = sprintf("%.2f%%", ipw_row$ate_estimate * 100),
      `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", ipw_row$ci_lower * 100, ipw_row$ci_upper * 100),
      SMD = ipw_row$SMD,
      `ATE (G-comp)` = sprintf("%.2f%%", std_row$ate_estimate * 100),
      `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", std_row$ci_lower * 100, std_row$ci_upper * 100),
      `N Users` = ipw_row$n_treated,
      `N Non-users` = ipw_row$n_control
    )
  }
  bind_rows(results)
}

# -----------------------------------------------------------------------------
# CREATE METFORMIN SUMMARY TABLE
# -----------------------------------------------------------------------------

# Extract Metformin from each analysis
metformin_summary <- bind_rows(
  extract_metformin(IPW_results_df, Standardization_results_df, "Main"),
  extract_metformin(IPW_subgroup_male_df, Standardization_subgroup_male_df, "Males"),
  extract_metformin(IPW_subgroup_female_df, Standardization_subgroup_female_df, "Females"),
  extract_metformin(IPW_subgroup_younger_df, Standardization_subgroup_younger_df, "Younger"),
  extract_metformin(IPW_subgroup_older_df, Standardization_subgroup_older_df, "Older"),
  extract_metformin(IPW_subgroup_european_df, Standardization_subgroup_european_df, "European"),
  extract_metformin(IPW_subgroup_noneuropean_df, Standardization_subgroup_noneuropean_df, "Non-European"),
  extract_exercise_freq(IPW_subgroup_exercise_freq_df, Standardization_subgroup_exercise_freq_df)
)

# -----------------------------------------------------------------------------
# SAVE OUTPUT
# -----------------------------------------------------------------------------

output_path <- here::here("Output", "metformin_summary")
save(metformin_summary, file = output_path)
message("Saved metformin_summary to: ", output_path)

# -----------------------------------------------------------------------------
# CREATE ANTI-AGING INTERVENTIONS FOREST PLOT
# -----------------------------------------------------------------------------

# Create Forest_plots directory if it doesn't exist
forest_plots_dir <- here::here("Output", "Forest_plots")
if (!dir.exists(forest_plots_dir)) {
  dir.create(forest_plots_dir, recursive = TRUE)
}

# Prepare data for interventions forest plot
ipw_for_plot <- IPW_results_df |>
  mutate(
    Treatment = treatment,
    `ATE (IPW)` = sprintf("%.2f%%", ate_estimate * 100),
    `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100),
    ipw_ate = ate_estimate
  ) |>
  select(Treatment, `ATE (IPW)`, `CI (IPW)`, ipw_ate)

std_for_plot <- Standardization_results_df |>
  mutate(
    Treatment = treatment,
    `ATE (G-comp)` = sprintf("%.2f%%", ate_estimate * 100),
    `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100)
  ) |>
  select(Treatment, `ATE (G-comp)`, `CI (G-comp)`)

# Merge and order by IPW ATE
interventions_forest_data <- ipw_for_plot |>
  full_join(std_for_plot, by = "Treatment") |>
  arrange(ipw_ate) |>
  select(Treatment, `ATE (IPW)`, `CI (IPW)`, `ATE (G-comp)`, `CI (G-comp)`)

# Create and save interventions forest plot
interventions_forest_plot <- create_comparison_forest_plot(
  results_table = interventions_forest_data,
  label_col = "Treatment",
  x_label = " ",
  title = "Effect of Interventions on DunedinPACE",
  subtitle = "Comparing users to non-users",
  output_path = here::here("Output", "Forest_plots", "interventions_forest_plot.png")
)

# Save plot object for manuscript
save(interventions_forest_plot, file = here::here("Output", "Forest_plots", "interventions_forest_plot"))
message("Saved interventions_forest_plot to: Output/Forest_plots/interventions_forest_plot")

# -----------------------------------------------------------------------------
# CREATE METFORMIN FOREST PLOT
# -----------------------------------------------------------------------------

metformin_forest_plot <- create_comparison_forest_plot(
  results_table = metformin_summary,
  label_col = "Analysis",
  x_label = " ",
  title = "Effect of Metformin on DunedinPACE",
  subtitle = "Across main and subgroup analyses",
  output_path = here::here("Output", "Forest_plots", "metformin_forest_plot.png")
)

# -----------------------------------------------------------------------------
# HELPER FUNCTION FOR SUPPLEMENTARY FOREST PLOTS
# -----------------------------------------------------------------------------

# Function to prepare data and create forest plot for any analysis
# Note: Each plot is saved in two formats:
#   1. PNG file (.png) - standalone image for direct viewing or external use
#   2. R object (no extension) - ggplot/patchwork object loaded by Manuscript.qmd for
#      rendering with Quarto's figure sizing/resolution controls
create_supplementary_forest_plot <- function(ipw_df, std_df, title, subtitle, filename) {
  
  # Prepare IPW data
  ipw_for_plot <- ipw_df |>
    mutate(
      Treatment = treatment,
      `ATE (IPW)` = sprintf("%.2f%%", ate_estimate * 100),
      `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100),
      ipw_ate = ate_estimate
    ) |>
    select(Treatment, `ATE (IPW)`, `CI (IPW)`, ipw_ate)
  
  # Prepare Standardization data
  std_for_plot <- std_df |>
    mutate(
      Treatment = treatment,
      `ATE (G-comp)` = sprintf("%.2f%%", ate_estimate * 100),
      `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100)
    ) |>
    select(Treatment, `ATE (G-comp)`, `CI (G-comp)`)
  
  # Merge and order by IPW ATE
  forest_data <- ipw_for_plot |>
    full_join(std_for_plot, by = "Treatment") |>
    arrange(ipw_ate) |>
    select(Treatment, `ATE (IPW)`, `CI (IPW)`, `ATE (G-comp)`, `CI (G-comp)`)
  
  # Create and save forest plot
  plot <- create_comparison_forest_plot(
    results_table = forest_data,
    label_col = "Treatment",
    x_label = " ",
    title = title,
    subtitle = subtitle,
    output_path = here::here("Output", "Forest_plots", paste0(filename, ".png"))
  )
  
  # Save plot object for manuscript
  save(plot, file = here::here("Output", "Forest_plots", filename))
  message("Saved ", filename, " to: Output/Forest_plots/", filename)
  
  return(plot)
}

# Function to prepare data and create A/IPW and B/G-computation overlay panels
create_supplementary_overlay_forest_plot <- function(ipw_df1, std_df1, dataset1_name,
                                                     ipw_df2, std_df2, dataset2_name,
                                                     title, subtitle, filename) {
  
  # Prepare Dataset 1
  ipw_for_plot1 <- ipw_df1 |>
    mutate(
      Treatment = treatment,
      `ATE (IPW)` = sprintf("%.2f%%", ate_estimate * 100),
      `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100),
      ipw_ate = ate_estimate
    ) |>
    select(Treatment, `ATE (IPW)`, `CI (IPW)`, ipw_ate)
  
  std_for_plot1 <- std_df1 |>
    mutate(
      Treatment = treatment,
      `ATE (G-comp)` = sprintf("%.2f%%", ate_estimate * 100),
      `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100)
    ) |>
    select(Treatment, `ATE (G-comp)`, `CI (G-comp)`)
  
  forest_data1 <- ipw_for_plot1 |> full_join(std_for_plot1, by = "Treatment") |> arrange(ipw_ate)
  
  # Prepare Dataset 2
  ipw_for_plot2 <- ipw_df2 |>
    mutate(
      Treatment = treatment,
      `ATE (IPW)` = sprintf("%.2f%%", ate_estimate * 100),
      `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100)
    ) |>
    select(Treatment, `ATE (IPW)`, `CI (IPW)`)
  
  std_for_plot2 <- std_df2 |>
    mutate(
      Treatment = treatment,
      `ATE (G-comp)` = sprintf("%.2f%%", ate_estimate * 100),
      `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", ci_lower * 100, ci_upper * 100)
    ) |>
    select(Treatment, `ATE (G-comp)`, `CI (G-comp)`)
  
  forest_data2 <- ipw_for_plot2 |> full_join(std_for_plot2, by = "Treatment")
  
  plot <- create_overlay_forest_plot(
    results_table_1 = forest_data1,
    results_table_2 = forest_data2,
    dataset1_name = dataset1_name,
    dataset2_name = dataset2_name,
    label_col = "Treatment",
    x_label = " ",
    title = title,
    subtitle = subtitle,
    output_path = here::here("Output", "Forest_plots", paste0(filename, ".png"))
  )
  
  save(plot, file = here::here("Output", "Forest_plots", filename))
  message("Saved ", filename, " to: Output/Forest_plots/", filename)
  
  return(plot)
}

# -----------------------------------------------------------------------------
# CREATE OVERLAY FOREST PLOTS
# -----------------------------------------------------------------------------

message("Creating overlay forest plots...")

# Diet sensitivity analysis vs main analysis
sensitivity_diet_forest_plot <- create_supplementary_overlay_forest_plot(
  ipw_df1 = IPW_sensitivity_diet_df, std_df1 = Standardization_sensitivity_diet_df, dataset1_name = "Diet Adjusted",
  ipw_df2 = IPW_results_df, std_df2 = Standardization_results_df, dataset2_name = "Main Analysis",
  title = "Effects of Geroprotective Interventions on DunedinPACE",
  subtitle = "Sensitivity analysis: Diet adjustment vs Main Analysis",
  filename = "sensitivity_diet_forest_plot"
)

# Exercise type sensitivity analysis vs main analysis
sensitivity_exercise_type_forest_plot <- create_supplementary_overlay_forest_plot(
  ipw_df1 = IPW_sensitivity_exercise_type_df, std_df1 = Standardization_sensitivity_exercise_type_df, dataset1_name = "Exercise Type Adjusted",
  ipw_df2 = IPW_results_df, std_df2 = Standardization_results_df, dataset2_name = "Main Analysis",
  title = "Effects of Geroprotective Interventions on DunedinPACE",
  subtitle = "Sensitivity analysis: Exercise type adjustment vs Main Analysis",
  filename = "sensitivity_exercise_type_forest_plot"
)

# Vitamins sensitivity analysis vs main analysis
# Older biomarker workflows omit step 22. Skip only when both inputs are absent;
# a partial pair is an error rather than an incomplete sensitivity figure.
vitamins_result_paths <- here::here("Output", c(
  "IPW_sensitivity_vitamins_df", "Standardization_sensitivity_vitamins_df"
))
if (any(file.exists(vitamins_result_paths))) {
  if (!all(file.exists(vitamins_result_paths))) {
    stop("Both vitamin-adjusted result files are required. Run sensitivity step 22.")
  }
  load(vitamins_result_paths[1])
  load(vitamins_result_paths[2])

  sensitivity_vitamins_forest_plot <- create_supplementary_overlay_forest_plot(
    ipw_df1 = IPW_sensitivity_vitamins_df, std_df1 = Standardization_sensitivity_vitamins_df, dataset1_name = "Vitamins Adjusted",
    ipw_df2 = filter(IPW_results_df, treatment != "Vitamins"),
    std_df2 = filter(Standardization_results_df, treatment != "Vitamins"), dataset2_name = "Main Analysis",
    title = "Effects of Geroprotective Interventions on DunedinPACE",
    subtitle = "Sensitivity analysis: Vitamin adjustment vs Main Analysis",
    filename = "sensitivity_vitamins_forest_plot"
  )
} else {
  message("Skipping vitamin-adjusted sensitivity figure: step 22 outputs are absent.")
}

# Men vs Women
subgroup_sex_forest_plot <- create_supplementary_overlay_forest_plot(
  ipw_df1 = IPW_subgroup_male_df, std_df1 = Standardization_subgroup_male_df, dataset1_name = "Men",
  ipw_df2 = IPW_subgroup_female_df, std_df2 = Standardization_subgroup_female_df, dataset2_name = "Women",
  title = "Effects of Geroprotective Interventions on DunedinPACE",
  subtitle = "Subgroup analysis: Men vs Women",
  filename = "subgroup_sex_forest_plot"
)

# Younger vs Older
subgroup_age_forest_plot <- create_supplementary_overlay_forest_plot(
  ipw_df1 = IPW_subgroup_younger_df, std_df1 = Standardization_subgroup_younger_df, dataset1_name = "Younger",
  ipw_df2 = IPW_subgroup_older_df, std_df2 = Standardization_subgroup_older_df, dataset2_name = "Older",
  title = "Effects of Geroprotective Interventions on DunedinPACE",
  subtitle = "Subgroup analysis: Younger vs Older",
  filename = "subgroup_age_forest_plot"
)

# European vs Non-European
subgroup_ethnicity_forest_plot <- create_supplementary_overlay_forest_plot(
  ipw_df1 = IPW_subgroup_european_df, std_df1 = Standardization_subgroup_european_df, dataset1_name = "European",
  ipw_df2 = IPW_subgroup_noneuropean_df, std_df2 = Standardization_subgroup_noneuropean_df, dataset2_name = "Non-European",
  title = "Effects of Geroprotective Interventions on DunedinPACE",
  subtitle = "Subgroup analysis: European vs Non-European ancestry",
  filename = "subgroup_ethnicity_forest_plot"
)

# -----------------------------------------------------------------------------
# CREATE TRAINING/EXERCISE EFFECTS FOREST PLOT
# -----------------------------------------------------------------------------

message("Creating training effects forest plot...")

# Load training results from step 10
load(here::here("Output", "training_analysis_results.RData"))

# Create forest plot
training_forest_plot <- create_training_forest_plot(
  training_results_table,
  output_path = here::here("Output", "Forest_plots", "training_forest_plot.png")
)

# Save plot object for manuscript
save(training_forest_plot, file = here::here("Output", "Forest_plots", "training_forest_plot"))
message("Saved training_forest_plot to: Output/Forest_plots/training_forest_plot")

# -----------------------------------------------------------------------------
# CREATE EXERCISE BY METFORMIN FOREST PLOT
# -----------------------------------------------------------------------------

message("Creating exercise by metformin forest plot...")

# Load exercise by metformin summary from step 17
load(here::here("Output", "exercise_by_metformin_summary"))

# Create forest plot using the summary table (uses Analysis column as labels)
exercise_by_metformin_forest_plot <- create_comparison_forest_plot(
  results_table = exercise_by_metformin_summary,
  label_col = "Analysis",
  x_label = " ",
  title = "Effect of Exercise on DunedinPACE stratified by Metformin Use",
  subtitle = "Exercise 5+ times/week compared to 0-4 times/week",
  output_path = here::here("Output", "Forest_plots", "exercise_by_metformin_forest_plot.png")
)

# Save plot object for manuscript
save(exercise_by_metformin_forest_plot, file = here::here("Output", "Forest_plots", "exercise_by_metformin_forest_plot"))
message("Saved exercise_by_metformin_forest_plot to: Output/Forest_plots/exercise_by_metformin_forest_plot")

# Compose only the two main-text Love/forest figures using saved plot inputs.
# All standalone figure calls above retain their existing styles.
source(here::here("Program", "Functions", "combined_plot_functions.R"))
render_combined_figures(here::here("Output"))

message("Step 18 complete")
