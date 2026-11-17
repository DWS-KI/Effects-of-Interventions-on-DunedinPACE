# =============================================================================
# TRAINING ANALYSIS STEP 10: EXERCISE LEVELS VS NEVER
# =============================================================================
# This script performs IPW and Standardization analysis for exercise levels
# using "Never" (or a specified level) as the common control group.

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(stringr)

# -----------------------------------------------------------------------------
# Source Functions
# -----------------------------------------------------------------------------
source(here::here("Program", "Functions", "IPW_functions.R"))
source(here::here("Program", "Functions", "Standardization_functions.R"))
source(here::here("Program", "Functions", "forest_plot_functions.R"))

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------
load(here::here("Output", "data_after_step4"))
data <- data_after_step4

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
# Define the exercise columns to analyze
treatment_cols <- c(
  "Exercise.1to2.per.week",
  "Exercise.3to4.per.week",
  "Exercise.5to7.per.week",
  "Exercise.8plus.per.week"
)
control_col <- "Exercise.Never"

# Define outcomes
outcome_list <- c("DunedinPACE")

# Define covariates (same as Step 6, but we will remove exercise)
covariates_base <- c(
  "Decimal.Chronological.Age",
  "Biological.Sex",
  "Level.of.Education",
  "Tobacco.Use.Binary",
  "Alcohol.per.week",
  "Exercise.per.week", # Will be removed
  "BMI",
  "Stress.Level",
  "Hours.of.sleep.per.night"
)

# Remove exercise from covariates since it's the treatment
covariates <- setdiff(covariates_base, c("Exercise.per.week", treatment_cols, control_col))

# Bootstrap iterations (set centrally in Master_parallel.R)
n_bootstrap <- getOption("n_bootstrap", default = 1000)

# -----------------------------------------------------------------------------
# Prepare Data
# -----------------------------------------------------------------------------
cat("Preparing exercise treatment variables...\n")

# Check if control column exists
if (!control_col %in% names(data)) {
  # Fallback for mock data if Exercise.Never doesn't exist
  warning(paste("Control column", control_col, "not found. Checking for Exercise.per.week to derive it."))
  if ("Exercise.per.week" %in% names(data)) {
     # Try to find a "Never" level or use the first level
     levels <- unique(data$Exercise.per.week)
     never_level <- grep("Never", levels, value = TRUE)
     if (length(never_level) == 0) never_level <- levels[1] # Fallback
     
     cat("Creating temporary control column from level:", never_level, "\n")
     data[[control_col]] <- ifelse(data$Exercise.per.week == never_level, 1, 0)
  } else {
     stop("Cannot find control column or source column.")
  }
}

treatment_vars <- c()

for (t_col in treatment_cols) {
  
  # Check if treatment column exists
  if (!t_col %in% names(data)) {
     # Fallback for mock data
     warning(paste("Treatment column", t_col, "not found. Creating mock version if possible."))
     if ("Exercise.per.week" %in% names(data)) {
        # Just assign a random level that isn't the control level for testing
        levels <- unique(data$Exercise.per.week)
        # This is just a placeholder to make the script run on mock data
        # In real data, the column should exist
        data[[t_col]] <- 0 
     } else {
        next
     }
  }

  # Create a sanitized variable name
  var_name <- paste0(t_col, "_vs_Never")
  
  # Create the variable: 1 = Treated, 0 = Control (Never), NA = Others
  # We assume t_col and control_col are binary (1/0) or logical
  
  data[[var_name]] <- case_when(
    data[[t_col]] == 1 ~ 1,
    data[[control_col]] == 1 ~ 0,
    TRUE ~ NA_real_ # Set others to NA to exclude them
  )
  
  treatment_vars <- c(treatment_vars, var_name)
  cat("Created variable:", var_name, "- Treated:", sum(data[[var_name]] == 1, na.rm=TRUE), 
      "Control:", sum(data[[var_name]] == 0, na.rm=TRUE), "\n")
}

# -----------------------------------------------------------------------------
# Run IPW Analysis
# -----------------------------------------------------------------------------
cat("\nRunning IPW Analysis...\n")

IPW_results <- IPW(
  data = data,
  treatment_list = treatment_vars,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  love_plot_dir = "Output/love_plots/exercise_effects"
)

# -----------------------------------------------------------------------------
# Run Standardization Analysis
# -----------------------------------------------------------------------------
cat("\nRunning Standardization Analysis...\n")

Standardization_results <- Standardization(
  data = data,
  treatment_list = treatment_vars,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE
)

# -----------------------------------------------------------------------------
# Combine and Format Results
# -----------------------------------------------------------------------------
cat("\nCombining results...\n")

# Format IPW
ipw_formatted <- IPW_results |>
  mutate(
    `ATE (IPW)` = ate_estimate,
    `95% CI (IPW)` = sprintf("[%.4f, %.4f]", ci_lower, ci_upper),
    `Balance (SMD)` = SMD  # Keep original value
  ) |>
  select(Treatment = treatment, `ATE (IPW)`, `95% CI (IPW)`, `Balance (SMD)`, n_treated, n_control)

# Format Standardization
std_formatted <- Standardization_results |>
  mutate(
    `ATE (G-comp)` = ate_estimate,
    `95% CI (G-comp)` = sprintf("[%.4f, %.4f]", ci_lower, ci_upper)
  ) |>
  select(Treatment = treatment, `ATE (G-comp)`, `95% CI (G-comp)`)

# Merge
training_results_table <- left_join(ipw_formatted, std_formatted, by = "Treatment") |>
  rename(`N Treated` = n_treated, `N Control` = n_control) |>
  select(Treatment, `ATE (IPW)`, `95% CI (IPW)`, `Balance (SMD)`, `ATE (G-comp)`, `95% CI (G-comp)`, `N Treated`, `N Control`)

# Clean up treatment names for display
training_results_table$Treatment <- str_remove(training_results_table$Treatment, "Exercise.")
training_results_table$Treatment <- str_remove(training_results_table$Treatment, "_vs_Never")
training_results_table$Treatment <- str_replace_all(training_results_table$Treatment, "\\.", " ")

# Order by exercise frequency
exercise_order <- c("1to2 per week", "3to4 per week", "5to7 per week", "8plus per week")
training_results_table <- training_results_table |>
  mutate(Treatment = factor(Treatment, levels = exercise_order)) |>
  arrange(Treatment) |>
  mutate(Treatment = as.character(Treatment))

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
output_path <- here::here("Output", "training_analysis_results.RData")
save(training_results_table, IPW_results, Standardization_results, file = output_path)

cat("Results saved to:", output_path, "\n")
print(training_results_table)

