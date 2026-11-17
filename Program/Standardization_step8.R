# =============================================================================
# STANDARDIZATION STEP 8: G-COMPUTATION WITH BOOTSTRAP CONFIDENCE INTERVALS
# =============================================================================
# This script performs G-computation/standardization analysis with
# bootstrap-based confidence intervals for multiple treatments
#
# Key Features:
# 1. Modular design with functions imported from Standardization_functions.R
# 2. Bootstrap resampling to account for uncertainty in outcome model
# 3. Support for multiple treatments and outcomes
# 4. Automated result export to RData
#
# Method:
# For each treatment-outcome combination:
#   1. Create binary treatment variable
#   2. Remove missing values for outcome model variables
#   3. Fit outcome model: Y ~ treatment + covariates (linear regression)
#   4. Predict potential outcomes under treatment (Y1) and control (Y0) for everyone
#   5. Estimate ATE as mean(Y1) - mean(Y0)
#   6. Bootstrap (M iterations):
#      a. Resample with replacement
#      b. Refit outcome model
#      c. Predict potential outcomes
#      d. Re-estimate ATE
#   7. Calculate 95% CI from empirical bootstrap distribution

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
cat("Loading required packages...\n")

# Check and install packages if needed
required_packages <- c("here", "dplyr")

for (pkg in required_packages) {
  if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# -----------------------------------------------------------------------------
# Source Standardization Functions
# -----------------------------------------------------------------------------
cat("Loading Standardization functions...\n")
source(here::here("Program", "Functions", "Standardization_functions.R"))

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------
cat("Loading data from Step 4...\n")
load(here::here("Output", "data_after_step4"))

data <- data_after_step4

cat("  Data loaded successfully\n")
cat("  Number of observations:", nrow(data), "\n")
cat("  Number of variables:", ncol(data), "\n")

# -----------------------------------------------------------------------------
# Define Analysis Parameters
# -----------------------------------------------------------------------------
cat("\nSetting analysis parameters...\n")

# Define treatments to analyze
# Add or remove treatment names as needed
treatment_list <- c(
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

# Define outcomes to analyze
# Currently configured for aging biomarkers
outcome_list <- c(
  "DunedinPACE"
  # "PhenoAge",          # Uncomment to add more outcomes
  # "GrimAge.PC"
)

# Define covariates for outcome model
# These are the confounders to adjust for
covariates <- c(
  "Decimal.Chronological.Age",
  "Biological.Sex",
  "Level.of.Education",
  "Tobacco.Use.Binary",
  "Alcohol.per.week",
  "Exercise.per.week",
  "BMI",
  "Stress.Level",
  "Hours.of.sleep.per.night"
)

# Number of bootstrap iterations (set centrally in Master_parallel.R)
n_bootstrap <- getOption("n_bootstrap", default = 1000)

cat("  Treatments:", paste(treatment_list, collapse = ", "), "\n")
cat("  Outcomes:", paste(outcome_list, collapse = ", "), "\n")
cat("  Covariates:", length(covariates), "\n")
cat("  Bootstrap iterations:", n_bootstrap, "\n")

# -----------------------------------------------------------------------------
# Verify Data Structure
# -----------------------------------------------------------------------------
cat("\nVerifying data structure...\n")

# Check that all specified variables exist in the data
all_vars <- c(treatment_list, outcome_list, covariates)
missing_vars <- all_vars[!all_vars %in% names(data)]

if (length(missing_vars) > 0) {
  stop("The following variables are not found in the data:\n  ",
       paste(missing_vars, collapse = ", "),
       "\n\nPlease check variable names and try again.")
}

cat("  All variables found in data\n")

# -----------------------------------------------------------------------------
# Run Standardization Analysis
# -----------------------------------------------------------------------------
cat("\nStarting G-computation/standardization analysis...\n")

# Call main Standardization function
# This will:
# - Loop through all treatment-outcome combinations
# - Fit outcome models
# - Predict potential outcomes
# - Perform bootstrap for CIs
# - Return data frame with results
Standardization_results_df <- Standardization(
  data = data,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap
)

# -----------------------------------------------------------------------------
# Display Results
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("FINAL RESULTS TABLE\n")
cat(strrep("=", 70), "\n")
print(Standardization_results_df)

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\nSaving results...\n")

# Save as RData file
output_path <- here::here("Output", "Standardization_results_df")
save(Standardization_results_df, file = output_path)

cat("  Results saved to:", paste0(output_path, ".RData"), "\n")

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("ANALYSIS SUMMARY\n")
cat(strrep("=", 70), "\n")

cat("Total treatment-outcome combinations analyzed:", nrow(Standardization_results_df), "\n")
cat("Bootstrap iterations per analysis:", n_bootstrap, "\n")

# Check for significant results (CI does not include 0)
significant_results <- Standardization_results_df[
  (Standardization_results_df$ci_lower > 0 & Standardization_results_df$ci_upper > 0) |
  (Standardization_results_df$ci_lower < 0 & Standardization_results_df$ci_upper < 0),
]

if (nrow(significant_results) > 0) {
  cat("\nStatistically significant effects (95% CI excludes 0):\n")
  for (i in 1:nrow(significant_results)) {
    cat("  -", significant_results$treatment[i], "->", significant_results$outcome[i],
        ": ATE =", round(significant_results$ate_estimate[i], 4),
        ", 95% CI = [", round(significant_results$ci_lower[i], 4), ",",
        round(significant_results$ci_upper[i], 4), "]\n")
  }
} else {
  cat("\nNo statistically significant effects detected at 95% confidence level.\n")
}

cat("\n")
cat(strrep("=", 70), "\n")
cat("SCRIPT COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 70), "\n")
cat("Analysis date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat(strrep("=", 70), "\n")
