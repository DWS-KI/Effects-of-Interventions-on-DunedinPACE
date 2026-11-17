# =============================================================================
# SENSITIVITY ANALYSIS STEP 22: IPW AND STANDARDIZATION WITH VITAMIN COVARIATE
# =============================================================================
# This script repeats the main analyses from steps 7 and 8, moving Vitamins
# from the exposure list to the covariates in both IPW and standardization.
#
# Purpose:
# - Assess robustness of main findings when adjusting for concurrent vitamin use
# - Preserve the main-analysis cohort, estimators, and missing-data requirements
#
# Output:
# - IPW_sensitivity_vitamins_df: IPW results with vitamin adjustment
# - Standardization_sensitivity_vitamins_df: G-computation results with vitamin adjustment
# - Both saved to Output/ folder

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
cat("Loading required packages...\n")

required_packages <- c("here", "dplyr")

for (pkg in required_packages) {
  if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# -----------------------------------------------------------------------------
# Source Analysis Functions
# -----------------------------------------------------------------------------
cat("Loading analysis functions...\n")
source(here::here("Program", "Functions", "IPW_functions.R"))
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

# Define treatments to analyze (same as steps 7 and 8, excluding Vitamins)
treatment_list <- c(
  "Metformin",
  "Antioxidants",
  "Fish.oil.omega",
  "Workout.supplements",
  "Herbal.supplements",
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

# Define outcomes
outcome_list <- c(
  "DunedinPACE"
)

# Define covariates INCLUDING vitamin use
# Same as main analysis + Vitamins
covariates <- c(
  "Decimal.Chronological.Age",
  "Biological.Sex",
  "Level.of.Education",
  "Tobacco.Use.Binary",
  "Alcohol.per.week",
  "Exercise.per.week",
  "BMI",
  "Stress.Level",
  "Hours.of.sleep.per.night",
  "Vitamins"  # Added for sensitivity analysis
)

# Number of bootstrap iterations (set centrally in Master_parallel.R)
n_bootstrap <- getOption("n_bootstrap", default = 1000)

cat("  Treatments:", length(treatment_list), "\n")
cat("  Outcomes:", paste(outcome_list, collapse = ", "), "\n")
cat("  Covariates:", length(covariates), "(including Vitamins)\n")
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

# =============================================================================
# PART 1: IPW ANALYSIS WITH VITAMIN COVARIATE
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PART 1: IPW ANALYSIS WITH VITAMIN COVARIATE\n")
cat(strrep("=", 70), "\n")

# Run IPW analysis with vitamin covariate
# Keep the default strict treatment and complete-covariate checks from the main analysis
IPW_sensitivity_vitamins_df <- IPW(
  data = data,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  love_plot_dir = "Output/love_plots/sensitivity_vitamins"
)

# Display IPW results
cat("\n")
cat(strrep("-", 70), "\n")
cat("IPW SENSITIVITY RESULTS (with vitamin adjustment)\n")
cat(strrep("-", 70), "\n")
print(IPW_sensitivity_vitamins_df)

# =============================================================================
# PART 2: STANDARDIZATION ANALYSIS WITH VITAMIN COVARIATE
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PART 2: STANDARDIZATION ANALYSIS WITH VITAMIN COVARIATE\n")
cat(strrep("=", 70), "\n")

# Run Standardization analysis with vitamin covariate
# Keep the default strict treatment and complete-covariate checks from the main analysis
Standardization_sensitivity_vitamins_df <- Standardization(
  data = data,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap
)

# Display Standardization results
cat("\n")
cat(strrep("-", 70), "\n")
cat("STANDARDIZATION SENSITIVITY RESULTS (with vitamin adjustment)\n")
cat(strrep("-", 70), "\n")
print(Standardization_sensitivity_vitamins_df)

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\nSaving results...\n")

# Save IPW sensitivity results
ipw_output_path <- here::here("Output", "IPW_sensitivity_vitamins_df")
save(IPW_sensitivity_vitamins_df, file = ipw_output_path)
cat("  IPW results saved to:", ipw_output_path, "\n")

# Save Standardization sensitivity results
std_output_path <- here::here("Output", "Standardization_sensitivity_vitamins_df")
save(Standardization_sensitivity_vitamins_df, file = std_output_path)
cat("  Standardization results saved to:", std_output_path, "\n")

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("SENSITIVITY ANALYSIS SUMMARY\n")
cat(strrep("=", 70), "\n")

cat("\nIPW Analysis:\n")
cat("  Treatment-outcome combinations analyzed:", nrow(IPW_sensitivity_vitamins_df), "\n")

# Check for significant IPW results
sig_ipw <- IPW_sensitivity_vitamins_df[
  (IPW_sensitivity_vitamins_df$ci_lower > 0 & IPW_sensitivity_vitamins_df$ci_upper > 0) |
  (IPW_sensitivity_vitamins_df$ci_lower < 0 & IPW_sensitivity_vitamins_df$ci_upper < 0),
]

if (nrow(sig_ipw) > 0) {
  cat("  Significant effects (95% CI excludes 0):", nrow(sig_ipw), "\n")
  for (i in 1:nrow(sig_ipw)) {
    cat("    -", sig_ipw$treatment[i], ": ATE =", round(sig_ipw$ate_estimate[i], 4),
        "[", round(sig_ipw$ci_lower[i], 4), ",", round(sig_ipw$ci_upper[i], 4), "]\n")
  }
} else {
  cat("  No significant effects detected\n")
}

cat("\nStandardization Analysis:\n")
cat("  Treatment-outcome combinations analyzed:", nrow(Standardization_sensitivity_vitamins_df), "\n")

# Check for significant Standardization results
sig_std <- Standardization_sensitivity_vitamins_df[
  (Standardization_sensitivity_vitamins_df$ci_lower > 0 & Standardization_sensitivity_vitamins_df$ci_upper > 0) |
  (Standardization_sensitivity_vitamins_df$ci_lower < 0 & Standardization_sensitivity_vitamins_df$ci_upper < 0),
]

if (nrow(sig_std) > 0) {
  cat("  Significant effects (95% CI excludes 0):", nrow(sig_std), "\n")
  for (i in 1:nrow(sig_std)) {
    cat("    -", sig_std$treatment[i], ": ATE =", round(sig_std$ate_estimate[i], 4),
        "[", round(sig_std$ci_lower[i], 4), ",", round(sig_std$ci_upper[i], 4), "]\n")
  }
} else {
  cat("  No significant effects detected\n")
}

cat("\n")
cat(strrep("=", 70), "\n")
cat("SENSITIVITY ANALYSIS STEP 22 COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 70), "\n")
cat("Analysis date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Additional covariate: Vitamins\n")
cat(strrep("=", 70), "\n")
