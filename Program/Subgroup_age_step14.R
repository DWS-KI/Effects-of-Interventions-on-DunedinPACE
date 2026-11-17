# =============================================================================
# SUBGROUP ANALYSIS STEP 14: CREATE AGE STRATIFICATION VARIABLE
# =============================================================================
# This script creates a binary variable OverMedianAge for age-stratified analyses.
#
# Purpose:
# - Calculate median chronological age from the study population
# - Create binary indicator: 1 if above median, 0 if below median
#
# Output:
# - data_after_step13: Dataset with OverMedianAge column added
# - Saved to Output/ folder

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
# Load Data
# -----------------------------------------------------------------------------
cat("Loading data from Step 4...\n")
load(here::here("Output", "data_after_step4"))

data <- data_after_step4

cat("  Data loaded successfully\n")
cat("  Number of observations:", nrow(data), "\n")
cat("  Number of variables:", ncol(data), "\n")

# -----------------------------------------------------------------------------
# Calculate Median Age and Create Binary Variable
# -----------------------------------------------------------------------------
cat("\nCalculating median chronological age...\n")

# Calculate median age
median_age <- median(data$Decimal.Chronological.Age, na.rm = TRUE)

cat("  Median chronological age:", round(median_age, 2), "years\n")

# Create binary variable: 1 if above median, 0 if below
data <- data |>
  mutate(
    OverMedianAge = case_when(
      is.na(Decimal.Chronological.Age) ~ NA_integer_,
      Decimal.Chronological.Age >= median_age ~ 1L,
      Decimal.Chronological.Age < median_age ~ 0L
    )
  )

# -----------------------------------------------------------------------------
# Verify the New Variable
# -----------------------------------------------------------------------------
cat("\n=== OverMedianAge Distribution ===\n")
age_table <- table(data$OverMedianAge, useNA = "ifany")
print(age_table)

cat("\nPercentages:\n")
print(round(prop.table(age_table) * 100, 2))

cat("\nAge summary by group:\n")
cat("  Below median (OverMedianAge = 0):\n")
below_median <- data$Decimal.Chronological.Age[data$OverMedianAge == 0]
cat("    N:", length(below_median), "\n")
cat("    Mean age:", round(mean(below_median, na.rm = TRUE), 2), "\n")
cat("    Range:", round(min(below_median, na.rm = TRUE), 2), "-", 
    round(max(below_median, na.rm = TRUE), 2), "\n")

cat("\n  Above/equal median (OverMedianAge = 1):\n")
above_median <- data$Decimal.Chronological.Age[data$OverMedianAge == 1]
cat("    N:", length(above_median), "\n")
cat("    Mean age:", round(mean(above_median, na.rm = TRUE), 2), "\n")
cat("    Range:", round(min(above_median, na.rm = TRUE), 2), "-", 
    round(max(above_median, na.rm = TRUE), 2), "\n")

# -----------------------------------------------------------------------------
# Source Analysis Functions
# -----------------------------------------------------------------------------
cat("\nLoading analysis functions...\n")
source(here::here("Program", "Functions", "IPW_functions.R"))
source(here::here("Program", "Functions", "Standardization_functions.R"))

# -----------------------------------------------------------------------------
# Define Analysis Parameters
# -----------------------------------------------------------------------------
cat("\nSetting analysis parameters...\n")

# Define treatments to analyze (same as Steps 6 & 7)
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

# Define outcomes
outcome_list <- c(
  "DunedinPACE"
)

# Define covariates for analysis
# NOTE: Decimal.Chronological.Age is EXCLUDED since we are stratifying by age
# NOTE: Biological.Sex is INCLUDED for adjustment
covariates <- c(
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

cat("  Treatments:", length(treatment_list), "\n")
cat("  Outcomes:", paste(outcome_list, collapse = ", "), "\n")
cat("  Covariates:", length(covariates), "(excluding Decimal.Chronological.Age)\n")
cat("  Bootstrap iterations:", n_bootstrap, "\n")

# -----------------------------------------------------------------------------
# Verify Data Structure
# -----------------------------------------------------------------------------
cat("\nVerifying data structure...\n")

# Check that all specified variables exist in the data
all_vars <- c(treatment_list, outcome_list, covariates, "OverMedianAge")
missing_vars <- all_vars[!all_vars %in% names(data)]

if (length(missing_vars) > 0) {
  stop("The following variables are not found in the data:\n  ",
       paste(missing_vars, collapse = ", "),
       "\n\nPlease check variable names and try again.")
}

cat("  All variables found in data\n")

# -----------------------------------------------------------------------------
# Split Data by Age Group
# -----------------------------------------------------------------------------
cat("\nSplitting data by median age...\n")

data_younger <- data[data$OverMedianAge == 0, ]
data_older <- data[data$OverMedianAge == 1, ]

cat("  Younger subset (below median):", nrow(data_younger), "observations\n")
cat("  Older subset (at/above median):", nrow(data_older), "observations\n")

# =============================================================================
# PART 1: YOUNGER SUBGROUP ANALYSIS
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PART 1: YOUNGER SUBGROUP ANALYSIS (Below Median Age)\n")
cat(strrep("=", 70), "\n")

# -----------------------------------------------------------------------------
# IPW Analysis - Younger
# -----------------------------------------------------------------------------
cat("\n--- Running IPW analysis for younger subgroup ---\n")

IPW_subgroup_younger_df <- IPW(
  data = data_younger,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE,
  love_plot_dir = "Output/love_plots/subgroup_younger"
)

# Add subgroup identifier
IPW_subgroup_younger_df$subgroup <- "Younger"

cat("\n")
cat(strrep("-", 70), "\n")
cat("IPW RESULTS - YOUNGER\n")
cat(strrep("-", 70), "\n")
print(IPW_subgroup_younger_df)

# -----------------------------------------------------------------------------
# Standardization Analysis - Younger
# -----------------------------------------------------------------------------
cat("\n--- Running Standardization analysis for younger subgroup ---\n")

Standardization_subgroup_younger_df <- Standardization(
  data = data_younger,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE
)

# Add subgroup identifier
Standardization_subgroup_younger_df$subgroup <- "Younger"

cat("\n")
cat(strrep("-", 70), "\n")
cat("STANDARDIZATION RESULTS - YOUNGER\n")
cat(strrep("-", 70), "\n")
print(Standardization_subgroup_younger_df)

# =============================================================================
# PART 2: OLDER SUBGROUP ANALYSIS
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PART 2: OLDER SUBGROUP ANALYSIS (At/Above Median Age)\n")
cat(strrep("=", 70), "\n")

# -----------------------------------------------------------------------------
# IPW Analysis - Older
# -----------------------------------------------------------------------------
cat("\n--- Running IPW analysis for older subgroup ---\n")

IPW_subgroup_older_df <- IPW(
  data = data_older,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE,
  love_plot_dir = "Output/love_plots/subgroup_older"
)

# Add subgroup identifier
IPW_subgroup_older_df$subgroup <- "Older"

cat("\n")
cat(strrep("-", 70), "\n")
cat("IPW RESULTS - OLDER\n")
cat(strrep("-", 70), "\n")
print(IPW_subgroup_older_df)

# -----------------------------------------------------------------------------
# Standardization Analysis - Older
# -----------------------------------------------------------------------------
cat("\n--- Running Standardization analysis for older subgroup ---\n")

Standardization_subgroup_older_df <- Standardization(
  data = data_older,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE
)

# Add subgroup identifier
Standardization_subgroup_older_df$subgroup <- "Older"

cat("\n")
cat(strrep("-", 70), "\n")
cat("STANDARDIZATION RESULTS - OLDER\n")
cat(strrep("-", 70), "\n")
print(Standardization_subgroup_older_df)

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\nSaving results...\n")

# Save median age value for manuscript reference
save(median_age, file = here::here("Output", "median_age_value"))
cat("  Median age value saved\n")

# Save Younger results
save(IPW_subgroup_younger_df, 
     file = here::here("Output", "IPW_subgroup_younger_df"))
cat("  IPW younger results saved\n")

save(Standardization_subgroup_younger_df, 
     file = here::here("Output", "Standardization_subgroup_younger_df"))
cat("  Standardization younger results saved\n")

# Save Older results
save(IPW_subgroup_older_df, 
     file = here::here("Output", "IPW_subgroup_older_df"))
cat("  IPW older results saved\n")

save(Standardization_subgroup_older_df, 
     file = here::here("Output", "Standardization_subgroup_older_df"))
cat("  Standardization older results saved\n")

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("SUBGROUP ANALYSIS SUMMARY\n")
cat(strrep("=", 70), "\n")

cat("\nMedian chronological age:", round(median_age, 2), "years\n")

cat("\nYounger Subgroup (Below Median):\n")
cat("  Sample size:", nrow(data_younger), "\n")
cat("  Treatment-outcome combinations analyzed:", nrow(IPW_subgroup_younger_df), "\n")

# Check for significant IPW results - Younger
sig_ipw_younger <- IPW_subgroup_younger_df[
  (IPW_subgroup_younger_df$ci_lower > 0 & IPW_subgroup_younger_df$ci_upper > 0) |
  (IPW_subgroup_younger_df$ci_lower < 0 & IPW_subgroup_younger_df$ci_upper < 0),
]
cat("  Significant IPW effects:", nrow(sig_ipw_younger), "\n")

# Check for significant Standardization results - Younger
sig_std_younger <- Standardization_subgroup_younger_df[
  (Standardization_subgroup_younger_df$ci_lower > 0 & Standardization_subgroup_younger_df$ci_upper > 0) |
  (Standardization_subgroup_younger_df$ci_lower < 0 & Standardization_subgroup_younger_df$ci_upper < 0),
]
cat("  Significant G-computation effects:", nrow(sig_std_younger), "\n")

cat("\nOlder Subgroup (At/Above Median):\n")
cat("  Sample size:", nrow(data_older), "\n")
cat("  Treatment-outcome combinations analyzed:", nrow(IPW_subgroup_older_df), "\n")

# Check for significant IPW results - Older
sig_ipw_older <- IPW_subgroup_older_df[
  (IPW_subgroup_older_df$ci_lower > 0 & IPW_subgroup_older_df$ci_upper > 0) |
  (IPW_subgroup_older_df$ci_lower < 0 & IPW_subgroup_older_df$ci_upper < 0),
]
cat("  Significant IPW effects:", nrow(sig_ipw_older), "\n")

# Check for significant Standardization results - Older
sig_std_older <- Standardization_subgroup_older_df[
  (Standardization_subgroup_older_df$ci_lower > 0 & Standardization_subgroup_older_df$ci_upper > 0) |
  (Standardization_subgroup_older_df$ci_lower < 0 & Standardization_subgroup_older_df$ci_upper < 0),
]
cat("  Significant G-computation effects:", nrow(sig_std_older), "\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("SUBGROUP ANALYSIS STEP 14 COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 70), "\n")
cat("Analysis date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Stratification variable: OverMedianAge (Median =", round(median_age, 2), "years)\n")
cat(strrep("=", 70), "\n")
