# =============================================================================
# SUBGROUP ANALYSIS STEP 15: IPW AND STANDARDIZATION STRATIFIED BY ETHNICITY
# =============================================================================
# This script performs subgroup analysis by running IPW and G-computation
# separately for European and Non-European individuals.
#
# Purpose:
# - Assess whether treatment effects differ between European and Non-European groups
# - Provides ethnicity-stratified estimates for all treatments
#
# Output:
# - IPW_subgroup_european_df: IPW results for Europeans only
# - IPW_subgroup_noneuropean_df: IPW results for Non-Europeans only
# - Standardization_subgroup_european_df: G-computation results for Europeans only
# - Standardization_subgroup_noneuropean_df: G-computation results for Non-Europeans only
# - All saved to Output/ folder

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
# NOTE: European/Ethnicity is EXCLUDED since we are stratifying by ethnicity
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

cat("  Treatments:", length(treatment_list), "\n")
cat("  Outcomes:", paste(outcome_list, collapse = ", "), "\n")
cat("  Covariates:", length(covariates), "(excluding European/Ethnicity)\n")
cat("  Bootstrap iterations:", n_bootstrap, "\n")

# -----------------------------------------------------------------------------
# Verify Data Structure
# -----------------------------------------------------------------------------
cat("\nVerifying data structure...\n")

# Check that all specified variables exist in the data
all_vars <- c(treatment_list, outcome_list, covariates, "European")
missing_vars <- all_vars[!all_vars %in% names(data)]

if (length(missing_vars) > 0) {
  stop("The following variables are not found in the data:\n  ",
       paste(missing_vars, collapse = ", "),
       "\n\nPlease check variable names and try again.")
}

cat("  All variables found in data\n")

# Report on European variable
cat("\nEuropean ethnicity variable summary:\n")
cat("  Variable: European\n")
cat("  Class:", class(data$European), "\n")
european_table <- table(data$European, useNA = "ifany")
for (i in seq_along(european_table)) {
  cat("    European =", names(european_table)[i], ":", european_table[i], "\n")
}

n_missing_ethnicity <- sum(is.na(data$European))
cat("  Excluded from ethnicity subgroup analyses only:", n_missing_ethnicity,
    "observations with missing ethnicity\n")

invalid_ethnicity_values <- unique(data$European[!is.na(data$European) & !data$European %in% c(0, 1)])

if (length(invalid_ethnicity_values) > 0) {
  stop(
    "Unexpected values in ethnicity stratification variable 'European': ",
    paste(invalid_ethnicity_values, collapse = ", "),
    ". Expected only 0 = Non-European and 1 = European."
  )
}

# -----------------------------------------------------------------------------
# Split Data by Ethnicity
# -----------------------------------------------------------------------------
cat("\nSplitting data by European ethnicity...\n")

# %in% excludes missing ethnicity without introducing NA rows into either subset.
# Keep the main analysis data unchanged.
data_european <- data[data$European %in% 1, ]
data_noneuropean <- data[data$European %in% 0, ]

cat("  European subset (European = 1):", nrow(data_european), "observations\n")
cat("  Non-European subset (European = 0):", nrow(data_noneuropean), "observations\n")

# =============================================================================
# PART 1: EUROPEAN SUBGROUP ANALYSIS
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PART 1: EUROPEAN SUBGROUP ANALYSIS\n")
cat(strrep("=", 70), "\n")

# -----------------------------------------------------------------------------
# IPW Analysis - Europeans
# -----------------------------------------------------------------------------
cat("\n--- Running IPW analysis for Europeans ---\n")

IPW_subgroup_european_df <- IPW(
  data = data_european,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE,
  love_plot_dir = "Output/love_plots/subgroup_european"
)

# Add subgroup identifier
IPW_subgroup_european_df$subgroup <- "European"

cat("\n")
cat(strrep("-", 70), "\n")
cat("IPW RESULTS - EUROPEANS\n")
cat(strrep("-", 70), "\n")
print(IPW_subgroup_european_df)

# -----------------------------------------------------------------------------
# Standardization Analysis - Europeans
# -----------------------------------------------------------------------------
cat("\n--- Running Standardization analysis for Europeans ---\n")

Standardization_subgroup_european_df <- Standardization(
  data = data_european,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE
)

# Add subgroup identifier
Standardization_subgroup_european_df$subgroup <- "European"

cat("\n")
cat(strrep("-", 70), "\n")
cat("STANDARDIZATION RESULTS - EUROPEANS\n")
cat(strrep("-", 70), "\n")
print(Standardization_subgroup_european_df)

# =============================================================================
# PART 2: NON-EUROPEAN SUBGROUP ANALYSIS
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PART 2: NON-EUROPEAN SUBGROUP ANALYSIS\n")
cat(strrep("=", 70), "\n")

# -----------------------------------------------------------------------------
# IPW Analysis - Non-Europeans
# -----------------------------------------------------------------------------
cat("\n--- Running IPW analysis for Non-Europeans ---\n")

IPW_subgroup_noneuropean_df <- IPW(
  data = data_noneuropean,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE,
  love_plot_dir = "Output/love_plots/subgroup_noneuropean"
)

# Add subgroup identifier
IPW_subgroup_noneuropean_df$subgroup <- "Non-European"

cat("\n")
cat(strrep("-", 70), "\n")
cat("IPW RESULTS - NON-EUROPEANS\n")
cat(strrep("-", 70), "\n")
print(IPW_subgroup_noneuropean_df)

# -----------------------------------------------------------------------------
# Standardization Analysis - Non-Europeans
# -----------------------------------------------------------------------------
cat("\n--- Running Standardization analysis for Non-Europeans ---\n")

Standardization_subgroup_noneuropean_df <- Standardization(
  data = data_noneuropean,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE
)

# Add subgroup identifier
Standardization_subgroup_noneuropean_df$subgroup <- "Non-European"

cat("\n")
cat(strrep("-", 70), "\n")
cat("STANDARDIZATION RESULTS - NON-EUROPEANS\n")
cat(strrep("-", 70), "\n")
print(Standardization_subgroup_noneuropean_df)

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\nSaving results...\n")

# Save European results
save(IPW_subgroup_european_df, 
     file = here::here("Output", "IPW_subgroup_european_df"))
cat("  IPW European results saved\n")

save(Standardization_subgroup_european_df, 
     file = here::here("Output", "Standardization_subgroup_european_df"))
cat("  Standardization European results saved\n")

# Save Non-European results
save(IPW_subgroup_noneuropean_df, 
     file = here::here("Output", "IPW_subgroup_noneuropean_df"))
cat("  IPW Non-European results saved\n")

save(Standardization_subgroup_noneuropean_df, 
     file = here::here("Output", "Standardization_subgroup_noneuropean_df"))
cat("  Standardization Non-European results saved\n")

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("SUBGROUP ANALYSIS SUMMARY\n")
cat(strrep("=", 70), "\n")

cat("\nEuropean Subgroup:\n")
cat("  Sample size:", nrow(data_european), "\n")
cat("  Treatment-outcome combinations analyzed:", nrow(IPW_subgroup_european_df), "\n")

# Check for significant IPW results - Europeans
sig_ipw_european <- IPW_subgroup_european_df[
  (IPW_subgroup_european_df$ci_lower > 0 & IPW_subgroup_european_df$ci_upper > 0) |
  (IPW_subgroup_european_df$ci_lower < 0 & IPW_subgroup_european_df$ci_upper < 0),
]
cat("  Significant IPW effects:", nrow(sig_ipw_european), "\n")

# Check for significant Standardization results - Europeans
sig_std_european <- Standardization_subgroup_european_df[
  (Standardization_subgroup_european_df$ci_lower > 0 & Standardization_subgroup_european_df$ci_upper > 0) |
  (Standardization_subgroup_european_df$ci_lower < 0 & Standardization_subgroup_european_df$ci_upper < 0),
]
cat("  Significant G-computation effects:", nrow(sig_std_european), "\n")

cat("\nNon-European Subgroup:\n")
cat("  Sample size:", nrow(data_noneuropean), "\n")
cat("  Treatment-outcome combinations analyzed:", nrow(IPW_subgroup_noneuropean_df), "\n")

# Check for significant IPW results - Non-Europeans
sig_ipw_noneuropean <- IPW_subgroup_noneuropean_df[
  (IPW_subgroup_noneuropean_df$ci_lower > 0 & IPW_subgroup_noneuropean_df$ci_upper > 0) |
  (IPW_subgroup_noneuropean_df$ci_lower < 0 & IPW_subgroup_noneuropean_df$ci_upper < 0),
]
cat("  Significant IPW effects:", nrow(sig_ipw_noneuropean), "\n")

# Check for significant Standardization results - Non-Europeans
sig_std_noneuropean <- Standardization_subgroup_noneuropean_df[
  (Standardization_subgroup_noneuropean_df$ci_lower > 0 & Standardization_subgroup_noneuropean_df$ci_upper > 0) |
  (Standardization_subgroup_noneuropean_df$ci_lower < 0 & Standardization_subgroup_noneuropean_df$ci_upper < 0),
]
cat("  Significant G-computation effects:", nrow(sig_std_noneuropean), "\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("SUBGROUP ANALYSIS STEP 15 COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 70), "\n")
cat("Analysis date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Stratification variable: European (1 = European, 0 = Non-European)\n")
cat(strrep("=", 70), "\n")
