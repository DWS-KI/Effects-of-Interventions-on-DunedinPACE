# =============================================================================
# SUBGROUP ANALYSIS STEP 13: IPW AND STANDARDIZATION STRATIFIED BY SEX
# =============================================================================
# This script performs subgroup analysis by running IPW and G-computation
# separately for men and women.
#
# Purpose:
# - Assess whether treatment effects differ between men and women
# - Provides sex-stratified estimates for all treatments
#
# Output:
# - IPW_subgroup_male_df: IPW results for men only
# - IPW_subgroup_female_df: IPW results for women only
# - Standardization_subgroup_male_df: G-computation results for men only
# - Standardization_subgroup_female_df: G-computation results for women only
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
# NOTE: Biological.Sex is EXCLUDED since we are stratifying by sex
covariates <- c(
  "Decimal.Chronological.Age",
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
cat("  Covariates:", length(covariates), "(excluding Biological.Sex)\n")
cat("  Bootstrap iterations:", n_bootstrap, "\n")

# -----------------------------------------------------------------------------
# Verify Data Structure
# -----------------------------------------------------------------------------
cat("\nVerifying data structure...\n")

# Check that all specified variables exist in the data
all_vars <- c(treatment_list, outcome_list, covariates, "Biological.Sex")
missing_vars <- all_vars[!all_vars %in% names(data)]

if (length(missing_vars) > 0) {
  stop("The following variables are not found in the data:\n  ",
       paste(missing_vars, collapse = ", "),
       "\n\nPlease check variable names and try again.")
}

cat("  All variables found in data\n")

# Report on sex variable
cat("\nBiological sex variable summary:\n")
cat("  Variable: Biological.Sex\n")
cat("  Class:", class(data$Biological.Sex), "\n")
sex_table <- table(data$Biological.Sex, useNA = "ifany")
for (i in seq_along(sex_table)) {
  cat("    ", names(sex_table)[i], ":", sex_table[i], "\n")
}

# -----------------------------------------------------------------------------
# Split Data by Sex
# -----------------------------------------------------------------------------
cat("\nSplitting data by biological sex...\n")

data_male <- data[data$Biological.Sex == "Male", ]
data_female <- data[data$Biological.Sex == "Female", ]

cat("  Men subset:", nrow(data_male), "observations\n")
cat("  Women subset:", nrow(data_female), "observations\n")

# =============================================================================
# PART 1: MEN SUBGROUP ANALYSIS
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PART 1: MEN SUBGROUP ANALYSIS\n")
cat(strrep("=", 70), "\n")

# -----------------------------------------------------------------------------
# IPW Analysis - Men
# -----------------------------------------------------------------------------
cat("\n--- Running IPW analysis for men ---\n")

IPW_subgroup_male_df <- IPW(
  data = data_male,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE,
  love_plot_dir = "Output/love_plots/subgroup_male"
)

# Add subgroup identifier
IPW_subgroup_male_df$subgroup <- "Man"

cat("\n")
cat(strrep("-", 70), "\n")
cat("IPW RESULTS - MEN\n")
cat(strrep("-", 70), "\n")
print(IPW_subgroup_male_df)

# -----------------------------------------------------------------------------
# Standardization Analysis - Men
# -----------------------------------------------------------------------------
cat("\n--- Running Standardization analysis for men ---\n")

Standardization_subgroup_male_df <- Standardization(
  data = data_male,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE
)

# Add subgroup identifier
Standardization_subgroup_male_df$subgroup <- "Man"

cat("\n")
cat(strrep("-", 70), "\n")
cat("STANDARDIZATION RESULTS - MEN\n")
cat(strrep("-", 70), "\n")
print(Standardization_subgroup_male_df)

# =============================================================================
# PART 2: WOMEN SUBGROUP ANALYSIS
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("PART 2: WOMEN SUBGROUP ANALYSIS\n")
cat(strrep("=", 70), "\n")

# -----------------------------------------------------------------------------
# IPW Analysis - Women
# -----------------------------------------------------------------------------
cat("\n--- Running IPW analysis for women ---\n")

IPW_subgroup_female_df <- IPW(
  data = data_female,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE,
  love_plot_dir = "Output/love_plots/subgroup_female"
)

# Add subgroup identifier
IPW_subgroup_female_df$subgroup <- "Woman"

cat("\n")
cat(strrep("-", 70), "\n")
cat("IPW RESULTS - WOMEN\n")
cat(strrep("-", 70), "\n")
print(IPW_subgroup_female_df)

# -----------------------------------------------------------------------------
# Standardization Analysis - Women
# -----------------------------------------------------------------------------
cat("\n--- Running Standardization analysis for women ---\n")

Standardization_subgroup_female_df <- Standardization(
  data = data_female,
  treatment_list = treatment_list,
  outcome_list = outcome_list,
  covariates = covariates,
  n_bootstrap = n_bootstrap,
  strict_treatment = FALSE,
  filter_incomplete_covariates = TRUE
)

# Add subgroup identifier
Standardization_subgroup_female_df$subgroup <- "Woman"

cat("\n")
cat(strrep("-", 70), "\n")
cat("STANDARDIZATION RESULTS - WOMEN\n")
cat(strrep("-", 70), "\n")
print(Standardization_subgroup_female_df)

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\nSaving results...\n")

# Save Men results
save(IPW_subgroup_male_df, 
     file = here::here("Output", "IPW_subgroup_male_df"))
cat("  IPW men results saved\n")

save(Standardization_subgroup_male_df, 
     file = here::here("Output", "Standardization_subgroup_male_df"))
cat("  Standardization men results saved\n")

# Save Women results
save(IPW_subgroup_female_df, 
     file = here::here("Output", "IPW_subgroup_female_df"))
cat("  IPW women results saved\n")

save(Standardization_subgroup_female_df, 
     file = here::here("Output", "Standardization_subgroup_female_df"))
cat("  Standardization women results saved\n")

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("SUBGROUP ANALYSIS SUMMARY\n")
cat(strrep("=", 70), "\n")

cat("\nMen Subgroup:\n")
cat("  Sample size:", nrow(data_male), "\n")
cat("  Treatment-outcome combinations analyzed:", nrow(IPW_subgroup_male_df), "\n")

# Check for significant IPW results - Men
sig_ipw_male <- IPW_subgroup_male_df[
  (IPW_subgroup_male_df$ci_lower > 0 & IPW_subgroup_male_df$ci_upper > 0) |
  (IPW_subgroup_male_df$ci_lower < 0 & IPW_subgroup_male_df$ci_upper < 0),
]
cat("  Significant IPW effects:", nrow(sig_ipw_male), "\n")

# Check for significant Standardization results - Men
sig_std_male <- Standardization_subgroup_male_df[
  (Standardization_subgroup_male_df$ci_lower > 0 & Standardization_subgroup_male_df$ci_upper > 0) |
  (Standardization_subgroup_male_df$ci_lower < 0 & Standardization_subgroup_male_df$ci_upper < 0),
]
cat("  Significant G-computation effects:", nrow(sig_std_male), "\n")

cat("\nWomen Subgroup:\n")
cat("  Sample size:", nrow(data_female), "\n")
cat("  Treatment-outcome combinations analyzed:", nrow(IPW_subgroup_female_df), "\n")

# Check for significant IPW results - Women
sig_ipw_female <- IPW_subgroup_female_df[
  (IPW_subgroup_female_df$ci_lower > 0 & IPW_subgroup_female_df$ci_upper > 0) |
  (IPW_subgroup_female_df$ci_lower < 0 & IPW_subgroup_female_df$ci_upper < 0),
]
cat("  Significant IPW effects:", nrow(sig_ipw_female), "\n")

# Check for significant Standardization results - Women
sig_std_female <- Standardization_subgroup_female_df[
  (Standardization_subgroup_female_df$ci_lower > 0 & Standardization_subgroup_female_df$ci_upper > 0) |
  (Standardization_subgroup_female_df$ci_lower < 0 & Standardization_subgroup_female_df$ci_upper < 0),
]
cat("  Significant G-computation effects:", nrow(sig_std_female), "\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("SUBGROUP ANALYSIS STEP 13 COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 70), "\n")
cat("Analysis date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Stratification variable: Biological.Sex\n")
cat(strrep("=", 70), "\n")
