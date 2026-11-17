# =============================================================================
# SUBGROUP ANALYSIS STEP 16: METFORMIN EFFECT BY EXERCISE FREQUENCY (BINARY)
# =============================================================================
# This script performs subgroup analysis by running IPW and G-computation
# for Metformin separately within each binary exercise frequency group.
#
# Purpose:
# - Assess whether Metformin treatment effects differ by exercise frequency
# - Provides exercise frequency-stratified estimates for Metformin
#
# Subgroups (binary, based on Exerc_5x_or_More):
# - Exercises <5x per week (FALSE): Never, 1-2, or 3-4 times per week
# - Exercises 5+ times per week (TRUE): 5-7 or 8+ times per week
#
# Output:
# - IPW_subgroup_exercise_freq_df: Combined IPW results for both subgroups
# - Standardization_subgroup_exercise_freq_df: Combined G-computation results
# - All saved to Output/ folder

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
cat("Loading required packages...\n")

required_packages <- c("here", "dplyr", "tidyr")

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

# -----------------------------------------------------------------------------
# Create Binary Exercise Variable
# -----------------------------------------------------------------------------
cat("\nCreating binary exercise variable (Exerc_5x_or_More)...\n")

data <- data |>
  mutate(
    Exerc_5x_or_More = case_when(
      Exercise.per.week %in% c("5-7 times per week", "8 or more times per week") ~ TRUE,
      Exercise.per.week %in% c("Never", "1-2 times per week", "3-4 times per week") ~ FALSE,
      TRUE ~ NA
    )
  )

cat("  Exerc_5x_or_More distribution:\n")
exerc_binary_table <- table(data$Exerc_5x_or_More, useNA = "ifany")
for (i in seq_along(exerc_binary_table)) {
  cat("    ", names(exerc_binary_table)[i], ":", exerc_binary_table[i], "\n")
}
cat("  Number of observations:", nrow(data), "\n")
cat("  Number of variables:", ncol(data), "\n")

# -----------------------------------------------------------------------------
# Define Analysis Parameters
# -----------------------------------------------------------------------------
cat("\nSetting analysis parameters...\n")

# Define treatment to analyze (Metformin only)
treatment_list <- c("Metformin")

# Define outcomes
outcome_list <- c("DunedinPACE")

# Define covariates for analysis
# NOTE: Exercise.per.week is EXCLUDED since we are stratifying by exercise frequency
covariates <- c(
  "Decimal.Chronological.Age",
  "Biological.Sex",
  "Level.of.Education",
  "Tobacco.Use.Binary",
  "Alcohol.per.week",
  "BMI",
  "Stress.Level",
  "Hours.of.sleep.per.night"
)

# Define binary exercise frequency subgroups based on Exerc_5x_or_More
# FALSE = <5x per week (Never, 1-2, 3-4 times per week)
# TRUE = 5+ times per week (5-7, 8+ times per week)
exercise_levels <- c(FALSE, TRUE)
exercise_level_labels <- c(
  "FALSE" = "Exercises <5x per week",
  "TRUE" = "Exercises 5+ times per week"
)

# Number of bootstrap iterations (set centrally in Master_parallel.R)
n_bootstrap <- getOption("n_bootstrap", default = 1000)

cat("  Treatment:", paste(treatment_list, collapse = ", "), "\n")
cat("  Outcomes:", paste(outcome_list, collapse = ", "), "\n")
cat("  Covariates:", length(covariates), "(excluding Exercise.per.week)\n")
cat("  Exercise frequency subgroups (binary):", length(exercise_levels), "\n")
cat("  Bootstrap iterations:", n_bootstrap, "\n")

# -----------------------------------------------------------------------------
# Verify Data Structure
# -----------------------------------------------------------------------------
cat("\nVerifying data structure...\n")

# Check that all specified variables exist in the data
all_vars <- c(treatment_list, outcome_list, covariates, "Exerc_5x_or_More")
missing_vars <- all_vars[!all_vars %in% names(data)]

if (length(missing_vars) > 0) {
  stop("The following variables are not found in the data:\n  ",
       paste(missing_vars, collapse = ", "),
       "\n\nPlease check variable names and try again.")
}

cat("  All variables found in data\n")

# Report on binary exercise frequency variable
cat("\nBinary exercise frequency variable summary:\n")
cat("  Variable: Exerc_5x_or_More\n")
cat("  Class:", class(data$Exerc_5x_or_More), "\n")
exercise_table <- table(data$Exerc_5x_or_More, useNA = "ifany")
for (i in seq_along(exercise_table)) {
  cat("    ", names(exercise_table)[i], ":", exercise_table[i], "\n")
}

# Check Metformin distribution
cat("\nMetformin variable summary:\n")
metformin_table <- table(data$Metformin, useNA = "ifany")
for (i in seq_along(metformin_table)) {
  cat("    ", names(metformin_table)[i], ":", metformin_table[i], "\n")
}

# -----------------------------------------------------------------------------
# Initialize Results Storage
# -----------------------------------------------------------------------------
IPW_results_list <- list()
Standardization_results_list <- list()

# =============================================================================
# LOOP THROUGH BINARY EXERCISE FREQUENCY SUBGROUPS
# =============================================================================

for (exercise_level in exercise_levels) {
  
  # Get label for this subgroup
  subgroup_label <- exercise_level_labels[as.character(exercise_level)]
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("SUBGROUP: ", subgroup_label, " (Exerc_5x_or_More = ", exercise_level, ")\n", sep = "")
  cat(strrep("=", 70), "\n")
  
  # Subset data for this binary exercise level
  data_subset <- data[data$Exerc_5x_or_More == exercise_level & !is.na(data$Exerc_5x_or_More), ]
  
  cat("  Subset size:", nrow(data_subset), "observations\n")
  
  # Check Metformin users in this subgroup
  metformin_counts <- table(data_subset$Metformin, useNA = "ifany")
  cat("  Metformin distribution in subgroup:\n")
  for (i in seq_along(metformin_counts)) {
    cat("    ", names(metformin_counts)[i], ":", metformin_counts[i], "\n")
  }
  
  # Check if we have enough data (at least some users and non-users)
  n_users <- sum(data_subset$Metformin == 1, na.rm = TRUE)
  n_nonusers <- sum(data_subset$Metformin == 0, na.rm = TRUE)
  
  if (n_users < 5 || n_nonusers < 5) {
    cat("  WARNING: Insufficient sample size (users:", n_users, 
        ", non-users:", n_nonusers, "). Skipping this subgroup.\n")
    next
  }
  
  # ---------------------------------------------------------------------------
  # IPW Analysis
  # ---------------------------------------------------------------------------
  cat("\n--- Running IPW analysis ---\n")
  
  tryCatch({
    ipw_result <- IPW(
      data = data_subset,
      treatment_list = treatment_list,
      outcome_list = outcome_list,
      covariates = covariates,
      n_bootstrap = n_bootstrap,
      strict_treatment = FALSE,
      filter_incomplete_covariates = TRUE,
      love_plot_dir = paste0("Output/love_plots/subgroup_exercise_freq_", 
                              ifelse(exercise_level, "5plus", "less5"))
    )
    
    # Add subgroup identifier using descriptive label
    ipw_result$subgroup <- subgroup_label
    
    # Store result
    IPW_results_list[[subgroup_label]] <- ipw_result
    
    cat("  IPW ATE:", round(ipw_result$ate_estimate, 4), "\n")
    cat("  IPW 95% CI: [", round(ipw_result$ci_lower, 4), ", ", 
        round(ipw_result$ci_upper, 4), "]\n")
    
  }, error = function(e) {
    cat("  ERROR in IPW analysis:", conditionMessage(e), "\n")
  })
  
  # ---------------------------------------------------------------------------
  # Standardization Analysis
  # ---------------------------------------------------------------------------
  cat("\n--- Running Standardization analysis ---\n")
  
  tryCatch({
    std_result <- Standardization(
      data = data_subset,
      treatment_list = treatment_list,
      outcome_list = outcome_list,
      covariates = covariates,
      n_bootstrap = n_bootstrap,
      strict_treatment = FALSE,
      filter_incomplete_covariates = TRUE
    )
    
    # Add subgroup identifier using descriptive label
    std_result$subgroup <- subgroup_label
    
    # Store result
    Standardization_results_list[[subgroup_label]] <- std_result
    
    cat("  G-comp ATE:", round(std_result$ate_estimate, 4), "\n")
    cat("  G-comp 95% CI: [", round(std_result$ci_lower, 4), ", ", 
        round(std_result$ci_upper, 4), "]\n")
    
  }, error = function(e) {
    cat("  ERROR in Standardization analysis:", conditionMessage(e), "\n")
  })
}

# -----------------------------------------------------------------------------
# Combine Results
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("COMBINING RESULTS\n")
cat(strrep("=", 70), "\n")

# Combine IPW results
if (length(IPW_results_list) > 0) {
  IPW_subgroup_exercise_freq_df <- bind_rows(IPW_results_list)
  cat("  Combined IPW results:", nrow(IPW_subgroup_exercise_freq_df), "rows\n")
} else {
  IPW_subgroup_exercise_freq_df <- data.frame()
  cat("  WARNING: No IPW results to combine\n")
}

# Combine Standardization results
if (length(Standardization_results_list) > 0) {
  Standardization_subgroup_exercise_freq_df <- bind_rows(Standardization_results_list)
  cat("  Combined Standardization results:", 
      nrow(Standardization_subgroup_exercise_freq_df), "rows\n")
} else {
  Standardization_subgroup_exercise_freq_df <- data.frame()
  cat("  WARNING: No Standardization results to combine\n")
}

# -----------------------------------------------------------------------------
# Display Results
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("FINAL RESULTS - IPW\n")
cat(strrep("=", 70), "\n")
print(IPW_subgroup_exercise_freq_df)

cat("\n")
cat(strrep("=", 70), "\n")
cat("FINAL RESULTS - STANDARDIZATION\n")
cat(strrep("=", 70), "\n")
print(Standardization_subgroup_exercise_freq_df)

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\nSaving results...\n")

# Save combined results
save(IPW_subgroup_exercise_freq_df, 
     file = here::here("Output", "IPW_subgroup_exercise_freq_df"))
cat("  IPW results saved to: Output/IPW_subgroup_exercise_freq_df\n")

save(Standardization_subgroup_exercise_freq_df, 
     file = here::here("Output", "Standardization_subgroup_exercise_freq_df"))
cat("  Standardization results saved to: Output/Standardization_subgroup_exercise_freq_df\n")

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("SUBGROUP ANALYSIS SUMMARY\n")
cat(strrep("=", 70), "\n")

if (nrow(IPW_subgroup_exercise_freq_df) > 0) {
  cat("\nIPW Results by Exercise Frequency:\n")
  for (i in 1:nrow(IPW_subgroup_exercise_freq_df)) {
    row <- IPW_subgroup_exercise_freq_df[i, ]
    sig_marker <- ""
    if ((row$ci_lower > 0 & row$ci_upper > 0) | (row$ci_lower < 0 & row$ci_upper < 0)) {
      sig_marker <- " *"
    }
    cat(sprintf("  %-25s: ATE = %7.4f, 95%% CI [%7.4f, %7.4f], n=%d/%d%s\n",
                row$subgroup, row$ate_estimate, row$ci_lower, row$ci_upper,
                row$n_treated, row$n_control, sig_marker))
  }
}

if (nrow(Standardization_subgroup_exercise_freq_df) > 0) {
  cat("\nG-computation Results by Exercise Frequency:\n")
  for (i in 1:nrow(Standardization_subgroup_exercise_freq_df)) {
    row <- Standardization_subgroup_exercise_freq_df[i, ]
    sig_marker <- ""
    if ((row$ci_lower > 0 & row$ci_upper > 0) | (row$ci_lower < 0 & row$ci_upper < 0)) {
      sig_marker <- " *"
    }
    cat(sprintf("  %-25s: ATE = %7.4f, 95%% CI [%7.4f, %7.4f], n=%d/%d%s\n",
                row$subgroup, row$ate_estimate, row$ci_lower, row$ci_upper,
                row$n_treated, row$n_control, sig_marker))
  }
}

cat("\n* indicates statistically significant effect (95% CI excludes 0)\n")

cat("\n")
cat(strrep("=", 70), "\n")
cat("SUBGROUP ANALYSIS STEP 16 COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 70), "\n")
cat("Analysis date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Stratification variable: Exerc_5x_or_More (binary)\n")
cat("  - FALSE: Exercises <5x per week (Never, 1-2, 3-4 times/week)\n")
cat("  - TRUE: Exercises 5+ times per week (5-7, 8+ times/week)\n")
cat("Treatment analyzed: Metformin\n")
cat(strrep("=", 70), "\n")
