# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
library(here)
library(dplyr)
library(tibble)

# Source helper functions
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
# Create Exerc_more_than_4x Variable
# -----------------------------------------------------------------------------
cat("\nCreating Exerc_more_than_4x variable...\n")

# Exerc_more_than_4x: 1 = exercises 5+ times/week, 0 = exercises 0-4 times/week
data <- data |>
  mutate(
    Exerc_more_than_4x = case_when(
      is.na(Exercise.per.week) ~ NA_integer_,
      Exercise.per.week %in% c("Never", "1-2 times per week", "3-4 times per week") ~ 0L,
      Exercise.per.week %in% c("5-7 times per week", "8 or more times per week") ~ 1L,
      TRUE ~ NA_integer_
    )
  )

cat("  Exerc_more_than_4x variable distribution:\n")
cat("  (1 = 5+ times/week, 0 = 0-4 times/week)\n")
exercising_table <- table(data$Exerc_more_than_4x, useNA = "ifany")
for (i in seq_along(exercising_table)) {
  cat("    ", names(exercising_table)[i], ":", exercising_table[i], "\n")
}

# -----------------------------------------------------------------------------
# HELPER FUNCTIONS
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Define Analysis Parameters
# -----------------------------------------------------------------------------
cat("\nSetting analysis parameters...\n")

# Treatment: Exerc_more_than_4x (binary: 1 = 5+ times/week, 0 = 0-4 times/week)
treatment_list <- c("Exerc_more_than_4x")

# Outcome: DunedinPACE
outcome_list <- c("DunedinPACE")

# Define covariates for analysis
# NOTE: Exercise.per.week is EXCLUDED since Exerc_more_than_4x is derived from it
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

# Define Metformin subgroups
metformin_levels <- c(0, 1)
metformin_level_labels <- c(
  "0" = "Metformin non-users",
  "1" = "Metformin users"
)

# Number of bootstrap iterations (set centrally in Master_parallel.R)
n_bootstrap <- getOption("n_bootstrap", default = 1000)

cat("  Treatment:", paste(treatment_list, collapse = ", "), "\n")
cat("  Outcomes:", paste(outcome_list, collapse = ", "), "\n")
cat("  Covariates:", length(covariates), "(excluding Exercise.per.week)\n")
cat("  Metformin subgroups:", length(metformin_levels), "\n")
cat("  Bootstrap iterations:", n_bootstrap, "\n")

# -----------------------------------------------------------------------------
# Verify Data Structure
# -----------------------------------------------------------------------------
cat("\nVerifying data structure...\n")

# Check that all specified variables exist in the data
all_vars <- c(treatment_list, outcome_list, covariates, "Metformin")
missing_vars <- all_vars[!all_vars %in% names(data)]

if (length(missing_vars) > 0) {
  stop("The following variables are not found in the data:\n  ",
       paste(missing_vars, collapse = ", "),
       "\n\nPlease check variable names and try again.")
}

cat("  All variables found in data\n")

# Report on Metformin variable
cat("\nMetformin variable summary:\n")
cat("  Variable: Metformin\n")
cat("  Class:", class(data$Metformin), "\n")
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
# LOOP THROUGH METFORMIN SUBGROUPS
# =============================================================================

for (metformin_status in metformin_levels) {
  
  # Get label for this subgroup
  subgroup_label <- metformin_level_labels[as.character(metformin_status)]
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("SUBGROUP: ", subgroup_label, " (Metformin = ", metformin_status, ")\n", sep = "")
  cat(strrep("=", 70), "\n")
  
  # Subset data for this Metformin status
  data_subset <- data[data$Metformin == metformin_status & !is.na(data$Metformin), ]
  
  cat("  Subset size:", nrow(data_subset), "observations\n")
  
  # Check Exerc_more_than_4x distribution in this subgroup
  exercising_counts <- table(data_subset$Exerc_more_than_4x, useNA = "ifany")
  cat("  Exerc_more_than_4x distribution in subgroup:\n")
  cat("  (1 = 5+ times/week, 0 = 0-4 times/week)\n")
  for (i in seq_along(exercising_counts)) {
    cat("    ", names(exercising_counts)[i], ":", exercising_counts[i], "\n")
  }
  
  # Check if we have enough data (at least some in each group)
  n_exercisers <- sum(data_subset$Exerc_more_than_4x == 1, na.rm = TRUE)
  n_nonexercisers <- sum(data_subset$Exerc_more_than_4x == 0, na.rm = TRUE)
  
  if (n_exercisers < 5 || n_nonexercisers < 5) {
    cat("  WARNING: Insufficient sample size (exercisers:", n_exercisers, 
        ", non-exercisers:", n_nonexercisers, "). Skipping this subgroup.\n")
    next
  }
  
  # Define love plot directory based on metformin status
  if (metformin_status == 1) {
    love_plot_dir <- "Output/love_plots/exercise_in_metformin_users"
  } else {
    love_plot_dir <- "Output/love_plots/exercise_in_metformin_nonusers"
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
      love_plot_dir = love_plot_dir
    )
    
    # Add subgroup identifier
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
    
    # Add subgroup identifier
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
  IPW_exercise_by_metformin_df <- bind_rows(IPW_results_list)
  cat("  Combined IPW results:", nrow(IPW_exercise_by_metformin_df), "rows\n")
} else {
  IPW_exercise_by_metformin_df <- data.frame()
  cat("  WARNING: No IPW results to combine\n")
}

# Combine Standardization results
if (length(Standardization_results_list) > 0) {
  Standardization_exercise_by_metformin_df <- bind_rows(Standardization_results_list)
  cat("  Combined Standardization results:", 
      nrow(Standardization_exercise_by_metformin_df), "rows\n")
} else {
  Standardization_exercise_by_metformin_df <- data.frame()
  cat("  WARNING: No Standardization results to combine\n")
}

# -----------------------------------------------------------------------------
# Create Summary Table (S5-style format)
# -----------------------------------------------------------------------------
cat("\nCreating summary table...\n")

if (nrow(IPW_exercise_by_metformin_df) > 0 && 
    nrow(Standardization_exercise_by_metformin_df) > 0) {
  
  exercise_by_metformin_summary <- tibble(
    Analysis = IPW_exercise_by_metformin_df$subgroup,
    `ATE (IPW)` = sprintf("%.2f%%", IPW_exercise_by_metformin_df$ate_estimate * 100),
    `CI (IPW)` = sprintf("[%.2f%%, %.2f%%]", 
                          IPW_exercise_by_metformin_df$ci_lower * 100, 
                          IPW_exercise_by_metformin_df$ci_upper * 100),
    SMD = IPW_exercise_by_metformin_df$SMD,
    `ATE (G-comp)` = sprintf("%.2f%%", Standardization_exercise_by_metformin_df$ate_estimate * 100),
    `CI (G-comp)` = sprintf("[%.2f%%, %.2f%%]", 
                             Standardization_exercise_by_metformin_df$ci_lower * 100, 
                             Standardization_exercise_by_metformin_df$ci_upper * 100),
    `N 5+ times/wk` = IPW_exercise_by_metformin_df$n_treated,
    `N 0-4 times/wk` = IPW_exercise_by_metformin_df$n_control,
    ipw_ate = IPW_exercise_by_metformin_df$ate_estimate
  ) |>
    arrange(ipw_ate) |>
    select(-ipw_ate)
  
  cat("  Summary table created with", nrow(exercise_by_metformin_summary), "rows\n")
  
} else {
  exercise_by_metformin_summary <- tibble()
  cat("  WARNING: Could not create summary table\n")
}

# -----------------------------------------------------------------------------
# Display Results
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("FINAL RESULTS - IPW\n")
cat(strrep("=", 70), "\n")
print(IPW_exercise_by_metformin_df)

cat("\n")
cat(strrep("=", 70), "\n")
cat("FINAL RESULTS - STANDARDIZATION\n")
cat(strrep("=", 70), "\n")
print(Standardization_exercise_by_metformin_df)

cat("\n")
cat(strrep("=", 70), "\n")
cat("SUMMARY TABLE (S5-style format)\n")
cat(strrep("=", 70), "\n")
print(exercise_by_metformin_summary)

# -----------------------------------------------------------------------------
# Save Results
# -----------------------------------------------------------------------------
cat("\nSaving results...\n")

# Save combined results
save(IPW_exercise_by_metformin_df, 
     file = here::here("Output", "IPW_exercise_by_metformin_df"))
cat("  IPW results saved to: Output/IPW_exercise_by_metformin_df\n")

save(Standardization_exercise_by_metformin_df, 
     file = here::here("Output", "Standardization_exercise_by_metformin_df"))
cat("  Standardization results saved to: Output/Standardization_exercise_by_metformin_df\n")

save(exercise_by_metformin_summary, 
     file = here::here("Output", "exercise_by_metformin_summary"))
cat("  Summary table saved to: Output/exercise_by_metformin_summary\n")

# -----------------------------------------------------------------------------
# Summary Statistics
# -----------------------------------------------------------------------------
cat("\n")
cat(strrep("=", 70), "\n")
cat("EXERCISE EFFECT BY METFORMIN STATUS - SUMMARY\n")
cat(strrep("=", 70), "\n")

if (nrow(IPW_exercise_by_metformin_df) > 0) {
  cat("\nIPW Results by Metformin Status:\n")
  for (i in 1:nrow(IPW_exercise_by_metformin_df)) {
    row <- IPW_exercise_by_metformin_df[i, ]
    sig_marker <- ""
    if ((row$ci_lower > 0 & row$ci_upper > 0) | (row$ci_lower < 0 & row$ci_upper < 0)) {
      sig_marker <- " *"
    }
    cat(sprintf("  %-25s: ATE = %7.4f, 95%% CI [%7.4f, %7.4f], n=%d/%d%s\n",
                row$subgroup, row$ate_estimate, row$ci_lower, row$ci_upper,
                row$n_treated, row$n_control, sig_marker))
  }
}

if (nrow(Standardization_exercise_by_metformin_df) > 0) {
  cat("\nG-computation Results by Metformin Status:\n")
  for (i in 1:nrow(Standardization_exercise_by_metformin_df)) {
    row <- Standardization_exercise_by_metformin_df[i, ]
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
cat("STEP 17 COMPLETED SUCCESSFULLY\n")
cat(strrep("=", 70), "\n")
cat("Analysis date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Treatment: Exerc_more_than_4x (1 = 5+ times/week, 0 = 0-4 times/week)\n")
cat("Outcome: DunedinPACE\n")
cat("Stratification: Metformin use (users vs non-users)\n")
cat("Covariates: Age, Sex, Education, Tobacco, Alcohol, BMI, Stress, Sleep\n")
cat("           (Exercise.per.week EXCLUDED from adjustment)\n")
cat(strrep("=", 70), "\n")
