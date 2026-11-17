# =============================================================================
# Master_parallel.R - Run all analysis steps with parallel execution
# =============================================================================
# This script runs all step files with steps 6-17, 22, 23 and 25 parallelized using future.
# Run this script BEFORE rendering Manuscript.qmd
#
# Workflow:
#   1. Run Master_parallel.R (this file) - runs all analyses, saves outputs
#   2. Render Manuscript.qmd - loads saved outputs, generates document
#
# Parallelization:
#   - Steps 1-5: Sequential (required for data preparation and objects)
#   - Steps 6-17, 22, 23 and 25: Parallel (independent analyses)
#   - Steps 18-21, 24 and 26: Sequential, after the parallel analyses finish
#     Step 24 follows step 18 and requires successful steps 7, 8 and 23.
#     Step 26 requires successful steps 7, 8 and 25.
# =============================================================================

# -----------------------------------------------------------------------------
# USER OPTIONS
# -----------------------------------------------------------------------------

# INSTRUCTIONS:
#   - Supply your real data as All_metrics_crossectional_samples.csv in
#     biological_calendar_aging/Data/EPICv1 and v2 data before running.
#   - Survey identification always runs when this script is run directly.

n_bootstrap <- 2
# Number of bootstrap iterations for causal inference analyses
# Recommended: 1000 for final analysis, 2-100 for testing
options(n_bootstrap = n_bootstrap)

# -----------------------------------------------------------------------------
# LOAD PACKAGES
# -----------------------------------------------------------------------------

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# Install pacman if not already installed
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

library(pacman)

# Load/install all required packages
p_load(
  here,           # File path management
  tidyverse,      # Data manipulation and visualization
  readr,          # Reading CSV files
  lubridate,      # Date/time handling
  dplyr,          # Data manipulation
  DiagrammeR,     # Creating flowcharts
  DiagrammeRsvg,  # Converting diagrams to SVG
  rsvg,           # SVG rendering
  UpSetR,         # UpSet plots for set intersections
  patchwork,      # Combining plots
  png,            # Reading PNG files
  grid,           # Grid graphics
  future,         # Parallel execution framework
  future.apply    # Apply functions for future
)

# -----------------------------------------------------------------------------
# PARALLEL CONFIGURATION
# -----------------------------------------------------------------------------

# Detect available cores and use all but one
n_workers <- parallel::detectCores() - 1
n_workers <- max(1, n_workers)  # Ensure at least 1 worker

message("========================================")
message("Starting Master_parallel.R analysis pipeline")
message("========================================")
message("Number of parallel workers: ", n_workers)
message("========================================\n")

# -----------------------------------------------------------------------------
# STEP 1: Survey identification (SEQUENTIAL)
# -----------------------------------------------------------------------------

message("--- STEP 1: Survey identification ---")

source(here::here("Program", "Set_survey_versions_step1.R"))

# -----------------------------------------------------------------------------
# STEP 2: Set datatypes (SEQUENTIAL)
# -----------------------------------------------------------------------------

message("\n--- STEP 2: Setting datatypes ---")
source(here::here("Program", "Set_datatypes_step2.R"))

# -----------------------------------------------------------------------------
# STEP 3: Reduce dimensions (SEQUENTIAL)
# -----------------------------------------------------------------------------

message("\n--- STEP 3: Reducing dimensions ---")
source(here::here("Program", "Reduce_dimensions_step3.R"))

# -----------------------------------------------------------------------------
# STEP 4: Flowchart and exclusions (SEQUENTIAL)
# -----------------------------------------------------------------------------

message("\n--- STEP 4: Generating flowchart and applying exclusions ---")
source(here::here("Program", "Make_flowchart_and_exclusions_step4.R"))

# Supplementary affliction table uses the same final population as Table 1.
message("\n--- STEP 4b: Summarizing self-reported afflictions ---")
source(here::here("Program", "Table1_afflictions_step4b.R"))

# -----------------------------------------------------------------------------
# STEP 5: Table 1 (SEQUENTIAL)
# -----------------------------------------------------------------------------

message("\n--- STEP 5: Generating Table 1 ---")
source(here::here("Program", "Table1_DunedinPACE_step5.R"))

# -----------------------------------------------------------------------------
# STEPS 6-17, 22, 23 AND 25: PARALLEL EXECUTION
# -----------------------------------------------------------------------------

message("\n========================================")
message("Starting PARALLEL execution of steps 6-17, 22, 23 and 25")
message("Using ", n_workers, " workers")
message("========================================\n")

# Set up parallel plan
plan(multisession, workers = n_workers)

# Define the steps to run in parallel
parallel_steps <- list(
  list(step = 6,  name = "Table 1 excluded individuals",    script = "Table1_excluded_step6.R"),
  list(step = 7,  name = "IPW analysis",                    script = "IPW_step7.R"),
  list(step = 8,  name = "Standardization analysis",        script = "Standardization_step8.R"),
  list(step = 9,  name = "UpSet plot",                      script = "Upset_plot_step9.R"),
  list(step = 10, name = "Exercise effects analysis",       script = "exercise_effects_step10.R"),
  list(step = 11, name = "Sensitivity (diet)",              script = "Sensitivity_diet_step11.R"),
  list(step = 12, name = "Sensitivity (exercise type)",     script = "Sensitivity_exercise_type_step12.R"),
  list(step = 13, name = "Subgroup (sex)",                  script = "Subgroup_sex_step13.R"),
  list(step = 14, name = "Subgroup (age)",                  script = "Subgroup_age_step14.R"),
  list(step = 15, name = "Subgroup (ethnicity)",            script = "Subgroup_ethnicity_step15.R"),
  list(step = 16, name = "Metformin in training subgroups", script = "Metformin_effect_in_training_subgroups_step16.R"),
  list(step = 17, name = "Exercise effect by Metformin",    script = "Exercise_effect_by_metformin_step17.R"),
  list(step = 22, name = "Sensitivity (vitamins)",          script = "Sensitivity_vitamins_step22.R"),
  list(step = 23, name = "Sensitivity (mutual adjustment)", script = "Sensitivity_mutual_adjustment_step23.R"),
  list(step = 25, name = "Sensitivity (exact combinations)", script = "Sensitivity_combinations_step25.R")
)

# Function to run a single step with error handling
# n_bootstrap_val is passed explicitly because options don't transfer to workers
run_step <- function(step_info, n_bootstrap_val) {
  # Set the option in this worker

  options(n_bootstrap = n_bootstrap_val)
  
  step_num <- step_info$step
  step_name <- step_info$name
  script_path <- here::here("Program", step_info$script)
  
  result <- list(
    step = step_num,
    name = step_name,
    success = FALSE,
    error = NULL,
    start_time = Sys.time(),
    end_time = NULL
  )
  
  tryCatch({
    source(script_path)
    result$success <- TRUE
  }, error = function(e) {
    result$error <- conditionMessage(e)
  })
  
  result$end_time <- Sys.time()
  return(result)
}

# Record start time
parallel_start_time <- Sys.time()

# Run all steps in parallel (pass n_bootstrap to each worker)
results <- future_lapply(parallel_steps, run_step, n_bootstrap_val = n_bootstrap, future.seed = TRUE)

# Record end time
parallel_end_time <- Sys.time()
parallel_duration <- difftime(parallel_end_time, parallel_start_time, units = "mins")

# Reset to sequential execution
plan(sequential)

# -----------------------------------------------------------------------------
# PARALLEL EXECUTION SUMMARY
# -----------------------------------------------------------------------------

message("\n========================================")
message("PARALLEL execution complete")
message("Total parallel time: ", round(as.numeric(parallel_duration), 2), " minutes")
message("========================================\n")

# Report results
successful_steps <- sum(sapply(results, function(x) x$success))
failed_steps <- sum(sapply(results, function(x) !x$success))

message("Successful steps: ", successful_steps, "/", length(parallel_steps))
message("Failed steps: ", failed_steps, "/", length(parallel_steps))

# Document any errors
if (failed_steps > 0) {
  message("\n--- ERRORS ENCOUNTERED ---")
  for (res in results) {
    if (!res$success) {
      message("Step ", res$step, " (", res$name, "): ", res$error)
    }
  }
  message("----------------------------\n")
}

# Print timing for each step
message("\n--- Step Timing Summary ---")
for (res in results) {
  duration <- difftime(res$end_time, res$start_time, units = "mins")
  status <- if (res$success) "SUCCESS" else "FAILED"
  message(sprintf("Step %2d: %s (%.2f min) - %s", 
                  res$step, status, as.numeric(duration), res$name))
}
message("----------------------------\n")

# Save error log if any errors occurred
if (failed_steps > 0) {
  error_log <- data.frame(
    step = sapply(results, function(x) x$step),
    name = sapply(results, function(x) x$name),
    success = sapply(results, function(x) x$success),
    error = sapply(results, function(x) ifelse(is.null(x$error), NA, x$error)),
    duration_mins = sapply(results, function(x) 
      as.numeric(difftime(x$end_time, x$start_time, units = "mins")))
  )
  
  write.csv(error_log, 
            here::here("Output", "parallel_execution_log.csv"), 
            row.names = FALSE)
  message("Error log saved to Output/parallel_execution_log.csv")
}

# -----------------------------------------------------------------------------
# STEP 18: Metformin summary table and figure (SEQUENTIAL)
# -----------------------------------------------------------------------------

message("\n--- STEP 18: Creating Metformin summary table ---")
# Require fresh vitamin-adjusted results when step 22 is selected for this run.
# Biomarker wrappers that select only steps 5-21 do not request this analysis.
if (any(vapply(parallel_steps, function(x) x$step == 22, logical(1))) &&
    !any(vapply(results, function(x) x$step == 22 && x$success, logical(1)))) {
  stop("Step 22 must succeed before vitamin-adjusted sensitivity figures can be prepared.")
}
source(here::here("Program", "Metformin_table_and_figures_step18.R"))

# -----------------------------------------------------------------------------
# STEP 24: MUTUAL-ADJUSTMENT SENSITIVITY FIGURE (SEQUENTIAL)
# -----------------------------------------------------------------------------

# Restricted workflows that omit step 23 also omit its figure and prerequisites.
if (any(vapply(parallel_steps, function(x) x$step == 23, logical(1)))) {
  required_steps <- c(7L, 8L, 23L)
  successful_step_numbers <- vapply(Filter(function(x) x$success, results),
                                    function(x) as.integer(x$step), integer(1))
  missing_steps <- setdiff(required_steps, successful_step_numbers)
  if (length(missing_steps)) {
    stop("Steps 7, 8 and 23 must succeed in this run before the mutual-adjustment figure can be prepared. ",
         "Missing or failed steps: ", paste(missing_steps, collapse = ", "), call. = FALSE)
  }
  message("\n--- STEP 24: Creating mutual-adjustment sensitivity figure ---")
  source(here::here("Program", "Sensitivity_mutual_adjustment_figures_step24.R"))
}

# -----------------------------------------------------------------------------
# STEP 26: EXACT-COMBINATION SENSITIVITY FIGURE (SEQUENTIAL)
# -----------------------------------------------------------------------------

if (any(vapply(parallel_steps, function(x) x$step == 25, logical(1)))) {
  required_steps <- c(7L, 8L, 25L)
  successful_step_numbers <- vapply(Filter(function(x) x$success, results),
                                    function(x) as.integer(x$step), integer(1))
  missing_steps <- setdiff(required_steps, successful_step_numbers)
  if (length(missing_steps)) {
    stop("Steps 7, 8 and 25 must succeed in this run before the combination figure can be prepared. ",
         "Missing or failed steps: ", paste(missing_steps, collapse = ", "), call. = FALSE)
  }
  message("\n--- STEP 26: Creating exact-combination sensitivity figure ---")
  source(here::here("Program", "Sensitivity_combinations_figures_step26.R"))
}

# -----------------------------------------------------------------------------
# STEP 19: Concurrent interventions histogram (SEQUENTIAL)
# -----------------------------------------------------------------------------

message("\n--- STEP 19: Creating concurrent interventions histogram ---")
source(here::here("Program", "Concurrent_treatments_histogram_step19.R"))

# -----------------------------------------------------------------------------
# STEP 20: PREPARE DE-IDENTIFIED METADATA FOR MANUSCRIPT
# -----------------------------------------------------------------------------

message("\n--- STEP 20: Preparing de-identified metadata for manuscript ---")
# Do not package stale step 6 outputs if that worker failed in this master run.
if (!any(vapply(results, function(x) x$step == 6 && x$success, logical(1)))) {
  stop("Step 6 must succeed before manuscript metadata can be prepared.")
}
source(here::here("Program", "Prepare_manuscript_metadata_step20.R"))

# -----------------------------------------------------------------------------
# STEP 21: IPW DIAGNOSTICS FOR INTERVENTIONS (SEQUENTIAL)
# -----------------------------------------------------------------------------

message("\n--- STEP 21: Running IPW diagnostics for rare interventions ---")
source(here::here("Program", "IPW_diagnostics_step21.R"))

# -----------------------------------------------------------------------------
# COMPLETE
# -----------------------------------------------------------------------------

message("\n========================================")
message("Master_parallel.R analysis pipeline COMPLETE")
message("========================================")
if (failed_steps > 0) {
  message("WARNING: ", failed_steps, " step(s) failed. Check errors above.")
}
message("You can now render Manuscript.qmd")
message("========================================")
