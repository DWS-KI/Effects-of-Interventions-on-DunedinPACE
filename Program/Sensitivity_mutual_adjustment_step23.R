# =============================================================================
# SENSITIVITY ANALYSIS STEP 23: MUTUAL ADJUSTMENT FOR CONCLUSIVE INTERVENTIONS
# =============================================================================
# Reuse the main IPW and G-computation estimators, evaluating each intervention
# with the other 11 interventions and the nine primary covariates as adjusters.
# The fixed set reflects conclusive findings in both methods in the manuscript;
# it is not reselected using p-values or confidence intervals on each run.

library(here)
source(here::here("Program", "Functions", "IPW_functions.R"))
source(here::here("Program", "Functions", "Standardization_functions.R"))

load(here::here("Output", "data_after_step4"))
data <- data_after_step4

treatment_list <- c(
  "Metformin",
  "Antioxidants",
  "Fish.oil.omega",
  "Workout.supplements",
  "Herbal.supplements",
  "Vitamins",
  "Minerals",
  "NR.NAD",
  "Sulphoraphane.broccoli.extract",
  "Resveratrol",
  "Antiaging.Exosome",
  "Antiaging.Stem.Cell"
)

outcome_list <- "DunedinPACE"
primary_covariates <- c(
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
n_bootstrap <- getOption("n_bootstrap", default = 1000)

stopifnot(length(treatment_list) == 12L, !anyDuplicated(treatment_list),
          length(primary_covariates) == 9L, !anyDuplicated(primary_covariates),
          !any(treatment_list %in% primary_covariates))
missing_vars <- setdiff(c(treatment_list, outcome_list, primary_covariates), names(data))
if (length(missing_vars)) {
  stop("Missing variables for mutual-adjustment sensitivity analysis: ",
       paste(missing_vars, collapse = ", "), call. = FALSE)
}

message("Step 23: Mutually adjusting the 12 conclusive interventions")
ipw_results <- vector("list", length(treatment_list))
std_results <- vector("list", length(treatment_list))

# Keep exposures sequential within this worker and retain the primary cohort.
for (i in seq_along(treatment_list)) {
  exposure <- treatment_list[i]
  covariates <- c(primary_covariates, setdiff(treatment_list, exposure))
  message("Mutual adjustment [", i, "/", length(treatment_list), "]: ", exposure)

  ipw_results[[i]] <- IPW(
    data = data,
    treatment_list = exposure,
    outcome_list = outcome_list,
    covariates = covariates,
    n_bootstrap = n_bootstrap,
    strict_treatment = TRUE,
    filter_incomplete_covariates = FALSE,
    love_plot_dir = "Output/love_plots/sensitivity_mutual_adjustment"
  )
  std_results[[i]] <- Standardization(
    data = data,
    treatment_list = exposure,
    outcome_list = outcome_list,
    covariates = covariates,
    n_bootstrap = n_bootstrap,
    strict_treatment = TRUE,
    filter_incomplete_covariates = FALSE
  )
}

# Save the existing result schemas only after all exposure analyses complete.
IPW_sensitivity_mutual_adjustment_df <- do.call(rbind, ipw_results)
Standardization_sensitivity_mutual_adjustment_df <- do.call(rbind, std_results)
rownames(IPW_sensitivity_mutual_adjustment_df) <- NULL
rownames(Standardization_sensitivity_mutual_adjustment_df) <- NULL

save(IPW_sensitivity_mutual_adjustment_df,
     file = here::here("Output", "IPW_sensitivity_mutual_adjustment_df"))
save(Standardization_sensitivity_mutual_adjustment_df,
     file = here::here("Output", "Standardization_sensitivity_mutual_adjustment_df"))
message("Step 23 complete: saved IPW and G-computation mutual-adjustment results.")
