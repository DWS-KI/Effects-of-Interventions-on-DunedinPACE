# Exact-use combinations versus all other participants in the primary cohort.

# USER OPTION: change this number to select more or fewer top combinations.
# Figures, constituent-intervention rows, tables, and captions follow the saved selection.
n_top_combinations <- 10L

library(here)
source(here::here("Program", "Functions", "display_names.R"))
source(here::here("Program", "Functions", "intervention_combination_functions.R"))
source(here::here("Program", "Functions", "IPW_functions.R"))
source(here::here("Program", "Functions", "Standardization_functions.R"))

load(here::here("Output", "data_after_step4"))
prepared <- select_intervention_combinations(data_after_step4, n_top_combinations)
data <- prepared$data
combination_selection <- prepared$selection
treatment_list <- combination_selection$definitions$treatment
outcome_list <- "DunedinPACE"

# Identical to steps 7 and 8; no intervention or combination is an adjuster.
covariates <- c(
  "Decimal.Chronological.Age", "Biological.Sex", "Level.of.Education",
  "Tobacco.Use.Binary", "Alcohol.per.week", "Exercise.per.week", "BMI",
  "Stress.Level", "Hours.of.sleep.per.night"
)
n_bootstrap <- getOption("n_bootstrap", default = 1000)

# Exposure definitions are selected once, before either estimator bootstraps.
IPW_sensitivity_combinations_df <- IPW(
  data = data, treatment_list = treatment_list, outcome_list = outcome_list,
  covariates = covariates, n_bootstrap = n_bootstrap,
  strict_treatment = TRUE, filter_incomplete_covariates = FALSE,
  love_plot_dir = "Output/love_plots/sensitivity_combinations"
)
Standardization_sensitivity_combinations_df <- Standardization(
  data = data, treatment_list = treatment_list, outcome_list = outcome_list,
  covariates = covariates, n_bootstrap = n_bootstrap,
  strict_treatment = TRUE, filter_incomplete_covariates = FALSE
)

# Only aggregate results and definitions are saved, after both methods complete.
save(IPW_sensitivity_combinations_df,
     file = here::here("Output", "IPW_sensitivity_combinations_df"))
save(Standardization_sensitivity_combinations_df,
     file = here::here("Output", "Standardization_sensitivity_combinations_df"))
saveRDS(combination_selection, here::here("Output", "combination_selection.rds"))
message("Step 25 complete: saved estimates for ", n_top_combinations, " exact combinations.")
