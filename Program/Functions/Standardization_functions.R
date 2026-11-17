# =============================================================================
# STANDARDIZATION FUNCTIONS FOR G-COMPUTATION AVERAGE TREATMENT EFFECT ESTIMATION
# =============================================================================
# This file contains modular functions for conducting G-computation/standardization
# analysis with bootstrap-based confidence intervals

# -----------------------------------------------------------------------------
# create_treatment_variable_std
# -----------------------------------------------------------------------------
#' Create binary treatment variable from medication column (for standardization)
#'
#' @param data Data frame containing the medication variable
#' @param treatment_name Name of the medication column to convert
#' @return Data frame with added binary 'treatment' column (0 = Non-User, 1 = User)
create_treatment_variable_std <- function(data, treatment_name) {
  cat("Creating treatment variable for:", treatment_name, "\n")
  
  if (!treatment_name %in% names(data)) {
    stop("Treatment variable '", treatment_name, "' not found in data")
  }
  
  data$treatment <- ifelse(data[[treatment_name]] == 1 | data[[treatment_name]] == TRUE, 1, 0)
  data$treatment <- as.numeric(data$treatment)  # Keep as numeric for modeling
  
  cat("  Treatment group (", treatment_name, "):", sum(data$treatment == 1, na.rm = TRUE), "observations\n")
  cat("  Control group:", sum(data$treatment == 0, na.rm = TRUE), "observations\n")
  
  return(data)
}

# -----------------------------------------------------------------------------
# prepare_complete_cases_std
# -----------------------------------------------------------------------------
#' Remove missing values for outcome model variables
#'
#' @param data Data frame to clean
#' @param outcome_name Name of outcome variable
#' @param covariates Character vector of covariate names
#' @param strict_treatment Logical. If TRUE (default), throws error if treatment has NA.
#'   If FALSE, filters out rows with NA treatment before checking other variables.
#' @param filter_incomplete_covariates Logical. If TRUE, filters out rows with missing
#'   covariate data instead of throwing an error. Default is FALSE.
#' @return Data frame with complete cases for treatment, outcome, and covariates
prepare_complete_cases_std <- function(data, outcome_name, covariates, strict_treatment = TRUE,
                                       filter_incomplete_covariates = FALSE) {
  cat("\nChecking for complete cases for analysis...\n")
  cat("  Strict treatment requirement:", strict_treatment, "\n")
  cat("  Filter incomplete covariates:", filter_incomplete_covariates, "\n")
  
  # Check that all variables exist
  model_vars <- c("treatment", outcome_name, covariates)
  missing_vars <- model_vars[!model_vars %in% names(data)]
  if (length(missing_vars) > 0) {
    stop("Missing variables in data: ", paste(missing_vars, collapse = ", "))
  }
  
  # Handle treatment NA based on strict_treatment setting
  n_treatment_na <- sum(is.na(data$treatment))
  
  if (n_treatment_na > 0) {
    if (strict_treatment) {
      # Strict mode: throw error if treatment has NA
      error_msg <- paste0(
        "\n\nERROR: Missing treatment values detected!\n",
        "Number of rows with missing treatment: ", n_treatment_na, " (", 
        round(n_treatment_na/nrow(data)*100, 2), "%)\n\n",
        "Set strict_treatment = FALSE to filter out these rows and proceed with analysis.\n",
        "Or remove rows with missing data in Step 4."
      )
      stop(error_msg)
    } else {
      # Lenient mode: filter out NA treatment rows
      cat("  Filtering out", n_treatment_na, "rows with missing treatment values\n")
      data <- data[!is.na(data$treatment), ]
      cat("  Remaining rows after filtering:", nrow(data), "\n")
    }
  }
  
  # Now check outcome and covariates for complete cases
  check_vars <- c(outcome_name, covariates)
  incomplete_cases <- !complete.cases(data[, check_vars])
  n_incomplete <- sum(incomplete_cases)
  
  cat("  Total rows:", nrow(data), "\n")
  cat("  Complete cases (outcome + covariates):", sum(!incomplete_cases), "\n")
  cat("  Incomplete cases (outcome + covariates):", n_incomplete, "\n")
  
  # Handle incomplete cases
  if (n_incomplete > 0) {
    # Identify which variables have missing data
    missing_by_var <- sapply(check_vars, function(var) {
      sum(is.na(data[[var]]))
    })
    missing_by_var <- missing_by_var[missing_by_var > 0]
    
    if (filter_incomplete_covariates) {
      # Filter mode: remove rows with missing data
      cat("  Filtering out", n_incomplete, "rows with missing outcome/covariate data\n")
      cat("  Variables with missing data:\n")
      for (var_name in names(missing_by_var)) {
        cat("    -", var_name, ":", missing_by_var[var_name], "missing\n")
      }
      data <- data[complete.cases(data[, check_vars]), ]
      cat("  Remaining rows after filtering:", nrow(data), "\n")
    } else {
      # Strict mode: throw error
      error_msg <- paste0(
        "\n\nERROR: Incomplete cases detected in outcome/covariates!\n",
        "Number of rows with missing data: ", n_incomplete, " (", 
        round(n_incomplete/nrow(data)*100, 2), "%)\n\n",
        "Variables with missing data:\n",
        paste0("  - ", names(missing_by_var), ": ", missing_by_var, 
               " missing (", round(missing_by_var/nrow(data)*100, 2), "%)",
               collapse = "\n"),
        "\n\nPlease ensure all outcome and covariate variables are complete ",
        "before running Standardization analysis.\n",
        "Remove rows with missing data in Step 4, set filter_incomplete_covariates = TRUE,\n",
        "or handle missing data appropriately."
      )
      
      stop(error_msg)
    }
  }
  
  cat("  All cases are complete - proceeding with analysis\n")
  
  return(data)
}

# -----------------------------------------------------------------------------
# build_outcome_formula
# -----------------------------------------------------------------------------
#' Build outcome model formula for G-computation
#'
#' @param outcome_name Name of outcome variable
#' @param covariates Character vector of covariate names
#' @return Formula object for outcome model
build_outcome_formula <- function(outcome_name, covariates) {
  # Model: outcome ~ treatment + covariates (no interactions)
  formula_string <- paste(outcome_name, "~ treatment +", paste(covariates, collapse = " + "))
  return(as.formula(formula_string))
}

# -----------------------------------------------------------------------------
# fit_outcome_model
# -----------------------------------------------------------------------------
#' Fit outcome model using linear regression
#'
#' @param data Data frame with treatment, outcome, and covariates
#' @param outcome_name Name of outcome variable
#' @param covariates Character vector of covariate names
#' @return Fitted lm object
fit_outcome_model <- function(data, outcome_name, covariates) {
  outcome_formula <- build_outcome_formula(outcome_name, covariates)
  
  outcome_model <- lm(outcome_formula, data = data)
  
  return(outcome_model)
}

# -----------------------------------------------------------------------------
# predict_potential_outcomes
# -----------------------------------------------------------------------------
#' Predict potential outcomes under treatment and control for all individuals
#'
#' @param model Fitted outcome model (lm object)
#' @param data Data frame to make predictions for
#' @return List with Y1 (potential outcomes under treatment) and Y0 (under control)
predict_potential_outcomes <- function(model, data) {
  # Create datasets where everyone is treated (treatment = 1)
  data_treated <- data
  data_treated$treatment <- 1
  
  # Create datasets where everyone is control (treatment = 0)
  data_control <- data
  data_control$treatment <- 0
  
  # Predict potential outcomes
  Y1 <- predict(model, newdata = data_treated)
  Y0 <- predict(model, newdata = data_control)
  
  return(list(Y1 = Y1, Y0 = Y0))
}

# -----------------------------------------------------------------------------
# estimate_ate_gcomp
# -----------------------------------------------------------------------------
#' Estimate average treatment effect using G-computation
#'
#' @param Y1 Vector of potential outcomes under treatment
#' @param Y0 Vector of potential outcomes under control
#' @return Scalar ATE estimate
estimate_ate_gcomp <- function(Y1, Y0) {
  # ATE is the mean difference in potential outcomes
  ate <- mean(Y1, na.rm = TRUE) - mean(Y0, na.rm = TRUE)
  return(ate)
}

# -----------------------------------------------------------------------------
# bootstrap_ate_single_std
# -----------------------------------------------------------------------------
#' Perform single bootstrap iteration for G-computation ATE estimation
#'
#' @param data Data frame with treatment, outcome, and covariates
#' @param outcome_name Name of outcome variable
#' @param covariates Character vector of covariate names
#' @return Scalar ATE estimate for this bootstrap sample, or NA if failed
bootstrap_ate_single_std <- function(data, outcome_name, covariates) {
  # Resample with replacement
  boot_data <- data[sample(nrow(data), replace = TRUE), ]
  
  # Fit outcome model
  boot_model <- tryCatch({
    fit_outcome_model(boot_data, outcome_name, covariates)
  }, error = function(e) NULL)
  
  # Return NA if model fails
  if (is.null(boot_model)) {
    return(NA)
  }
  
  # Predict potential outcomes
  pot_outcomes <- predict_potential_outcomes(boot_model, boot_data)
  
  # Estimate ATE
  boot_ate <- estimate_ate_gcomp(pot_outcomes$Y1, pot_outcomes$Y0)
  
  return(boot_ate)
}

# -----------------------------------------------------------------------------
# bootstrap_ate_confidence_interval_std
# -----------------------------------------------------------------------------
#' Perform bootstrap to get confidence intervals for G-computation ATE
#'
#' @param data Data frame with complete cases
#' @param outcome_name Name of outcome variable
#' @param covariates Character vector of covariate names
#' @param n_bootstrap Number of bootstrap iterations (default = 1000)
#' @param seed Random seed for reproducibility (default = 12435)
#' @return List with bootstrap estimates, CI bounds, and standard error
bootstrap_ate_confidence_interval_std <- function(data, 
                                                  outcome_name, 
                                                  covariates, 
                                                  n_bootstrap = 1000,
                                                  seed = 12435) {
  cat("\nPerforming bootstrap (", n_bootstrap, " iterations)...\n")
  
  # Set seed for reproducibility
  set.seed(seed)
  
  # Initialize vector to store bootstrap estimates
  boot_estimates <- numeric(n_bootstrap)
  
  # Bootstrap loop with progress indication
  for (i in 1:n_bootstrap) {
    if (i %% 100 == 0) {
      cat("  Completed", i, "of", n_bootstrap, "iterations\n")
    }
    
    boot_estimates[i] <- bootstrap_ate_single_std(data, outcome_name, covariates)
  }
  
  # Remove any NA values from failed bootstrap iterations
  boot_estimates_clean <- boot_estimates[!is.na(boot_estimates)]
  
  # Calculate confidence intervals using percentile method
  ci_lower <- quantile(boot_estimates_clean, 0.025)
  ci_upper <- quantile(boot_estimates_clean, 0.975)
  
  # Calculate standard error from bootstrap distribution
  boot_se <- sd(boot_estimates_clean)
  
  cat("Bootstrap complete.\n")
  cat("  Successful iterations:", length(boot_estimates_clean), "/", n_bootstrap, "\n")
  
  return(list(
    estimates = boot_estimates_clean,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    std_error = boot_se,
    n_successful = length(boot_estimates_clean)
  ))
}

# -----------------------------------------------------------------------------
# estimate_ate_for_treatment_std
# -----------------------------------------------------------------------------
#' Complete G-computation ATE estimation with bootstrap CI for single treatment
#'
#' @param data Data frame containing all variables
#' @param treatment_name Name of treatment variable
#' @param outcome_name Name of outcome variable
#' @param covariates Character vector of covariate names
#' @param n_bootstrap Number of bootstrap iterations
#' @param strict_treatment Logical. If TRUE (default), throws error if treatment has NA.
#'   If FALSE, filters out rows with NA treatment before analysis.
#' @param filter_incomplete_covariates Logical. If TRUE, filters out rows with missing
#'   covariate data instead of throwing an error. Default is FALSE.
#' @return Named list with point estimate, CI, SE, and sample sizes
estimate_ate_for_treatment_std <- function(data, 
                                           treatment_name, 
                                           outcome_name, 
                                           covariates,
                                           n_bootstrap = 1000,
                                           strict_treatment = TRUE,
                                           filter_incomplete_covariates = FALSE) {
  cat("\n", strrep("=", 70), "\n")
  cat("ANALYZING TREATMENT:", treatment_name, "| OUTCOME:", outcome_name, "\n")
  cat(strrep("=", 70), "\n")
  
  # Step 1: Create treatment variable
  data <- create_treatment_variable_std(data, treatment_name)
  
  # Step 2: Prepare complete cases
  data_complete <- prepare_complete_cases_std(data, outcome_name, covariates, strict_treatment,
                                              filter_incomplete_covariates)
  
  # Step 3: Fit outcome model
  cat("\nFitting outcome model (linear regression)...\n")
  outcome_model <- fit_outcome_model(data_complete, outcome_name, covariates)
  cat("  Model fitted successfully\n")
  
  # Step 4: Predict potential outcomes for all individuals
  cat("\nPredicting potential outcomes...\n")
  pot_outcomes <- predict_potential_outcomes(outcome_model, data_complete)
  cat("  Potential outcomes predicted for", length(pot_outcomes$Y1), "individuals\n")
  
  # Step 5: Estimate point estimate of ATE
  cat("\nCalculating G-computation ATE estimate...\n")
  ate_point <- estimate_ate_gcomp(pot_outcomes$Y1, pot_outcomes$Y0)
  cat("  ATE point estimate:", round(ate_point, 6), "\n")
  
  # Step 6: Bootstrap for confidence intervals
  boot_results <- bootstrap_ate_confidence_interval_std(data_complete, 
                                                        outcome_name, 
                                                        covariates, 
                                                        n_bootstrap)
  
  # Step 7: Compile results
  results <- list(
    treatment = treatment_name,
    outcome = outcome_name,
    ate_estimate = ate_point,
    ci_lower = boot_results$ci_lower,
    ci_upper = boot_results$ci_upper,
    std_error = boot_results$std_error,
    n_treated = sum(data_complete$treatment == 1),
    n_control = sum(data_complete$treatment == 0),
    n_bootstrap = boot_results$n_successful
  )
  
  cat("\n=== Results Summary ===\n")
  cat("ATE Estimate:", round(results$ate_estimate, 6), "\n")
  cat("95% CI: [", round(results$ci_lower, 6), ",", round(results$ci_upper, 6), "]\n")
  cat("Bootstrap SE:", round(results$std_error, 6), "\n")
  
  return(results)
}

# -----------------------------------------------------------------------------
# Standardization
# -----------------------------------------------------------------------------
#' Estimate average treatment effects for multiple treatments using G-computation
#'
#' Main function that performs G-computation/standardization analysis with 
#' bootstrap CIs for multiple treatments and outcomes. This is the primary 
#' user-facing function.
#'
#' @param data Data frame containing all variables
#' @param treatment_list Character vector of treatment variable names
#' @param outcome_list Character vector of outcome variable names
#' @param covariates Character vector of covariate names to include in outcome model
#' @param n_bootstrap Number of bootstrap iterations (default = 1000)
#' @param strict_treatment Logical. If TRUE (default), throws error if treatment has NA.
#'   If FALSE, filters out rows with NA treatment before analysis.
#' @param filter_incomplete_covariates Logical. If TRUE, filters out rows with missing
#'   covariate data instead of throwing an error. Default is FALSE.
#' @return Data frame with ATE results for all treatment-outcome combinations
#' @export
#' @examples
#' covariates <- c("Decimal.Chronological.Age", "Biological.Sex", 
#'                 "Level.of.Education", "BMI")
#' results <- Standardization(data, 
#'                            treatment_list = c("Metformin", "Aspirin"),
#'                            outcome_list = c("DunedinPACE"),
#'                            covariates = covariates,
#'                            n_bootstrap = 1000)
Standardization <- function(data, 
                           treatment_list, 
                           outcome_list = "DunedinPACE",
                           covariates, 
                           n_bootstrap = 1000,
                           strict_treatment = TRUE,
                           filter_incomplete_covariates = FALSE) {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("G-COMPUTATION/STANDARDIZATION ANALYSIS WITH BOOTSTRAP CONFIDENCE INTERVALS\n")
  cat(strrep("=", 70), "\n")
  cat("Treatments to analyze:", paste(treatment_list, collapse = ", "), "\n")
  cat("Outcomes to analyze:", paste(outcome_list, collapse = ", "), "\n")
  cat("Number of covariates:", length(covariates), "\n")
  cat("Bootstrap iterations:", n_bootstrap, "\n")
  cat("Strict treatment requirement:", strict_treatment, "\n")
  cat("Filter incomplete covariates:", filter_incomplete_covariates, "\n")
  cat(strrep("=", 70), "\n")
  
  # Initialize empty list to store results
  all_results <- list()
  
  # Loop through all treatment-outcome combinations
  counter <- 1
  for (treatment in treatment_list) {
    for (outcome in outcome_list) {
      cat("\n[", counter, "/", length(treatment_list) * length(outcome_list), "] ")
      
      # Estimate ATE for this combination
      result <- estimate_ate_for_treatment_std(data, 
                                               treatment, 
                                               outcome, 
                                               covariates, 
                                               n_bootstrap,
                                               strict_treatment,
                                               filter_incomplete_covariates)
      
      all_results[[counter]] <- result
      counter <- counter + 1
    }
  }
  
  # Convert list of results to data frame
  results_df <- do.call(rbind, lapply(all_results, as.data.frame))
  
  rownames(results_df) <- NULL  # Reset row names
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("ANALYSIS COMPLETE\n")
  cat(strrep("=", 70), "\n")
  
  return(results_df)
}
