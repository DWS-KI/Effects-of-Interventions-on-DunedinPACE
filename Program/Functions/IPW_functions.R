# =============================================================================
# IPW FUNCTIONS FOR BOOTSTRAP-BASED AVERAGE TREATMENT EFFECT ESTIMATION
# =============================================================================
# This file contains modular functions for conducting inverse probability
# weighting (IPW) analysis with bootstrap-based confidence intervals

# Load required packages for plotting
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
  library(tidyr)
}

if (!require("here", quietly = TRUE)) {
  install.packages("here")
  library(here)
}

# Source display names function for reader-friendly labels
source(here::here("Program", "Functions", "display_names.R"))
source(here::here("Program", "Functions", "love_plot_functions.R"))

# -----------------------------------------------------------------------------
# create_treatment_variable
# -----------------------------------------------------------------------------
#' Create binary treatment variable from medication column
#'
#' @param data Data frame containing the medication variable
#' @param treatment_name Name of the medication column to convert
#' @return Data frame with added binary 'treatment' column (0 = Non-User, 1 = User)
#' @examples
#' data <- create_treatment_variable(data, "Metformin")
create_treatment_variable <- function(data, treatment_name) {
  cat("Creating treatment variable for:", treatment_name, "\n")
  
  if (!treatment_name %in% names(data)) {
    stop("Treatment variable '", treatment_name, "' not found in data")
  }
  
  data$treatment <- ifelse(data[[treatment_name]] == 1 | data[[treatment_name]] == TRUE, 1, 0)
  data$treatment <- as.factor(data$treatment)
  
  cat("  Treatment group (", treatment_name, "):", sum(data$treatment == 1, na.rm = TRUE), "observations\n")
  cat("  Control group:", sum(data$treatment == 0, na.rm = TRUE), "observations\n")
  
  return(data)
}

# -----------------------------------------------------------------------------
# prepare_complete_cases
# -----------------------------------------------------------------------------
#' Check for complete cases and throw error if missing data found
#'
#' @param data Data frame to check
#' @param covariates Character vector of covariate names
#' @param strict_treatment Logical. If TRUE (default), throws error if treatment has NA.
#'   If FALSE, filters out rows with NA treatment before checking covariates.
#' @param filter_incomplete_covariates Logical. If TRUE, filters out rows with missing
#'   covariate data instead of throwing an error. Default is FALSE.
#' @return Data frame (unchanged if all cases complete, or filtered if filter options are TRUE)
prepare_complete_cases <- function(data, covariates, strict_treatment = TRUE, 
                                   filter_incomplete_covariates = FALSE) {
  cat("\nChecking for complete cases...\n")
  cat("  Strict treatment requirement:", strict_treatment, "\n")
  cat("  Filter incomplete covariates:", filter_incomplete_covariates, "\n")
  
  # Check that all variables exist
  ps_vars <- c("treatment", covariates)
  missing_vars <- ps_vars[!ps_vars %in% names(data)]
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
  
  # Now check covariates for complete cases
  incomplete_cases <- !complete.cases(data[, covariates])
  n_incomplete <- sum(incomplete_cases)
  
  cat("  Total rows:", nrow(data), "\n")
  cat("  Complete cases (covariates):", sum(!incomplete_cases), "\n")
  cat("  Incomplete cases (covariates):", n_incomplete, "\n")
  
  # Handle incomplete covariate cases
  if (n_incomplete > 0) {
    # Identify which variables have missing data
    missing_by_var <- sapply(covariates, function(var) {
      sum(is.na(data[[var]]))
    })
    missing_by_var <- missing_by_var[missing_by_var > 0]
    
    if (filter_incomplete_covariates) {
      # Filter mode: remove rows with missing covariate data
      cat("  Filtering out", n_incomplete, "rows with missing covariate data\n")
      cat("  Variables with missing data:\n")
      for (var_name in names(missing_by_var)) {
        cat("    -", var_name, ":", missing_by_var[var_name], "missing\n")
      }
      data <- data[complete.cases(data[, covariates]), ]
      cat("  Remaining rows after filtering:", nrow(data), "\n")
    } else {
      # Strict mode: throw error
      error_msg <- paste0(
        "\n\nERROR: Incomplete cases detected in covariates!\n",
        "Number of rows with missing covariate data: ", n_incomplete, " (", 
        round(n_incomplete/nrow(data)*100, 2), "%)\n\n",
        "Variables with missing data:\n",
        paste0("  - ", names(missing_by_var), ": ", missing_by_var, 
               " missing (", round(missing_by_var/nrow(data)*100, 2), "%)",
               collapse = "\n"),
        "\n\nPlease ensure all covariate variables are complete ",
        "before running IPW analysis.\n",
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
# build_ps_formula
# -----------------------------------------------------------------------------
#' Build propensity score model formula
#'
#' @param covariates Character vector of covariate names
#' @return Formula object for propensity score model
build_ps_formula <- function(covariates) {
  formula_string <- paste("treatment ~", paste(covariates, collapse = " + "))
  return(as.formula(formula_string))
}

# -----------------------------------------------------------------------------
# fit_propensity_model
# -----------------------------------------------------------------------------
#' Fit propensity score model using logistic regression
#'
#' @param data Data frame with treatment and covariates
#' @param covariates Character vector of covariate names
#' @return Fitted glm object (logistic regression model)
fit_propensity_model <- function(data, covariates) {
  ps_formula <- build_ps_formula(covariates)
  
  ps_model <- glm(ps_formula, 
                  data = data, 
                  family = binomial)
  
  return(ps_model)
}

# -----------------------------------------------------------------------------
# calculate_ipw_weights
# -----------------------------------------------------------------------------
#' Calculate inverse probability weights for ATE
#'
#' @param treatment Binary treatment indicator (0 or 1)
#' @param propensity_scores Propensity scores from PS model
#' @return Numeric vector of IPW weights
calculate_ipw_weights <- function(treatment, propensity_scores) {
  weights <- ifelse(treatment == 1, 
                   1 / propensity_scores, 
                   1 / (1 - propensity_scores))
  return(weights)
}

# -----------------------------------------------------------------------------
# estimate_ate_weighted
# -----------------------------------------------------------------------------
#' Estimate average treatment effect using IP weights
#'
#' @param outcome Numeric vector of outcome values
#' @param treatment Binary treatment indicator
#' @param weights IPW weights
#' @return Scalar ATE estimate
estimate_ate_weighted <- function(outcome, treatment, weights) {
  # Calculate weighted mean for treated group
  mean_treated <- weighted.mean(outcome[treatment == 1], 
                                weights[treatment == 1],
                                na.rm = TRUE)
  
  # Calculate weighted mean for control group
  mean_control <- weighted.mean(outcome[treatment == 0], 
                                weights[treatment == 0],
                                na.rm = TRUE)
  
  # ATE is the difference
  ate <- mean_treated - mean_control
  
  return(ate)
}

# -----------------------------------------------------------------------------
# calculate_weighted_smd
# -----------------------------------------------------------------------------
#' Calculate weighted standardized mean difference for a single covariate
#'
#' @param covariate Numeric or factor vector of covariate values
#' @param treatment Binary treatment indicator
#' @param weights IPW weights
#' @return Scalar SMD value (maximum across levels for categorical)
calculate_weighted_smd <- function(covariate, treatment, weights) {
  # For numeric variables
  if (is.numeric(covariate) && !is.factor(covariate)) {
    # Weighted mean for treated
    mean_treated <- weighted.mean(covariate[treatment == 1], 
                                  weights[treatment == 1], 
                                  na.rm = TRUE)
    
    # Weighted mean for control
    mean_control <- weighted.mean(covariate[treatment == 0], 
                                  weights[treatment == 0], 
                                  na.rm = TRUE)
    
    # Weighted variance for treated
    var_treated <- weighted.mean((covariate[treatment == 1] - mean_treated)^2, 
                                 weights[treatment == 1], 
                                 na.rm = TRUE)
    
    # Weighted variance for control
    var_control <- weighted.mean((covariate[treatment == 0] - mean_control)^2, 
                                 weights[treatment == 0], 
                                 na.rm = TRUE)
    
    # Pooled standard deviation
    pooled_sd <- sqrt((var_treated + var_control) / 2)
    
    # SMD
    smd <- abs((mean_treated - mean_control) / pooled_sd)
    
  } else {
    # For categorical variables, calculate SMD for ALL levels
    if (is.factor(covariate) || is.character(covariate)) {
      covariate <- as.factor(covariate)
      levels_var <- levels(covariate)
      
      # Calculate SMD for each level
      smd_by_level <- sapply(levels_var, function(level) {
        covariate_binary <- as.numeric(covariate == level)
        
        # Calculate proportion for treated
        prop_treated <- weighted.mean(covariate_binary[treatment == 1], 
                                      weights[treatment == 1], 
                                      na.rm = TRUE)
        
        # Calculate proportion for control
        prop_control <- weighted.mean(covariate_binary[treatment == 0], 
                                      weights[treatment == 0], 
                                      na.rm = TRUE)
        
        # Variance for proportions
        var_treated <- prop_treated * (1 - prop_treated)
        var_control <- prop_control * (1 - prop_control)
        
        # Pooled standard deviation
        pooled_sd <- sqrt((var_treated + var_control) / 2)
        
        # SMD (handle case where pooled_sd = 0)
        if (pooled_sd == 0 || is.na(pooled_sd)) {
          return(0)
        } else {
          return(abs((prop_treated - prop_control) / pooled_sd))
        }
      })
      
      # Return maximum SMD across all levels
      smd <- max(smd_by_level, na.rm = TRUE)
      
    } else {
      # Binary variable
      covariate_binary <- as.numeric(covariate)
      
      prop_treated <- weighted.mean(covariate_binary[treatment == 1], 
                                    weights[treatment == 1], 
                                    na.rm = TRUE)
      
      prop_control <- weighted.mean(covariate_binary[treatment == 0], 
                                    weights[treatment == 0], 
                                    na.rm = TRUE)
      
      var_treated <- prop_treated * (1 - prop_treated)
      var_control <- prop_control * (1 - prop_control)
      
      pooled_sd <- sqrt((var_treated + var_control) / 2)
      
      if (pooled_sd == 0) {
        smd <- 0
      } else {
        smd <- abs((prop_treated - prop_control) / pooled_sd)
      }
    }
  }
  
  return(smd)
}

# -----------------------------------------------------------------------------
# assess_covariate_balance
# -----------------------------------------------------------------------------
#' Assess covariate balance after weighting using SMD
#'
#' @param data Data frame with treatment, covariates, and weights
#' @param covariates Character vector of covariate names
#' @param weights IPW weights
#' @return Character string: "<0.1" if all balanced, otherwise list of unbalanced covariates
assess_covariate_balance <- function(data, covariates, weights) {
  cat("\nAssessing covariate balance...\n")
  
  # Calculate SMD for each covariate
  smd_values <- sapply(covariates, function(cov) {
    calculate_weighted_smd(data[[cov]], data$treatment, weights)
  })
  
  # Identify covariates with SMD > 0.1
  unbalanced <- names(smd_values)[smd_values > 0.1]
  
  if (length(unbalanced) == 0) {
    cat("  All covariates balanced (SMD < 0.1)\n")
    return("<0.1")
  } else {
    cat("  Unbalanced covariates (SMD > 0.1):", paste(unbalanced, collapse = ", "), "\n")
    return(paste(unbalanced, collapse = ", "))
  }
}

# -----------------------------------------------------------------------------
# calculate_unweighted_smd
# -----------------------------------------------------------------------------
#' Calculate unweighted standardized mean difference for a single covariate
#'
#' @param covariate Numeric or factor vector of covariate values
#' @param treatment Binary treatment indicator
#' @return Named vector of SMD values (one per level for categorical, scalar for numeric)
calculate_unweighted_smd <- function(covariate, treatment) {
  # For numeric variables
  if (is.numeric(covariate) && !is.factor(covariate)) {
    # Mean for treated
    mean_treated <- mean(covariate[treatment == 1], na.rm = TRUE)
    
    # Mean for control
    mean_control <- mean(covariate[treatment == 0], na.rm = TRUE)
    
    # Variance for treated
    var_treated <- var(covariate[treatment == 1], na.rm = TRUE)
    
    # Variance for control
    var_control <- var(covariate[treatment == 0], na.rm = TRUE)
    
    # Pooled standard deviation
    pooled_sd <- sqrt((var_treated + var_control) / 2)
    
    # SMD
    smd <- (mean_treated - mean_control) / pooled_sd
    
    # Return as named vector for consistency
    result <- smd
    names(result) <- "continuous"
    
  } else {
    # For categorical variables
    if (is.factor(covariate) || is.character(covariate)) {
      covariate <- as.factor(covariate)
      levels_var <- levels(covariate)
      
      # Calculate SMD for each level
      smd_by_level <- sapply(levels_var, function(level) {
        covariate_binary <- as.numeric(covariate == level)
        
        # Calculate proportion for treated
        prop_treated <- mean(covariate_binary[treatment == 1], na.rm = TRUE)
        
        # Calculate proportion for control
        prop_control <- mean(covariate_binary[treatment == 0], na.rm = TRUE)
        
        # Variance for proportions
        var_treated <- prop_treated * (1 - prop_treated)
        var_control <- prop_control * (1 - prop_control)
        
        # Pooled standard deviation
        pooled_sd <- sqrt((var_treated + var_control) / 2)
        
        # SMD
        if (pooled_sd == 0 || is.na(pooled_sd)) {
          return(0)
        } else {
          return((prop_treated - prop_control) / pooled_sd)
        }
      })
      
      result <- smd_by_level
      
    } else {
      # Binary variable
      covariate_binary <- as.numeric(covariate)
      
      prop_treated <- mean(covariate_binary[treatment == 1], na.rm = TRUE)
      prop_control <- mean(covariate_binary[treatment == 0], na.rm = TRUE)
      
      var_treated <- prop_treated * (1 - prop_treated)
      var_control <- prop_control * (1 - prop_control)
      
      pooled_sd <- sqrt((var_treated + var_control) / 2)
      
      if (pooled_sd == 0) {
        smd <- 0
      } else {
        smd <- (prop_treated - prop_control) / pooled_sd
      }
      
      result <- smd
      names(result) <- "binary"
    }
  }
  
  return(result)
}

# -----------------------------------------------------------------------------
# create_love_plot
# -----------------------------------------------------------------------------
#' Calculate signed covariate balance using the existing IPW definitions.
#' Returns one row per covariate level and weighting status for plotting.
calculate_love_plot_data <- function(data, covariates, weights) {
  # Initialize vectors to store results
  covariate_labels <- c()
  smd_before_values <- c()
  smd_after_values <- c()
  
  # Calculate SMD for each covariate
  for (cov in covariates) {
    # Unweighted SMD (before weighting)
    smd_before <- calculate_unweighted_smd(data[[cov]], data$treatment)
    
    # Weighted SMD (after weighting) 
    # For categorical, get max across levels but keep sign from unweighted
    if (length(smd_before) > 1) {
      # Multi-level categorical
      for (i in seq_along(smd_before)) {
        level_name <- names(smd_before)[i]
        covariate_labels <- c(covariate_labels, paste0(cov, ": ", level_name))
        smd_before_values <- c(smd_before_values, smd_before[i])
        
        # Calculate weighted SMD for this level
        covariate_binary <- as.numeric(data[[cov]] == level_name)
        prop_treated <- weighted.mean(covariate_binary[data$treatment == 1], 
                                      weights[data$treatment == 1], 
                                      na.rm = TRUE)
        prop_control <- weighted.mean(covariate_binary[data$treatment == 0], 
                                      weights[data$treatment == 0], 
                                      na.rm = TRUE)
        var_treated <- prop_treated * (1 - prop_treated)
        var_control <- prop_control * (1 - prop_control)
        pooled_sd <- sqrt((var_treated + var_control) / 2)
        
        if (pooled_sd == 0 || is.na(pooled_sd)) {
          smd_after_val <- 0
        } else {
          smd_after_val <- (prop_treated - prop_control) / pooled_sd
        }
        
        smd_after_values <- c(smd_after_values, smd_after_val)
      }
    } else {
      # Continuous or binary
      covariate_labels <- c(covariate_labels, cov)
      smd_before_values <- c(smd_before_values, smd_before)
      
      # Calculate weighted SMD
      if (is.numeric(data[[cov]]) && !is.factor(data[[cov]])) {
        mean_treated <- weighted.mean(data[[cov]][data$treatment == 1], 
                                      weights[data$treatment == 1], 
                                      na.rm = TRUE)
        mean_control <- weighted.mean(data[[cov]][data$treatment == 0], 
                                      weights[data$treatment == 0], 
                                      na.rm = TRUE)
        var_treated <- weighted.mean((data[[cov]][data$treatment == 1] - mean_treated)^2, 
                                     weights[data$treatment == 1], 
                                     na.rm = TRUE)
        var_control <- weighted.mean((data[[cov]][data$treatment == 0] - mean_control)^2, 
                                     weights[data$treatment == 0], 
                                     na.rm = TRUE)
        pooled_sd <- sqrt((var_treated + var_control) / 2)
        smd_after_val <- (mean_treated - mean_control) / pooled_sd
      } else {
        # Binary
        covariate_binary <- as.numeric(data[[cov]])
        prop_treated <- weighted.mean(covariate_binary[data$treatment == 1], 
                                      weights[data$treatment == 1], 
                                      na.rm = TRUE)
        prop_control <- weighted.mean(covariate_binary[data$treatment == 0], 
                                      weights[data$treatment == 0], 
                                      na.rm = TRUE)
        var_treated <- prop_treated * (1 - prop_treated)
        var_control <- prop_control * (1 - prop_control)
        pooled_sd <- sqrt((var_treated + var_control) / 2)
        
        if (pooled_sd == 0) {
          smd_after_val <- 0
        } else {
          smd_after_val <- (prop_treated - prop_control) / pooled_sd
        }
      }
      
      smd_after_values <- c(smd_after_values, smd_after_val)
    }
  }
  
  # Create data frame
  smd_df <- data.frame(
    covariate = covariate_labels,
    smd_before = smd_before_values,
    smd_after = smd_after_values,
    stringsAsFactors = FALSE
  )
  
  # Apply display names to covariate labels for reader-friendly output
  smd_df$covariate <- sapply(smd_df$covariate, format_display_name)
  
  # Reshape for plotting
  smd_long <- pivot_longer(
    smd_df,
    cols = c(smd_before, smd_after),
    names_to = "status",
    values_to = "smd"
  )
  
  return(smd_long)
}

#' Create and save Love plot showing covariate balance before and after weighting
#'
#' @param data Data frame with treatment, covariates
#' @param covariates Character vector of covariate names
#' @param weights IPW weights
#' @param treatment_name Name of treatment used in plot metadata and the output filename
#' @param output_dir Directory to save plot (default: "Output/love_plots")
#' @return ggplot object
create_love_plot <- function(data, covariates, weights, treatment_name,
                             output_dir = "Output/love_plots") {
  smd_long <- calculate_love_plot_data(data, covariates, weights)
  p <- draw_love_plot(smd_long, treatment_name)

  # Create output directory if it doesn't exist
  if (!require("here", quietly = TRUE)) {
    install.packages("here")
    library(here)
  }
  
  output_path <- here::here(output_dir)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    cat("  Created directory:", output_path, "\n")
  }
  
  # Clean treatment name for filename (remove special characters)
  clean_name <- gsub("[^A-Za-z0-9_]", "_", treatment_name)
  filename <- paste0("love_plot_", clean_name, ".png")
  full_path <- file.path(output_path, filename)
  
  # Save plot
  ggsave(
    filename = full_path,
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  # Persist aggregate plotting inputs; no patient-level data or fitted models.
  saveRDS(smd_long, file.path(output_path, paste0("love_plot_", clean_name, ".rds")))

  cat("  Love plot saved to:", full_path, "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# bootstrap_ate_single
# -----------------------------------------------------------------------------
#' Perform single bootstrap iteration for ATE estimation
#'
#' @param data Data frame with treatment, outcome, and covariates
#' @param outcome_name Name of outcome variable
#' @param covariates Character vector of covariate names
#' @return Scalar ATE estimate for this bootstrap sample, or NA if failed
bootstrap_ate_single <- function(data, outcome_name, covariates) {
  # Resample with replacement
  boot_data <- data[sample(nrow(data), replace = TRUE), ]
  
  # Refit propensity score model
  boot_ps_model <- tryCatch({
    fit_propensity_model(boot_data, covariates)
  }, error = function(e) NULL)
  
  # Return NA if model fails to converge
  if (is.null(boot_ps_model)) {
    return(NA)
  }
  
  # Calculate propensity scores
  boot_ps <- predict(boot_ps_model, type = "response")
  
  # Calculate IPW weights
  boot_weights <- calculate_ipw_weights(boot_data$treatment, boot_ps)
  
  # Estimate ATE
  boot_ate <- estimate_ate_weighted(boot_data[[outcome_name]], 
                                    boot_data$treatment, 
                                    boot_weights)
  
  return(boot_ate)
}

# -----------------------------------------------------------------------------
# bootstrap_ate_confidence_interval
# -----------------------------------------------------------------------------
#' Perform bootstrap to get confidence intervals for ATE
#'
#' @param data Data frame with complete cases
#' @param outcome_name Name of outcome variable
#' @param covariates Character vector of covariate names
#' @param n_bootstrap Number of bootstrap iterations (default = 1000)
#' @param seed Random seed for reproducibility (default = 12435)
#' @return List with bootstrap estimates, CI bounds, and standard error
bootstrap_ate_confidence_interval <- function(data, 
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
    
    boot_estimates[i] <- bootstrap_ate_single(data, outcome_name, covariates)
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
# estimate_ate_for_treatment
# -----------------------------------------------------------------------------
#' Complete ATE estimation with bootstrap CI for single treatment
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
#' @param love_plot_dir Directory to save Love plots (default: "Output/love_plots")
#' @return Named list with point estimate, CI, SE, and sample sizes
estimate_ate_for_treatment <- function(data, 
                                       treatment_name, 
                                       outcome_name, 
                                       covariates,
                                       n_bootstrap = 1000,
                                       strict_treatment = TRUE,
                                       filter_incomplete_covariates = FALSE,
                                       love_plot_dir = "Output/love_plots") {
  cat("\n", strrep("=", 70), "\n")
  cat("ANALYZING TREATMENT:", treatment_name, "| OUTCOME:", outcome_name, "\n")
  cat(strrep("=", 70), "\n")
  
  # Step 1: Create treatment variable
  data <- create_treatment_variable(data, treatment_name)
  
  # Step 2: Prepare complete cases
  data_complete <- prepare_complete_cases(data, covariates, strict_treatment, 
                                          filter_incomplete_covariates)
  
  # Check if outcome exists
  if (!outcome_name %in% names(data_complete)) {
    stop("Outcome variable '", outcome_name, "' not found in data")
  }
  
  # Step 3: Fit propensity score model
  cat("\nFitting propensity score model...\n")
  ps_model <- fit_propensity_model(data_complete, covariates)
  cat("  Model fitted successfully\n")
  
  # Step 4: Calculate propensity scores
  propensity_scores <- predict(ps_model, type = "response")
  
  # Step 5: Calculate IPW weights
  weights <- calculate_ipw_weights(data_complete$treatment, propensity_scores)
  
  # Step 6: Assess covariate balance using SMD
  smd_result <- assess_covariate_balance(data_complete, covariates, weights)
  
  # Step 6b: Create and save Love plot
  cat("\nCreating Love plot...\n")
  tryCatch({
    create_love_plot(data_complete, covariates, weights, treatment_name, output_dir = love_plot_dir)
  }, error = function(e) {
    cat("  Warning: Could not create Love plot:", e$message, "\n")
  })
  
  # Step 7: Estimate point estimate of ATE
  cat("\nCalculating point estimate...\n")
  ate_point <- estimate_ate_weighted(data_complete[[outcome_name]], 
                                     data_complete$treatment, 
                                     weights)
  cat("  ATE point estimate:", round(ate_point, 6), "\n")
  
  # Step 8: Bootstrap for confidence intervals
  boot_results <- bootstrap_ate_confidence_interval(data_complete, 
                                                    outcome_name, 
                                                    covariates, 
                                                    n_bootstrap)
  
  # Step 9: Compile results
  results <- list(
    treatment = treatment_name,
    outcome = outcome_name,
    ate_estimate = ate_point,
    ci_lower = boot_results$ci_lower,
    ci_upper = boot_results$ci_upper,
    std_error = boot_results$std_error,
    n_treated = sum(data_complete$treatment == 1),
    n_control = sum(data_complete$treatment == 0),
    n_bootstrap = boot_results$n_successful,
    SMD = smd_result
  )
  
  cat("\n=== Results Summary ===\n")
  cat("ATE Estimate:", round(results$ate_estimate, 6), "\n")
  cat("95% CI: [", round(results$ci_lower, 6), ",", round(results$ci_upper, 6), "]\n")
  cat("Bootstrap SE:", round(results$std_error, 6), "\n")
  
  return(results)
}

# -----------------------------------------------------------------------------
# IPW
# -----------------------------------------------------------------------------
#' Estimate average treatment effects for multiple treatments
#'
#' Main function that performs IPW analysis with bootstrap CIs for multiple
#' treatments and outcomes. This is the primary user-facing function.
#'
#' @param data Data frame containing all variables
#' @param treatment_list Character vector of treatment variable names
#' @param outcome_list Character vector of outcome variable names
#' @param covariates Character vector of covariate names to include in PS model
#' @param n_bootstrap Number of bootstrap iterations (default = 1000)
#' @param strict_treatment Logical. If TRUE (default), throws error if treatment has NA.
#'   If FALSE, filters out rows with NA treatment before analysis.
#' @param filter_incomplete_covariates Logical. If TRUE, filters out rows with missing
#'   covariate data instead of throwing an error. Default is FALSE.
#' @param love_plot_dir Directory to save Love plots (default: "Output/love_plots")
#' @return Data frame with ATE results for all treatment-outcome combinations
#' @export
#' @examples
#' covariates <- c("Decimal.Chronological.Age", "Biological.Sex", 
#'                 "Level.of.Education", "BMI")
#' results <- IPW(data, 
#'                treatment_list = c("Metformin", "Aspirin"),
#'                outcome_list = c("DunedinPACE"),
#'                covariates = covariates,
#'                n_bootstrap = 1000)
IPW <- function(data, 
                treatment_list, 
                outcome_list = "DunedinPACE",
                covariates, 
                n_bootstrap = 1000,
                strict_treatment = TRUE,
                filter_incomplete_covariates = FALSE,
                love_plot_dir = "Output/love_plots") {
  
  cat("\n")
  cat(strrep("=", 70), "\n")
  cat("IPW ANALYSIS WITH BOOTSTRAP CONFIDENCE INTERVALS\n")
  cat(strrep("=", 70), "\n")
  cat("Treatments to analyze:", paste(treatment_list, collapse = ", "), "\n")
  cat("Outcomes to analyze:", paste(outcome_list, collapse = ", "), "\n")
  cat("Number of covariates:", length(covariates), "\n")
  cat("Bootstrap iterations:", n_bootstrap, "\n")
  cat("Strict treatment requirement:", strict_treatment, "\n")
  cat("Filter incomplete covariates:", filter_incomplete_covariates, "\n")
  cat("Love plot directory:", love_plot_dir, "\n")
  cat(strrep("=", 70), "\n")
  
  # Initialize empty list to store results
  all_results <- list()
  
  # Loop through all treatment-outcome combinations
  counter <- 1
  for (treatment in treatment_list) {
    for (outcome in outcome_list) {
      cat("\n[", counter, "/", length(treatment_list) * length(outcome_list), "] ")
      
      # Estimate ATE for this combination
      result <- estimate_ate_for_treatment(data, 
                                          treatment, 
                                          outcome, 
                                          covariates, 
                                          n_bootstrap,
                                          strict_treatment,
                                          filter_incomplete_covariates,
                                          love_plot_dir)
      
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
