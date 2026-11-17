# =============================================================================
# POPULATION COMPARISON FUNCTIONS
# =============================================================================
# Functions for calculating standardized mean differences (SMD) between two
# populations and creating Love plots to visualize covariate balance.
#
# These functions are designed for comparing characteristics between:
# - Main analysis population vs. excluded population
# - Any two distinct groups without weighting
#
# Usage:
#   source(here::here("Program", "Functions", "population_comparison_functions.R"))
#   smd_results <- calculate_population_smd(data, covariates, group_var)
#   create_population_love_plot(smd_results, "Main vs Excluded", output_dir)
# =============================================================================

# Load required packages
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
  library(ggplot2)
}

if (!require("here", quietly = TRUE)) {
  install.packages("here")
  library(here)
}

# Source display names function for reader-friendly labels
source(here::here("Program", "Functions", "display_names.R"))

# -----------------------------------------------------------------------------
# calculate_smd_continuous
# -----------------------------------------------------------------------------
#' Calculate SMD for a continuous variable between two groups
#'
#' @param values Numeric vector of values
#' @param group Binary group indicator (0 or 1)
#' @return Scalar SMD value (signed, positive means group 1 > group 0)
calculate_smd_continuous <- function(values, group) {
  # Split by group
  group0_vals <- values[group == 0]
  group1_vals <- values[group == 1]
  
  # Remove NA values
  group0_vals <- group0_vals[!is.na(group0_vals)]
  group1_vals <- group1_vals[!is.na(group1_vals)]
  
  # Check if we have enough data
  if (length(group0_vals) < 2 || length(group1_vals) < 2) {
    return(NA)
  }
  
  # Calculate means
  mean0 <- mean(group0_vals, na.rm = TRUE)
  mean1 <- mean(group1_vals, na.rm = TRUE)
  
  # Calculate variances
  var0 <- var(group0_vals, na.rm = TRUE)
  var1 <- var(group1_vals, na.rm = TRUE)
  
  # Pooled standard deviation
  pooled_sd <- sqrt((var0 + var1) / 2)
  
  # Handle zero variance
  if (pooled_sd == 0 || is.na(pooled_sd)) {
    return(0)
  }
  
  # SMD (group 1 - group 0)
  smd <- (mean1 - mean0) / pooled_sd
  
  return(smd)
}

# -----------------------------------------------------------------------------
# calculate_smd_categorical
# -----------------------------------------------------------------------------
#' Calculate SMD for each level of a categorical variable
#'
#' @param values Factor or character vector of values
#' @param group Binary group indicator (0 or 1)
#' @return Named vector of SMD values (one per level)
calculate_smd_categorical <- function(values, group) {
  # Convert to factor
  values <- as.factor(values)
  levels_var <- levels(values)
  
  # Calculate SMD for each level
  smd_by_level <- sapply(levels_var, function(level) {
    # Create binary indicator for this level
    binary <- as.numeric(values == level)
    
    # Split by group
    group0_binary <- binary[group == 0]
    group1_binary <- binary[group == 1]
    
    # Remove NA values (from original variable)
    group0_binary <- group0_binary[!is.na(values[group == 0])]
    group1_binary <- group1_binary[!is.na(values[group == 1])]
    
    # Check if we have data
    if (length(group0_binary) == 0 || length(group1_binary) == 0) {
      return(NA)
    }
    
    # Calculate proportions
    prop0 <- mean(group0_binary, na.rm = TRUE)
    prop1 <- mean(group1_binary, na.rm = TRUE)
    
    # Variance for proportions
    var0 <- prop0 * (1 - prop0)
    var1 <- prop1 * (1 - prop1)
    
    # Pooled standard deviation
    pooled_sd <- sqrt((var0 + var1) / 2)
    
    # Handle zero variance
    if (pooled_sd == 0 || is.na(pooled_sd)) {
      return(0)
    }
    
    # SMD (group 1 - group 0)
    smd <- (prop1 - prop0) / pooled_sd
    
    return(smd)
  })
  
  names(smd_by_level) <- levels_var
  return(smd_by_level)
}

# -----------------------------------------------------------------------------
# calculate_population_smd
# -----------------------------------------------------------------------------
#' Calculate SMD for all covariates between two populations
#'
#' @param data Data frame containing covariates and group variable
#' @param covariates Character vector of covariate names
#' @param group_var Name of binary group variable (0 = reference, 1 = comparison)
#' @return Data frame with covariate labels and SMD values
#' @export
calculate_population_smd <- function(data, covariates, group_var) {
  
  cat("\nCalculating population SMD for", length(covariates), "covariates...\n")
  
  # Get group variable
  group <- data[[group_var]]
  
  # Ensure group is numeric 0/1
  if (is.factor(group)) {
    group <- as.numeric(group) - 1
  }
  
  # Initialize vectors to store results
  covariate_labels <- c()
  smd_values <- c()
  
  # Calculate SMD for each covariate
  for (cov in covariates) {
    if (!cov %in% names(data)) {
      cat("  Warning: Covariate", cov, "not found in data\n")
      next
    }
    
    values <- data[[cov]]
    
    if (is.numeric(values) && !is.factor(values)) {
      # Continuous variable
      smd <- calculate_smd_continuous(values, group)
      covariate_labels <- c(covariate_labels, cov)
      smd_values <- c(smd_values, smd)
      
    } else {
      # Categorical variable
      smd_by_level <- calculate_smd_categorical(values, group)
      
      for (i in seq_along(smd_by_level)) {
        level_name <- names(smd_by_level)[i]
        covariate_labels <- c(covariate_labels, paste0(cov, ": ", level_name))
        smd_values <- c(smd_values, smd_by_level[i])
      }
    }
  }
  
  # Create result data frame
  smd_df <- data.frame(
    covariate = covariate_labels,
    smd = smd_values,
    stringsAsFactors = FALSE
  )
  
  # Add display names
  smd_df$covariate_display <- sapply(smd_df$covariate, format_display_name)
  
  # Report summary
  n_unbalanced <- sum(abs(smd_df$smd) > 0.1, na.rm = TRUE)
  cat("  Total covariate levels:", nrow(smd_df), "\n")
  cat("  Unbalanced (|SMD| > 0.1):", n_unbalanced, "\n")
  
  return(smd_df)
}

# -----------------------------------------------------------------------------
# create_population_love_plot
# -----------------------------------------------------------------------------
#' Create and save Love plot for population comparison
#'
#' @param smd_df Data frame from calculate_population_smd()
#' @param title Plot title retained for backward compatibility but not displayed
#' @param output_dir Directory to save plot
#' @param filename Optional custom filename (without extension)
#' @param group0_label Label for group 0 (default: "Reference")
#' @param group1_label Label for group 1 (default: "Comparison")
#' @return ggplot object
#' @export
create_population_love_plot <- function(smd_df, 
                                        title = "Population Comparison",
                                        output_dir = "Output/love_plots",
                                        filename = NULL,
                                        group0_label = "Main Analysis",
                                        group1_label = "Excluded") {
  
  cat("\nCreating Love plot for population comparison...\n")
  
  # Create plot
  p <- ggplot(smd_df, aes(x = smd, y = reorder(covariate_display, smd))) +
    scale_y_discrete(labels = format_figure_category_labels) +
    geom_point(size = 3, color = "#0072B2") +
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = -0.1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "solid", color = "gray50", alpha = 0.5) +
    labs(
      title = title,
      subtitle = paste0("Standardized mean differences between ", group0_label, " and ", group1_label),
      y = "",
      x = "Standardized Mean Difference",
      caption = paste0("Positive SMD: higher in ", group1_label, " | Negative SMD: higher in ", group0_label)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_text(size = 8, color = "gray50"),
      axis.text.y = element_text(size = 9),
      panel.grid.minor = element_blank()
    )
  
  # Create output directory if it doesn't exist
  output_path <- here::here(output_dir)
  if (!dir.exists(output_path)) {
    dir.create(output_path, recursive = TRUE)
    cat("  Created directory:", output_path, "\n")
  }
  
  # Generate filename
  if (is.null(filename)) {
    filename <- "love_plot_population_comparison"
  }
  filename <- paste0(filename, ".png")
  full_path <- file.path(output_path, filename)
  
  # Save plot
  ggsave(
    filename = full_path,
    plot = p,
    width = 10,
    height = max(6, nrow(smd_df) * 0.25),
    dpi = 300
  )
  
  cat("  Love plot saved to:", full_path, "\n")
  
  return(p)
}

# -----------------------------------------------------------------------------
# get_max_smd_per_variable
# -----------------------------------------------------------------------------
#' Get maximum absolute SMD for each variable (for summary table)
#'
#' For categorical variables with multiple levels, returns the maximum
#' absolute SMD across all levels.
#'
#' @param smd_df Data frame from calculate_population_smd()
#' @param covariates Character vector of original covariate names
#' @return Named vector of max SMD per variable
#' @export
get_max_smd_per_variable <- function(smd_df, covariates) {
  
  max_smd <- sapply(covariates, function(cov) {
    # Get all rows matching this covariate
    pattern <- paste0("^", cov, "(: |$)")
    matching_rows <- grepl(pattern, smd_df$covariate)
    
    if (sum(matching_rows) == 0) {
      return(NA)
    }
    
    # Return max absolute SMD
    max(abs(smd_df$smd[matching_rows]), na.rm = TRUE)
  })
  
  return(max_smd)
}

# -----------------------------------------------------------------------------
# format_smd_for_table
# -----------------------------------------------------------------------------
#' Format SMD values for table display
#'
#' Rounds SMD and adds indicator for imbalance (|SMD| > 0.1)
#'
#' @param smd Numeric SMD value
#' @param digits Number of decimal places (default: 3)
#' @return Formatted string
#' @export
format_smd_for_table <- function(smd, digits = 3) {
  if (is.na(smd)) {
    return("")
  }
  
  formatted <- sprintf(paste0("%.", digits, "f"), smd)
  
  return(formatted)
}
