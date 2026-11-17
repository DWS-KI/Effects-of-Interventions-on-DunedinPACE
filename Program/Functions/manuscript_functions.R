# manuscript_functions.R
# Helper functions for extracting and formatting values in Manuscript.qmd

# =============================================================================
# HELPER FUNCTION: Normalize apostrophes for matching
# =============================================================================
# Converts curly apostrophes to straight apostrophes for consistent matching
normalize_apostrophes <- function(x) {
  gsub("\u2019|\u2018|\u0060|\u00B4", "'", x)
}

# =============================================================================
# ORDINAL LEVEL ORDERING
# =============================================================================
# Define the correct ordinal ordering for categorical variables
# These are used to reorder table rows for proper display

# Education: Lowest to Highest (Professional before Doctorate)
education_level_order <- c(
  "Did not complete high school",
  "High school or equivalent",
  "Technical or occupational certificate",
  "Some college coursework completed",
  "Associate degree",
  "Bachelor's degree",
  "Master's degree",
  "Professional (MD, DO, DDS, JD)",
  "Doctorate (PhD)"
)

# Alcohol use: Never to Most Frequent
alcohol_level_order <- c(
  "Never",
  "On special occasions",
  "Once per week",
  "3-5 times per week",
  "Regularly"
)

# Exercise per week: Never to Most Frequent
exercise_level_order <- c(
  "Never",
  "1-2 times per week",
  "3-4 times per week",
  "5-7 times per week",
  "8 or more times per week"
)

# Hours of sleep: Least to Most
sleep_level_order <- c(
  "6 hours or less",
  "6 to 8 hours",
  "More than 8 hours"
)

# =============================================================================
# TABLE REORDERING FUNCTION
# =============================================================================

#' Reorder table rows so that categorical levels appear in proper ordinal order
#' 
#' @param df A data frame with Variable and level columns (from tableone output)
#' @param level_col The name of the column containing level values (default: "level")
#' @return The data frame with rows reordered within each variable group
reorder_table_levels <- function(df, level_col = "level") {
  
  # If level column doesn't exist, try "Level" (case-insensitive)
  if (!level_col %in% names(df)) {
    if ("Level" %in% names(df)) {
      level_col <- "Level"
    } else {
      # No level column found, return unchanged
      return(df)
    }
  }
  
  # Create a combined ordering vector for all ordinal variables
  all_level_orders <- list(
    education = education_level_order,
    alcohol = alcohol_level_order,
    exercise = exercise_level_order,
    sleep = sleep_level_order
  )
  
  # Create a lookup table for level ordering
  # Normalize keys to handle different apostrophe encodings
  level_order_lookup <- c()
  for (order_vec in all_level_orders) {
    for (i in seq_along(order_vec)) {
      # Store with normalized key
      normalized_key <- normalize_apostrophes(order_vec[i])
      level_order_lookup[normalized_key] <- i
    }
  }
  
  var_col <- if ("Variable" %in% names(df)) "Variable" else names(df)[1]
  
  # Mark header rows (where Variable is not empty)
  df$is_header <- df[[var_col]] != "" & !is.na(df[[var_col]])
  
  # Create group identifier based on header rows
  df$group_id <- cumsum(df$is_header)
  
  # Store original Variable values (to relocate header labels)
  df$orig_variable <- df[[var_col]]
  
  # Add a sort key column based on level ordering
  df$sort_key <- sapply(df[[level_col]], function(lvl) {
    if (is.na(lvl) || lvl == "") {
      return(1000)  # Empty levels go to end
    }
    # Normalize the level for lookup
    normalized_lvl <- normalize_apostrophes(lvl)
    if (normalized_lvl %in% names(level_order_lookup)) {
      return(level_order_lookup[normalized_lvl])
    }
    return(1000)  # Unknown levels go to end
  })
  
  # For each group, reorder by sort_key
  df <- df[order(df$group_id, df$sort_key), ]
  
  # Now relocate Variable labels: the first row in each group should have the header label
  # Clear all Variable values first, then set the first row of each group
  groups <- unique(df$group_id)
  for (g in groups) {
    group_rows <- which(df$group_id == g)
    if (length(group_rows) > 0) {
      # Get the header label for this group (from orig_variable where is_header == TRUE)
      header_label <- df$orig_variable[df$group_id == g & df$is_header][1]
      if (!is.na(header_label) && header_label != "") {
        # Clear Variable for all rows in this group
        df[[var_col]][group_rows] <- ""
        # Set the Variable for the first row in the reordered group
        df[[var_col]][group_rows[1]] <- header_label
      }
    }
  }
  
  # Remove helper columns
  df$sort_key <- NULL
  df$group_id <- NULL
  df$is_header <- NULL
  df$orig_variable <- NULL
  
  # Reset row names
  rownames(df) <- NULL
  
  return(df)
}

# =============================================================================
# VALUE EXTRACTION FUNCTIONS
# =============================================================================

# Extract value from IPW results
get_ipw_value <- function(treatment, column) {
  row <- IPW_results_df[IPW_results_df$treatment == treatment, ]
  if (nrow(row) == 0) return(NA)
  row[[column]]
}

# Extract value from Standardization results
get_std_value <- function(treatment, column) {
  row <- Standardization_results_df[Standardization_results_df$treatment == treatment, ]
  if (nrow(row) == 0) return(NA)
  row[[column]]
}

# Extract value from metformin summary
get_metformin_value <- function(analysis, column) {
  row <- metformin_summary[metformin_summary$Analysis == analysis, ]
  if (nrow(row) == 0) return(NA)
  row[[column]]
}

# Extract value from training results
get_training_value <- function(treatment, column) {
  row <- training_results_table[training_results_table$Treatment == treatment, ]
  if (nrow(row) == 0) return(NA)
  row[[column]]
}

# Extract value from subgroup analysis data frames
get_subgroup_value <- function(df, treatment, column) {
  row <- df[df$treatment == treatment, ]
  if (nrow(row) == 0) return(NA)
  row[[column]]
}

# Format decimal as percentage
fmt_pct <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f%%"), x * 100)
}

# Format CI string from decimal to percentage
# Converts CI strings like "[0.02, 0.05]" or "[-0.03, -0.01]" to "[2.00%, 5.00%]" or "[-3.00%, -1.00%]"
fmt_ci_pct <- function(ci_string, digits = 2) {
  # Extract numeric values from the CI string
  ci_vals <- stringr::str_extract_all(ci_string, "-?[0-9]+\\.?[0-9]*")[[1]]
  if (length(ci_vals) < 2) return(ci_string)
  lower <- as.numeric(ci_vals[1]) * 100
  upper <- as.numeric(ci_vals[2]) * 100
  sprintf(paste0("[%.", digits, "f%%, %.", digits, "f%%]"), lower, upper)
}

# =============================================================================
# RESIDUAL IMBALANCE DISPLAY
# =============================================================================

#' Format saved post-IPW diagnostics without recalculating balance or effects.
#' smd_summary contains the original "<0.1" or comma-separated covariate names.
#' treatment contains the original treatment identifiers used in diagnostic filenames.
#' analysis is a Love-plot subdirectory or an original summary-table analysis label.
#' treatment and analysis may each be scalar or have one value per summary row.
format_residual_imbalance <- function(smd_summary, treatment, analysis) {
  n <- length(smd_summary)
  if (n == 0L) return(character())
  if (!length(treatment) %in% c(1L, n) || !length(analysis) %in% c(1L, n)) {
    stop("Residual imbalance requires one treatment and analysis per row, or scalar identifiers.",
         call. = FALSE)
  }
  smd_summary <- as.character(smd_summary)
  treatment <- rep_len(as.character(treatment), n)
  analysis <- rep_len(as.character(analysis), n)

  # These aliases are the original Analysis values saved by steps 17 and 18.
  analysis_aliases <- c(
    "Main" = "main", "Males" = "subgroup_male", "Females" = "subgroup_female",
    "Younger" = "subgroup_younger", "Older" = "subgroup_older",
    "European" = "subgroup_european", "Non-European" = "subgroup_noneuropean",
    "Exercises <5x per week" = "subgroup_exercise_freq_less5",
    "Exercises 5+ times per week" = "subgroup_exercise_freq_5plus",
    "Metformin non-users" = "exercise_in_metformin_nonusers",
    "Metformin users" = "exercise_in_metformin_users"
  )
  analysis_dirs <- c(unname(analysis_aliases), "exercise_effects",
                     "sensitivity_diet", "sensitivity_exercise_type", "sensitivity_vitamins",
                     "sensitivity_mutual_adjustment", "sensitivity_combinations")
  short_names <- c(
    "Decimal.Chronological.Age" = "Age", "Biological.Sex" = "Sex",
    "Level.of.Education" = "Education", "Tobacco.Use.Binary" = "Tobacco use",
    "Alcohol.per.week" = "Alcohol", "Exercise.per.week" = "Exercise",
    "BMI" = "BMI", "Stress.Level" = "Stress", "Hours.of.sleep.per.night" = "Sleep",
    "Ethnicity.Recoded" = "Ethnicity", "Main.Diet.Factor" = "Diet",
    "Exercise.Type" = "Exercise type"
  )

  vapply(seq_len(n), function(i) {
    if (is.na(analysis[i]) || !nzchar(analysis[i]) ||
        is.na(treatment[i]) || !nzchar(treatment[i])) {
      stop("Missing residual-imbalance identifier at row ", i,
           " (analysis: ", analysis[i], "; treatment: ", treatment[i], ").", call. = FALSE)
    }
    directory <- if (analysis[i] %in% names(analysis_aliases)) {
      unname(analysis_aliases[analysis[i]])
    } else {
      analysis[i]
    }
    if (!directory %in% analysis_dirs) {
      stop("Unknown residual-imbalance analysis '", analysis[i],
           "' for treatment '", treatment[i], "'.", call. = FALSE)
    }
    # Use exactly the filename sanitization performed by create_love_plot().
    filename <- paste0("love_plot_", gsub("[^A-Za-z0-9_]", "_", treatment[i]), ".rds")
    path <- here::here("Output", "love_plots", directory, filename)
    fail <- function(reason) {
      stop("Cannot display residual imbalance for analysis '", analysis[i],
           "', treatment '", treatment[i], "': ", reason,
           "\nDiagnostic file: ", path, call. = FALSE)
    }
    if (is.na(smd_summary[i]) || !nzchar(trimws(smd_summary[i]))) {
      fail("the saved balance summary is missing.")
    }
    # Balanced rows need no additional diagnostics; preserve their classification.
    if (trimws(smd_summary[i]) == "<0.1") return("None")

    flagged <- trimws(strsplit(smd_summary[i], ",", fixed = TRUE)[[1]])
    if (any(!nzchar(flagged)) || anyDuplicated(flagged)) {
      fail("the saved list of imbalanced covariates is invalid.")
    }
    if (!file.exists(path)) {
      fail(paste0("saved signed diagnostics are missing. These files are produced by the existing ",
                  "analysis workflow; refresh the corresponding outputs separately. ",
                  "Rendering does not run analyses."))
    }
    diagnostics <- tryCatch(readRDS(path), error = function(e) {
      fail(paste0("could not read signed diagnostics: ", conditionMessage(e)))
    })
    if (!is.data.frame(diagnostics) ||
        !all(c("covariate", "status", "smd") %in% names(diagnostics))) {
      fail("signed diagnostics must contain covariate, status, and smd columns.")
    }
    if (!is.character(diagnostics$covariate) || anyNA(diagnostics$covariate) ||
        any(!nzchar(diagnostics$covariate)) || anyNA(diagnostics$status) ||
        any(!diagnostics$status %in% c("smd_before", "smd_after")) ||
        !is.numeric(diagnostics$smd)) {
      fail("signed diagnostics contain invalid labels, weighting statuses, or SMD values.")
    }
    weighted <- diagnostics[diagnostics$status == "smd_after", , drop = FALSE]
    if (nrow(weighted) == 0L || anyDuplicated(weighted$covariate) ||
        any(!is.finite(weighted$smd)) ||
        !setequal(weighted$covariate, diagnostics$covariate)) {
      fail("signed diagnostics require one finite post-weighting SMD per covariate/category.")
    }

    # Match full covariate names before shortening labels, retaining every level.
    expected <- vapply(flagged, format_display_name, character(1), USE.NAMES = FALSE)
    covariate_names <- sub(": .*", "", weighted$covariate)
    # Preserve assess_covariate_balance()'s strict cutoff using unrounded values.
    imbalanced <- abs(weighted$smd) > 0.1
    observed <- unique(covariate_names[imbalanced])
    if (!setequal(expected, observed)) {
      fail(paste0("signed diagnostics disagree with the saved balance summary. Expected: ",
                  paste(expected, collapse = ", "), "; found: ",
                  paste(observed, collapse = ", "), ". Refresh matching outputs together."))
    }
    entries <- lapply(seq_along(flagged), function(j) {
      rows <- which(imbalanced & covariate_names == expected[j])
      label <- if (flagged[j] %in% names(short_names)) {
        unname(short_names[flagged[j]])
      } else {
        expected[j]
      }
      category <- substring(weighted$covariate[rows], nchar(expected[j]) + 1L)
      signed_smd <- sub("^-", "\u2212", sprintf("%+.2f", weighted$smd[rows]))
      paste0(label, category, " (", signed_smd, ")")
    })
    paste(unlist(entries, use.names = FALSE), collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}
