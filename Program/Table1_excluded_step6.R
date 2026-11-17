# =============================================================================
# Table 1 Comparison: Main Analysis vs Excluded Individuals
# =============================================================================
# This script creates a comparison Table 1 showing characteristics of:
#   - Main Analysis population (from Step 5)
#   - Excluded population (due to missing covariate data)
#   - SMD between the two populations
#
# The table structure is:
#   Variable | Level | Main Analysis | Excluded | SMD
#
# A Love plot is also generated to visualize covariate balance.
# =============================================================================

# Load required packages
library(pacman)
p_load(dplyr, tableone, tidyr, writexl, ggplot2)
library(here)

# Source functions
source(here::here("Program", "Functions", "get_exclusion_criteria.R"))
source(here::here("Program", "Functions", "population_comparison_functions.R"))
source(here::here("Program", "Functions", "display_names.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

# Load data after Step 3 (before exclusions) for excluded population
load(here::here("Output", "data_after_step3"))

# Load data after Step 4 (after exclusions) for main analysis population
load(here::here("Output", "data_after_step4"))

# =============================================================================
# IDENTIFY EXCLUDED INDIVIDUALS
# =============================================================================

# Create a working dataset
data_working <- data_after_step3

# Convert Collection.Date to Date format for any date-based filtering
data_working$Collection.Date <- as.Date(data_working$Collection.Date, format = "%m/%d/%Y")

# Ensure numeric columns are properly typed
if ("Decimal.Chronological.Age" %in% names(data_working)) {
  data_working$Decimal.Chronological.Age <- as.numeric(data_working$Decimal.Chronological.Age)
}

# Get the exclusion criteria
criteria <- get_exclusion_criteria()

# Apply all exclusion criteria EXCEPT the last one (missing covariate data)
# Criterion 1: EPICv1 array only
data_step1 <- data_working %>%
  filter(!is.na(Array) & Array == "EPICv1")

# Criterion 2: Valid biological sex
data_step2 <- data_step1 %>%
  filter(!is.na(Biological.Sex) & Biological.Sex != "Other")

# Criterion 3: No metabolic conditions
data_step3 <- data_step2 %>%
  filter(
    (Diabetes2 == 0 | is.na(Diabetes2)) & 
    (Gestational.Diabetes == 0 | is.na(Gestational.Diabetes)) &
    (Polycystic.Ovary == 0 | is.na(Polycystic.Ovary)) &
    (Prediabetes == 0 | is.na(Prediabetes))
  )

# Criterion 4: Age 18-110 years
data_step4 <- data_step3 %>%
  filter(Decimal.Chronological.Age >= 18 & Decimal.Chronological.Age <= 110)

# Criterion 5: Valid DunedinPACE (not zero or negative)
data_step5 <- data_step4 %>%
  filter(!is.na(DunedinPACE) & DunedinPACE > 0)

# Criterion 6: BMI >= 15 (or missing - those are excluded in next step)
data_step6 <- data_step5 %>%
  filter(is.na(BMI) | BMI >= 15)

# Identify those who FAIL the missing covariate data criterion
data_excluded_missing <- data_step6 %>%
  filter(
    is.na(Decimal.Chronological.Age) |
    is.na(Biological.Sex) |
    is.na(Level.of.Education) |
    is.na(Tobacco.Use.Binary) |
    is.na(Alcohol.per.week) |
    is.na(Exercise.per.week) |
    is.na(BMI) |
    is.na(Stress.Level) |
    is.na(Hours.of.sleep.per.night)
  )

cat("\n=== Population Sizes ===\n")
cat("Main Analysis population:", nrow(data_after_step4), "\n")
cat("Excluded (missing covariate):", nrow(data_excluded_missing), "\n")

# =============================================================================
# DEFINE COVARIATES
# =============================================================================

covariate_cols <- c(
  "Decimal.Chronological.Age",
  "DunedinPACE",
  "Biological.Sex",
  "Level.of.Education",
  "Tobacco.Use.Binary",
  "Alcohol.per.week",
  "Exercise.per.week",
  "BMI",
  "Stress.Level",
  "Hours.of.sleep.per.night",
  "Ethnicity.Recoded"
)

# Filter to only include columns that exist in both datasets
covariate_cols <- covariate_cols[covariate_cols %in% colnames(data_after_step4)]
covariate_cols <- covariate_cols[covariate_cols %in% colnames(data_excluded_missing)]

# =============================================================================
# CREATE TABLE 1 FOR MAIN ANALYSIS POPULATION
# =============================================================================

cat("\n=== Creating Table 1 for Main Analysis population ===\n")

# Identify categorical and continuous variables
categorical_vars <- c()
continuous_vars <- c()

for (col in covariate_cols) {
  if (is.numeric(data_after_step4[[col]]) & !is.factor(data_after_step4[[col]])) {
    continuous_vars <- c(continuous_vars, col)
  } else {
    categorical_vars <- c(categorical_vars, col)
  }
}

# Create Table 1 for main analysis (Overall only)
table1_main <- CreateTableOne(
  vars = covariate_cols,
  data = data_after_step4,
  factorVars = categorical_vars,
  test = FALSE,
  addOverall = FALSE,
  includeNA = FALSE
)

# Convert to matrix - use showAllLevels = FALSE to get simpler structure
table1_main_matrix <- print(table1_main, showAllLevels = FALSE, 
                            formatOptions = list(big.mark = ","),
                            printToggle = FALSE, noSpaces = TRUE, 
                            nonnormal = continuous_vars,
                            contDigits = 1, catDigits = 1)

# Convert to data frame
table1_main_df <- as.data.frame(table1_main_matrix, stringsAsFactors = FALSE)
table1_main_df$Variable <- rownames(table1_main_matrix)
rownames(table1_main_df) <- NULL

# =============================================================================
# CREATE TABLE 1 FOR EXCLUDED POPULATION
# =============================================================================

cat("\n=== Creating Table 1 for Excluded population ===\n")

# Create Table 1 for excluded (Overall only)
table1_excluded <- CreateTableOne(
  vars = covariate_cols,
  data = data_excluded_missing,
  factorVars = categorical_vars,
  test = FALSE,
  addOverall = FALSE,
  includeNA = FALSE
)

# Convert to matrix - use showAllLevels = FALSE to match main analysis
table1_excluded_matrix <- print(table1_excluded, showAllLevels = FALSE, 
                                formatOptions = list(big.mark = ","),
                                printToggle = FALSE, noSpaces = TRUE, 
                                nonnormal = continuous_vars,
                                contDigits = 1, catDigits = 1)

# Convert to data frame
table1_excl_df <- as.data.frame(table1_excluded_matrix, stringsAsFactors = FALSE)
table1_excl_df$Variable <- rownames(table1_excluded_matrix)
rownames(table1_excl_df) <- NULL

# =============================================================================
# CALCULATE SMD BETWEEN POPULATIONS
# =============================================================================

cat("\n=== Calculating SMD between populations ===\n")

# Combine data for SMD calculation
# Main analysis = 0, Excluded = 1
data_after_step4$population_group <- 0
data_excluded_missing$population_group <- 1

# Combine datasets (only keep columns that exist in both)
common_cols <- intersect(names(data_after_step4), names(data_excluded_missing))
data_combined <- rbind(
  data_after_step4[, common_cols],
  data_excluded_missing[, common_cols]
)

# Drop unused factor levels to avoid empty categories in SMD calculation
data_combined <- droplevels(data_combined)

cat("Combined dataset size:", nrow(data_combined), "\n")

# Calculate SMD using the new function
smd_results <- calculate_population_smd(
  data = data_combined,
  covariates = covariate_cols,
  group_var = "population_group"
)

# =============================================================================
# CREATE LOVE PLOT
# =============================================================================

cat("\n=== Creating Love plot ===\n")

love_plot <- create_population_love_plot(
  smd_df = smd_results,
  title = "Covariate Balance: Complete Case vs. Excluded",
  output_dir = "Output/love_plots/main_sample_vs_excluded_due_to_missing_covs",
  filename = "love_plot_main_vs_excluded",
  group0_label = "Complete Case",
  group1_label = "Excluded"
)

# =============================================================================
# BUILD COMPARISON TABLE
# =============================================================================

cat("\n=== Building comparison table ===\n")

# Debug: Print structure of both tables
cat("\n=== Debug: table1_main_df Variable column values ===\n")
print(table1_main_df$Variable)

cat("\n=== Debug: table1_excl_df Variable column values ===\n")
print(table1_excl_df$Variable)

cat("\n=== Debug: smd_results$covariate values ===\n")
print(smd_results$covariate)

# Get column names for the data columns
main_col_name <- names(table1_main_df)[!names(table1_main_df) %in% c("Variable", "level")]
excl_col_name <- names(table1_excl_df)[!names(table1_excl_df) %in% c("Variable", "level")]

# Create lookup tables using original Variable names as keys
# Qualify indented category names with their parent variable so repeated levels
# (for example, Alcohol and Exercise both having "Never") cannot collide.
make_table1_row_keys <- function(row_labels) {
  keys <- character(length(row_labels))
  parent <- NULL
  for (i in seq_along(row_labels)) {
    label <- row_labels[i]
    if (is.na(label) || !nzchar(trimws(label))) {
      stop("Table 1 contains a missing or empty row label.")
    }
    if (grepl("^\\s+", label)) {
      if (is.null(parent)) {
        stop("Table 1 category has no parent variable: ", trimws(label))
      }
      keys[i] <- paste("level", parent, trimws(label), sep = "\r")
    } else {
      parent <- label
      keys[i] <- paste("variable", label, sep = "\r")
    }
  }
  if (anyDuplicated(keys)) {
    stop("Table 1 contains duplicate variable/category keys; cannot match rows safely.")
  }
  keys
}

main_row_keys <- make_table1_row_keys(table1_main_df$Variable)
excl_row_keys <- make_table1_row_keys(table1_excl_df$Variable)
main_lookup <- setNames(table1_main_df[[main_col_name[1]]], main_row_keys)
excl_lookup <- setNames(table1_excl_df[[excl_col_name[1]]], excl_row_keys)

# Build comparison table row by row, keeping track of original variable names
comparison_df <- data.frame(
  Variable_Original = table1_main_df$Variable,
  Variable = character(nrow(table1_main_df)),
  Level = character(nrow(table1_main_df)),
  `Complete Case` = character(nrow(table1_main_df)),
  Excluded = character(nrow(table1_main_df)),
  SMD = character(nrow(table1_main_df)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Population sizes for n-row percentage formatting
n_main <- nrow(data_after_step4)
n_excluded <- nrow(data_excluded_missing)
n_total <- n_main + n_excluded

# Track current parent variable for indented rows
current_parent_cov <- ""

for (i in 1:nrow(comparison_df)) {
  orig_var_name <- comparison_df$Variable_Original[i]
  row_key <- main_row_keys[i]
  
  # Get Main Analysis value (direct from original table)
  comparison_df$`Complete Case`[i] <- main_lookup[row_key]
  
  # Get Excluded value (lookup using original variable name)
  if (row_key %in% names(excl_lookup)) {
    comparison_df$Excluded[i] <- excl_lookup[row_key]
  } else {
    comparison_df$Excluded[i] <- "NA"
  }
  
  # Parse variable name to get clean Variable and Level
  if (orig_var_name == "n") {
    comparison_df$Variable[i] <- "N"
    comparison_df$Level[i] <- ""
    comparison_df$`Complete Case`[i] <- paste0(
      format(n_main, big.mark = ","),
      " (",
      if (n_total > 0) formatC(round(100 * n_main / n_total, 1), format = "f", digits = 1) else "0.0",
      "%)"
    )
    comparison_df$Excluded[i] <- paste0(
      format(n_excluded, big.mark = ","),
      " (",
      if (n_total > 0) formatC(round(100 * n_excluded / n_total, 1), format = "f", digits = 1) else "0.0",
      "%)"
    )
    comparison_df$SMD[i] <- ""
    next
  }
  
  # Check if this is a binary variable format: "Variable = Level"
  if (grepl(" = ", orig_var_name)) {
    parts <- strsplit(orig_var_name, " = ", fixed = TRUE)[[1]]
    var_part <- parts[1]
    level_part <- parts[2]
    
    # Remove " (%)" suffix from level if present (tableone adds this for binary vars)
    level_part_clean <- gsub(" \\(%\\)$", "", level_part)
    
    comparison_df$Variable[i] <- var_part
    comparison_df$Level[i] <- level_part_clean
    current_parent_cov <- var_part
    
    # Get SMD for this binary variable level
    # Need to convert spaces back to dots for SMD lookup
    var_part_dots <- gsub(" ", ".", var_part)
    lookup_key <- paste0(var_part_dots, ": ", level_part_clean)
    
    # Debug: print lookup attempts for binary variables
    cat("  Binary var lookup: orig='", orig_var_name, "' -> key='", lookup_key, "'\n", sep = "")
    
    matching_idx <- which(smd_results$covariate == lookup_key)
    if (length(matching_idx) == 1) {
      comparison_df$SMD[i] <- format_smd_for_table(smd_results$smd[matching_idx])
      cat("    -> Found SMD:", smd_results$smd[matching_idx], "\n")
    } else {
      # Try alternative: just the variable name for binary coded as 0/1
      matching_idx <- which(smd_results$covariate == var_part_dots)
      cat("    -> Key not found, trying '", var_part_dots, "'\n", sep = "")
      if (length(matching_idx) == 1) {
        comparison_df$SMD[i] <- format_smd_for_table(smd_results$smd[matching_idx])
        cat("    -> Found SMD:", smd_results$smd[matching_idx], "\n")
      } else {
        # Try with level as numeric (0 or 1)
        lookup_key_numeric <- paste0(var_part_dots, ": 1")
        cat("    -> Not found, trying '", lookup_key_numeric, "'\n", sep = "")
        matching_idx <- which(smd_results$covariate == lookup_key_numeric)
        if (length(matching_idx) == 1) {
          comparison_df$SMD[i] <- format_smd_for_table(smd_results$smd[matching_idx])
          cat("    -> Found SMD:", smd_results$smd[matching_idx], "\n")
        } else {
          cat("    -> NOT FOUND in smd_results\n")
          comparison_df$SMD[i] <- ""
        }
      }
    }
    
  } else if (grepl(" \\(%\\)", orig_var_name)) {
    # Check if this row contains " (%) " - indicates a categorical variable header
    clean_var <- gsub(" \\(%\\)", "", orig_var_name)
    current_parent_cov <- clean_var
    comparison_df$Variable[i] <- clean_var
    comparison_df$Level[i] <- ""
    # No SMD for categorical headers
    comparison_df$SMD[i] <- ""
    
  } else if (grepl("^\\s+", orig_var_name)) {
    # This is an indented level (categorical level)
    level_name <- trimws(orig_var_name)
    comparison_df$Variable[i] <- ""
    comparison_df$Level[i] <- level_name
    
    # Get SMD for this level
    # Need to convert spaces back to dots for SMD lookup
    parent_cov_dots <- gsub(" ", ".", current_parent_cov)
    lookup_key <- paste0(parent_cov_dots, ": ", level_name)
    matching_idx <- which(smd_results$covariate == lookup_key)
    if (length(matching_idx) == 1) {
      comparison_df$SMD[i] <- format_smd_for_table(smd_results$smd[matching_idx])
    } else {
      comparison_df$SMD[i] <- ""
    }
    
  } else if (grepl(" \\(median \\[IQR\\]\\)", orig_var_name) || grepl(" \\(mean \\(SD\\)\\)", orig_var_name)) {
    # Continuous variable with summary type indicator
    clean_var <- gsub(" \\(median \\[IQR\\]\\)", "", orig_var_name)
    clean_var <- gsub(" \\(mean \\(SD\\)\\)", "", clean_var)
    comparison_df$Variable[i] <- clean_var
    comparison_df$Level[i] <- ""
    current_parent_cov <- clean_var
    
    # Get SMD for continuous variable
    matching_idx <- which(smd_results$covariate == clean_var)
    if (length(matching_idx) == 1) {
      comparison_df$SMD[i] <- format_smd_for_table(smd_results$smd[matching_idx])
    } else {
      comparison_df$SMD[i] <- ""
    }
    
  } else {
    # Plain variable name (may be binary or other)
    comparison_df$Variable[i] <- orig_var_name
    comparison_df$Level[i] <- ""
    current_parent_cov <- orig_var_name
    
    # Try to get SMD
    matching_idx <- which(smd_results$covariate == orig_var_name)
    if (length(matching_idx) == 1) {
      comparison_df$SMD[i] <- format_smd_for_table(smd_results$smd[matching_idx])
    } else {
      comparison_df$SMD[i] <- ""
    }
  }
}

# Apply display names to Variable column (for non-empty cells)
comparison_df$Variable <- sapply(comparison_df$Variable, function(v) {
  if (v == "" || v == "N") return(v)
  format_display_name(v)
})

# Remove the original variable column and reorder
comparison_df <- comparison_df[, c("Variable", "Level", "Complete Case", "Excluded", "SMD")]

# =============================================================================
# PRINT SUMMARY
# =============================================================================

cat("\n\n=== COMPARISON TABLE: Complete Case vs. Excluded ===\n\n")
print(comparison_df, row.names = FALSE)

# Print SMD summary
cat("\n=== SMD Summary ===\n")
cat("Variables with |SMD| > 0.1 (imbalanced):\n")
unbalanced <- smd_results[abs(smd_results$smd) > 0.1, ]
if (nrow(unbalanced) > 0) {
  for (i in 1:nrow(unbalanced)) {
    cat("  -", unbalanced$covariate_display[i], ":", 
        round(unbalanced$smd[i], 3), "\n")
  }
} else {
  cat("  None - all covariates balanced\n")
}

# =============================================================================
# SAVE OUTPUTS
# =============================================================================

# Rename for manuscript compatibility (replaces old table1_excluded_df)
table1_excluded_df <- comparison_df

# Save to Excel
write_xlsx(list(
  "Comparison_Table" = comparison_df,
  "SMD_Details" = smd_results
), 
path = here::here("Output", "Table1_comparison_main_vs_excluded.xlsx"),
col_names = TRUE,
format_headers = TRUE)

# Save the table1_excluded_df object for use in the manuscript
save(table1_excluded_df, file = here::here("Output", "table1_excluded_df"))

# Also save the count of excluded individuals
n_excluded_missing_covariate <- nrow(data_excluded_missing)
save(n_excluded_missing_covariate, file = here::here("Output", "n_excluded_missing_covariate"))

# Save SMD results for potential use elsewhere
save(smd_results, file = here::here("Output", "smd_main_vs_excluded"))

cat("\n\nOutputs saved to:\n")
cat("  Excel: ", here::here("Output", "Table1_comparison_main_vs_excluded.xlsx"), "\n")
cat("  Love plot: ", here::here("Output", "love_plots", "main_sample_vs_excluded_due_to_missing_covs", "love_plot_main_vs_excluded.png"), "\n")
cat("  R object (table1_excluded_df): ", here::here("Output", "table1_excluded_df"), "\n")
cat("  R object (smd_main_vs_excluded): ", here::here("Output", "smd_main_vs_excluded"), "\n")
cat("  Count: ", here::here("Output", "n_excluded_missing_covariate"), "\n")
cat("\nNumber of excluded individuals due to missing covariate data:", n_excluded_missing_covariate, "\n")
