# Table 1: Descriptive Statistics for DunedinPACE < 1 vs DunedinPACE >= 1
# This script creates a comprehensive table comparing characteristics between those with DunedinPACE < 1 vs >= 1

# Load required packages
library(pacman)
p_load(dplyr, tableone, tidyr, writexl)
library(here)

# Load data
load(here::here("Output", "data_after_step4"))



# Check if DunedinPACE variable exists
if(!"DunedinPACE" %in% colnames(data_after_step4)) {
  stop("No DunedinPACE variable found in the dataset")
}

# Create binary variable: DunedinPACE_under_one (1 if DunedinPACE < 1, 0 otherwise)
data_after_step4$DunedinPACE_under_one <- ifelse(data_after_step4$DunedinPACE < 1, 1, 0)

# Define covariate columns 
covariate_cols <- c(
  "Decimal.Chronological.Age",
  "Biological.Sex",
  "Level.of.Education",
  "Tobacco.Use.Binary",
  "Alcohol.per.week",
  "Exercise.per.week",
  "BMI",
  "Stress.Level",
  "Hours.of.sleep.per.night",
  "Ethnicity.Recoded"
  # "Main.Diet.Factor",
  # "Exercise.Type",
  # "Heart.Attack",
  # "Stroke",
  # "Chronic.Bronchitis",
  # "Cystic.Fibrosis",
  # "Emphysema",
  # "Addison.Disease",
  # "Cushing.Syndrome",
  # "Osteoporosis",
  # "Cirrhosis",
  # "Hepatitis",
  # "Alzheimer",
  # "Parkinson",
  # "HIV",
  # "Lupus",
  # "Cancer.Diagnosis.any"
)

# Filter to only include columns that exist in the dataset
covariate_cols <- covariate_cols[covariate_cols %in% colnames(data_after_step4)]

# Stop if any covariate contains missing values
# Ethnicity is descriptive, not a main-model covariate; missing values are allowed.
model_covariate_cols <- setdiff(covariate_cols, "Ethnicity.Recoded")
covariates_with_missing <- model_covariate_cols[
  vapply(model_covariate_cols, function(col) any(is.na(data_after_step4[[col]])), logical(1))
]

if (length(covariates_with_missing) > 0) {
  stop(
    paste0(
      "Missing values for covariete: ",
      paste(covariates_with_missing, collapse = ", "),
      ". Rewrite table script to include missing values?"
    )
  )
}

# Print summary of DunedinPACE_under_one
cat("\n=== DunedinPACE_under_one Summary ===\n")
dunedin_summary <- table(data_after_step4$DunedinPACE_under_one, useNA = "ifany")
print(dunedin_summary)
cat("\nPercentages:\n")
print(round(prop.table(dunedin_summary) * 100, 2))

# Create a categorical variable for table creation
data_after_step4$DunedinPACE_group <- ifelse(
  data_after_step4$DunedinPACE_under_one == 1,
  "DunedinPACE < 1",
  "DunedinPACE ≥ 1"
)

# Identify categorical and continuous variables based on column datatypes
categorical_vars <- c()
continuous_vars <- c()

for (col in covariate_cols) {
  if (col %in% colnames(data_after_step4)) {
    # Use datatype to classify variables
    if (is.numeric(data_after_step4[[col]]) & !is.factor(data_after_step4[[col]])) {
      continuous_vars <- c(continuous_vars, col)
    } else {
      categorical_vars <- c(categorical_vars, col)
    }
  }
}

# Use median [IQR] for continuous variables except Stress.Level,
# which should be reported as mean (SD)
nonnormal_vars <- setdiff(continuous_vars, "Stress.Level")

# Create Table 1 using tableone package with missing values included
table1 <- CreateTableOne(
  vars = covariate_cols,
  strata = "DunedinPACE_group",
  data = data_after_step4,
  factorVars = categorical_vars,
  test = FALSE,
  addOverall = TRUE,
  includeNA = FALSE  # We'll add missing values separately for better control
)

# Print the table
cat("\n\n=== TABLE 1: Characteristics by DunedinPACE Group ===\n\n")
print(table1, showAllLevels = FALSE, formatOptions = list(big.mark = ","), nonnormal = nonnormal_vars, contDigits = 1, catDigits = 1)

# Calculate missing values for each variable by strata
missing_values_list <- list()

# Get unique strata levels
strata_levels <- unique(data_after_step4$DunedinPACE_group)
strata_levels <- strata_levels[!is.na(strata_levels)]

for (var in covariate_cols) {
  missing_row <- c(Variable = paste0(var, " (Missing)"))
  
  # Overall missing
  overall_missing <- sum(is.na(data_after_step4[[var]]))
  overall_total <- nrow(data_after_step4)
  overall_pct <- round(100 * overall_missing / overall_total, 1)
  missing_row["Overall"] <- paste0(overall_missing, " (", overall_pct, "%)")
  
  # Missing by strata
  for (strata in strata_levels) {
    strata_data <- data_after_step4[data_after_step4$DunedinPACE_group == strata, ]
    strata_missing <- sum(is.na(strata_data[[var]]))
    strata_total <- nrow(strata_data)
    strata_pct <- round(100 * strata_missing / strata_total, 1)
    missing_row[strata] <- paste0(strata_missing, " (", strata_pct, "%)")
  }
  
  # Add test column (empty for missing values)
  missing_row["test"] <- ""
  
  missing_values_list[[var]] <- missing_row
}

# Convert to data frame
missing_df <- do.call(rbind, missing_values_list)
missing_df <- as.data.frame(missing_df, stringsAsFactors = FALSE)

# Convert table1 to data frame for Excel export
table1_matrix <- print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","), 
                       printToggle = FALSE, noSpaces = TRUE, nonnormal = nonnormal_vars, 
                       contDigits = 1, catDigits = 1)
table1_detailed_df <- as.data.frame(table1_matrix, stringsAsFactors = FALSE)
table1_detailed_df$Variable <- rownames(table1_matrix)

# Ensure variable names are not truncated by preserving full row names
rownames(table1_detailed_df) <- NULL
table1_detailed_df <- table1_detailed_df[, c("Variable", setdiff(names(table1_detailed_df), "Variable"))]

# Rename "n" to "N" for proper table formatting
names(table1_detailed_df) <- gsub("^n$", "N", names(table1_detailed_df))
names(table1_detailed_df)[names(table1_detailed_df) == "level"] <- "Level"

# Add percentages to the sample size row (n)
n_row_idx <- which(table1_detailed_df$Variable == "n")
if (length(n_row_idx) > 0) {
  n_total <- nrow(data_after_step4)
  n_counts <- c(
    "Overall" = n_total,
    "DunedinPACE < 1" = sum(data_after_step4$DunedinPACE_group == "DunedinPACE < 1", na.rm = TRUE),
    "DunedinPACE ≥ 1" = sum(data_after_step4$DunedinPACE_group == "DunedinPACE ≥ 1", na.rm = TRUE)
  )

  for (col in intersect(names(n_counts), names(table1_detailed_df))) {
    pct <- if (n_total > 0) round(100 * n_counts[col] / n_total, 1) else NA_real_
    table1_detailed_df[n_row_idx[1], col] <- paste0(
      format(n_counts[col], big.mark = ","),
      " (",
      formatC(pct, format = "f", digits = 1),
      "%)"
    )
  }
}

# Ensure missing_df has the same column structure as table1_detailed_df
colnames(missing_df)[1] <- "Variable"
missing_cols <- setdiff(names(table1_detailed_df), names(missing_df))
for (col in missing_cols) {
  if (!col %in% names(missing_df)) {
    missing_df[[col]] <- ""
  }
}
# Reorder columns to match table1_detailed_df
missing_df <- missing_df[, names(table1_detailed_df)]

# Combine the main table with missing values
# Interleave missing values after their respective variables
table1_with_missing <- data.frame()
for (var in covariate_cols) {
  # Add rows from main table that belong to this variable
  var_rows <- table1_detailed_df[grepl(paste0("^", var), table1_detailed_df$Variable), ]
  table1_with_missing <- rbind(table1_with_missing, var_rows)
  
  # Add missing value row for this variable
  missing_row <- missing_df[missing_df$Variable == paste0(var, " (Missing)"), ]
  if (nrow(missing_row) > 0) {
    table1_with_missing <- rbind(table1_with_missing, missing_row)
  }
}

# Set options to prevent truncation in console output
options(width = 200)
options(max.print = 10000)

# Print missing values summary
cat("\n\n=== Missing Values Summary ===\n")
print(missing_df)

# Persist the aggregate table for independent manuscript metadata preparation.
saveRDS(table1_detailed_df, here::here("Output", "table1_detailed_df.rds"))

# Save to Excel with full variable names and missing values
write_xlsx(list(
  "Table1_with_Missing" = table1_with_missing,
  "Table1_Original" = table1_detailed_df,
  "Missing_Values_Only" = missing_df
), 
           path = here::here("Output", "Table1_DunedinPACE.xlsx"),
           col_names = TRUE,
           format_headers = TRUE)


cat("\n\nTables saved to:\n")
cat("  Excel: ", here::here("Output", "Table1_DunedinPACE.xlsx"), "\n")

