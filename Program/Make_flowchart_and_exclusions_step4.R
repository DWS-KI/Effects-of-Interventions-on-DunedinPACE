# =============================================================================
# FILENAME: Make_flowchart_and_exclusions_step4.R
# PROJECT: Biological Calendar Aging
# PURPOSE: Create a flow chart to describe the study population and apply exclusion criteria
# AUTHOR: Created based on Yuliya Leontyeva's example
# CREATED: October 28, 2025
# INPUT DATA: Data files from Output folder after step 3
# OUTPUT: Flow chart showing exclusions, data_after_step4 with filtered dataset
# =============================================================================

here::here()

# necessary libraries 
library(dplyr)
# install.packages("here")
library(here)

# install.packages(c("DiagrammeRsvg", "rsvg"), repos='http://cran.rstudio.com')


# Install khsmisc package if not already installed
if (!require("khsmisc", quietly = TRUE)) {
  remotes::install_github("stopsack/khsmisc")
}
library(khsmisc)

# Source the exclusion criteria function
source(here::here("Program", "Functions", "get_exclusion_criteria.R"))

# =============================================================================
# LOAD DATA FROM EACH STEP
# =============================================================================


# Load data after Step 3 (Recode tobacco use)
load(here::here("Output", "data_after_step3"))



# =============================================================================
# CREATE EXCLUSION CRITERIA
# =============================================================================

# Create a working dataset starting from Step 3
data_working <- data_after_step3

# Convert Collection.Date to Date format for any date-based filtering
data_working$Collection.Date <- as.Date(data_working$Collection.Date, format = "%m/%d/%Y")

# Ensure numeric columns are properly typed
if ("Decimal.Chronological.Age" %in% names(data_working)) {
  data_working$Decimal.Chronological.Age <- as.numeric(data_working$Decimal.Chronological.Age)
}

# Get the exclusion criteria from centralized function
criteria <- get_exclusion_criteria()

# =============================================================================
# GENERATE EXCLUSION DESIGN
# =============================================================================

# Generate the exclusion design
exd <- make_exclusions(criteria = criteria, data = data_working)

# Print summary statistics
print(exd)

# =============================================================================
# PLOT FLOW CHART
# =============================================================================

# Generate the flow chart
plot <- exclusion_flowchart(exd, width = 3)

# Display the plot
plot

# =============================================================================
# SAVE FLOW CHART
# =============================================================================

# Save as PNG
svg <- DiagrammeRsvg::export_svg(plot)
rsvg::rsvg_png(charToRaw(svg), 
               here::here("Output", "flowchart_study_population_steps1234.png"))

cat("\nFlow chart saved to:", here::here("Output", "flowchart_study_population_steps1234.png"), "\n")

# =============================================================================
# SAVE FILTERED DATA
# =============================================================================

# Extract the filtered dataset from the exclusion design object
# The final filtered data is in the last row of the 'included' column
data_after_step4 <- exd$included[[nrow(exd)]]

# Persist only aggregate exclusion counts for independent metadata preparation.
if (is.list(exd) && "table" %in% names(exd)) {
  exd_table <- exd$table
} else if (is.data.frame(exd)) {
  exd_table <- exd
} else {
  stop("exd object has unexpected structure")
}
exd_table <- data.frame(
  n_left = as.numeric(exd_table$n_left),
  n_right = as.numeric(exd_table$n_right)
)
saveRDS(exd_table, here::here("Output", "exd_table.rds"))

# =============================================================================
# PRINT SESSION INFO
# =============================================================================

sessionInfo()

# =============================================================================
# DROP UNUSED FACTOR LEVELS
# =============================================================================

cat("\n=== Dropping Unused Factor Levels ===\n")

# Identify factor variables
factor_vars <- names(data_after_step4)[sapply(data_after_step4, is.factor)]

if (length(factor_vars) > 0) {
  cat("Found", length(factor_vars), "factor variables\n")
  
  # Count levels before dropping
  levels_before <- sapply(data_after_step4[, factor_vars], function(x) length(levels(x)))
  
  # Drop unused levels for all factor variables
  data_after_step4[, factor_vars] <- lapply(data_after_step4[, factor_vars], droplevels)
  
  # Count levels after dropping
  levels_after <- sapply(data_after_step4[, factor_vars], function(x) length(levels(x)))
  
  # Report changes
  levels_dropped <- levels_before - levels_after
  if (any(levels_dropped > 0)) {
    cat("\nLevels dropped by variable:\n")
    for (var in names(levels_dropped[levels_dropped > 0])) {
      cat("  ", var, ": ", levels_before[var], " -> ", levels_after[var], 
          " (", levels_dropped[var], " dropped)\n", sep = "")
    }
  } else {
    cat("No unused factor levels found\n")
  }
  
} else {
  cat("No factor variables found in the dataset\n")
}

# Save even when there are no factor variables.
save(data_after_step4, file = here::here("Output", "data_after_step4"))
cat("\nUpdated data saved to:", here::here("Output", "data_after_step4"), "\n")

cat("Number of individuals after exclusions:", nrow(data_after_step4), "\n")
