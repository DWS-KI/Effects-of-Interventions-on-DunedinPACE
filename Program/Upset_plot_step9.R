# =============================================================================
# STEP 9: UPSET PLOT FOR INTERVENTION OVERLAP
# =============================================================================
# This script creates an UpSet plot showing overlap patterns between
# different anti-aging interventions used by study participants.
#
# Output: upset_plot.jpeg saved to Output/ folder
# =============================================================================

# install.packages("UpSetR") # Uncomment if UpSetR is not installed


# Load UpSetR package
library(UpSetR)
library(here)

# Source display names function for reader-friendly labels
source(here::here("Program", "Functions", "display_names.R"))
source(here::here("Program", "Functions", "intervention_combination_functions.R"))

load(here::here("Output", "data_after_step4"))

data <- data_after_step4



columns_to_use <- intervention_pattern_columns()

# Create display name mapping for column renaming
display_columns <- sapply(columns_to_use, format_display_name)

# Check if columns are binary (0/1)
for (col in columns_to_use) {
  if (col %in% names(data)) {
    unique_vals <- unique(data[[col]][!is.na(data[[col]])])
    if (!all(unique_vals %in% c(0, 1))) {
      warning(paste("Column", col, "is not binary. Unique values:", paste(unique_vals, collapse=", ")))
    }
  } else {
    warning(paste("Column", col, "not found in data"))
  }
}


# Convert NAs to 0 in the columns to use
data_NA_converted <- data
data_NA_converted[columns_to_use] <- prepare_intervention_pattern_data(data)

# Rename columns to display names for the plot
for (i in seq_along(columns_to_use)) {
  names(data_NA_converted)[names(data_NA_converted) == columns_to_use[i]] <- display_columns[i]
}

#Save as PNG with specific directory. OPEN WITH PAINT, OTHERWISE IT WONT WORK
# Close any existing graphics devices to ensure clean state
while (!is.null(dev.list())) {
  dev.off()
}

# Open PNG device
png(here::here("Output", "upset_plot.jpeg"), 
    width=1500, height=1000, res=120)

# Create the upset plot using display names
# Select the most frequent exact patterns across all combination sizes.
# Default degree ordering can fill the 30 displayed columns with singles and pairs.
print(upset(data_NA_converted,
            sets = display_columns,
            nintersects = 30,
            order.by = "freq",
            decreasing = TRUE,
            text.scale = 1))            # Same parameters as your original plot

# Close the device and suppress the return value
invisible(dev.off())
