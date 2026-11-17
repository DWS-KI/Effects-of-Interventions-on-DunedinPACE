char_cols_to_factors <- function(df, max_categories = 50, cols_to_skip = NULL) {
  # Validate cols_to_skip
  if (!is.null(cols_to_skip)) {
    if (!all(cols_to_skip %in% names(df))) {
      missing_cols <- cols_to_skip[!cols_to_skip %in% names(df)]
      stop(sprintf("The following columns to skip do not exist in the data frame: %s", 
                   paste(missing_cols, collapse = ", ")))
    }
  }
  
  # Track conversions
  converted_cols <- character()
  skipped_cols <- list(
    specified = character(),
    already_factor = character(),
    exceeded_max = character(),
    not_character = character()
  )
  
  # Iterate over each column
  for (colname in names(df)) {
    # Skip columns specified in cols_to_skip
    if (!is.null(cols_to_skip) && colname %in% cols_to_skip) {
      message(sprintf("Skipping column '%s' (specified in cols_to_skip).", colname))
      skipped_cols$specified <- c(skipped_cols$specified, colname)
      next
    }
    
    # Check if the column exists and is not NULL
    if (is.null(df[[colname]])) {
      message(sprintf("Column '%s' is NULL and will be skipped.", colname))
      next
    }
    
    # Skip if already a factor or ordered factor
    if (is.factor(df[[colname]]) || is.ordered(df[[colname]])) {
      message(sprintf("Skipping column '%s' (already a factor/ordered factor).", colname))
      skipped_cols$already_factor <- c(skipped_cols$already_factor, colname)
      next
    }
    
    # Process only character columns
    if (is.character(df[[colname]])) {
      # Calculate unique non-NA values more robustly
      unique_vals <- unique(df[[colname]][!is.na(df[[colname]])])
      num_unique <- length(unique_vals)
      
      message(sprintf("Processing column '%s' with %d unique non-NA values.", colname, num_unique))
      
      # Check against max_categories threshold
      if (num_unique > max_categories) {
        message(sprintf(" - Skipping conversion: %d categories exceeds maximum of %d. Keeping as character.", 
                       num_unique, max_categories))
        skipped_cols$exceeded_max <- c(skipped_cols$exceeded_max, 
                                       sprintf("%s (%d categories)", colname, num_unique))
        next
      }
      
      # Convert to factor
      message(sprintf(" - Converting '%s' to factor.", colname))
      df[[colname]] <- as.factor(df[[colname]])
      
      # Report factor levels
      n_levels <- length(levels(df[[colname]]))
      levels_preview <- paste(head(levels(df[[colname]]), 5), collapse = ", ")
      if (n_levels > 5) levels_preview <- paste0(levels_preview, ", ...")
      message(sprintf("   - Created factor with %d levels: %s", n_levels, levels_preview))
      
      # Track conversion
      converted_cols <- c(converted_cols, sprintf("%s (%d levels)", colname, length(levels(df[[colname]]))))
    } else {
      message(sprintf("Skipping column '%s' (type: %s, not character).", 
                     colname, class(df[[colname]])[1]))
      skipped_cols$not_character <- c(skipped_cols$not_character, 
                                      sprintf("%s (%s)", colname, class(df[[colname]])[1]))
    }
  }
  
  # Print summary
  message("\n=== CONVERSION SUMMARY ===")
  
  if (length(converted_cols) > 0) {
    message(sprintf("\nConverted to factors (%d columns):", length(converted_cols)))
    for (col in converted_cols) {
      message(sprintf("  - %s", col))
    }
  } else {
    message("\nNo columns were converted to factors.")
  }
  
  if (length(skipped_cols$specified) > 0) {
    message(sprintf("\nSkipped (specified in cols_to_skip) (%d columns):", length(skipped_cols$specified)))
    for (col in skipped_cols$specified) {
      message(sprintf("  - %s", col))
    }
  }
  
  if (length(skipped_cols$already_factor) > 0) {
    message(sprintf("\nSkipped (already factor/ordered) (%d columns):", length(skipped_cols$already_factor)))
    for (col in skipped_cols$already_factor) {
      message(sprintf("  - %s", col))
    }
  }
  
  if (length(skipped_cols$exceeded_max) > 0) {
    message(sprintf("\nSkipped (exceeded max_categories=%d) (%d columns):", max_categories, length(skipped_cols$exceeded_max)))
    for (col in skipped_cols$exceeded_max) {
      message(sprintf("  - %s", col))
    }
  }
  
  if (length(skipped_cols$not_character) > 0) {
    message(sprintf("\nSkipped (not character type) (%d columns):", length(skipped_cols$not_character)))
    for (col in skipped_cols$not_character) {
      message(sprintf("  - %s", col))
    }
  }
  
  message("\n=========================")
  message("Data frame processing complete.")
  return(df)
}