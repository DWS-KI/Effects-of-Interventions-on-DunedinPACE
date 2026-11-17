seperate_survey <- function(df, cols) {
  # Ensure the specified columns exist in the data frame
  valid_cols <- cols[cols %in% names(df)]
  
  print("Valid cols: ")
  print(valid_cols)
  
  
  # If there are no valid columns, return both data frames as empty
  if (length(valid_cols) == 0) {
    return(list(filtered = data.frame(), removed = df))
  }
  
  # Create a logical vector indicating which rows to keep
  keep_rows <- apply(df[valid_cols], 1, function(row) any(!is.na(row) & row != ""))
  
  # Print how many hits each column generated
  for (col in valid_cols) {
    hits <- sum(!is.na(df[[col]]) & df[[col]] != "")
    cat("Column", col, "has", hits, "hits.\n")
  }
  
  # Subset the original data frame based on the logical vector
  df_filtered <- df[keep_rows, , drop = FALSE]
  df_removed <- df[!keep_rows, , drop = FALSE]
  
  # Return both data frames as a list
  return(list(survey_with_cols = df_filtered, survey_without_cols = df_removed))
}


add_surv_version_col <- function(df, cols_survey2_3, cols_survey3_1) {
  
  # Store the original number of rows
  original_rows <- nrow(df)
  
  # Seperate survey1
  print("")
  print("Identifying survey 1")
  survey1 <<- seperate_survey(df, cols_survey2_3)$survey_without_cols
  survey2_3 <<- seperate_survey(df, cols_survey2_3)$survey_with_cols
  
  # Ensure surveys have data before proceeding
  if (nrow(survey1) > 0) {
    survey1$survey_version <- 1
  } else {
    survey1 <- data.frame()
  }
  print("")
  print("Identifying survey 3 from surveys 2/3")
  survey3 <<- seperate_survey(survey2_3, cols_survey3_1)$survey_with_cols
  survey2 <<- seperate_survey(survey2_3, cols_survey3_1)$survey_without_cols
  
  if (nrow(survey2) > 0) {
    survey2$survey_version <- 2
  } else {
    survey2 <- data.frame()
  }
  
  if (nrow(survey3) > 0) {
    survey3$survey_version <- 3
  } else {
    survey3 <- data.frame()
  }
  
  # Bind surveys rowwise
  combined_survey <- dplyr::bind_rows(survey1, survey2, survey3)
  
  # Check if any rows were "left out" (not classified)
  combined_rows <- nrow(combined_survey)
  left_rows <- original_rows - combined_rows
  
  print(paste("Number of rows that don't fit any survey:", left_rows))
  
  # Stop execution if there are any left rows
  if (left_rows > 0) {
    stop("Error: Some rows don't fit into any survey version. Please check your survey classification criteria.")
  }
  
  return(combined_survey)
}



#' Adjust survey_version based on a cutoff date
#'
#' @param df              Data frame containing 'Collection.Date' and 'survey_version'
#' @param before_date     Character string cutoff in "%m/%d/%Y %I:%M:%S %p" format
#' @param new_version     Value to assign to survey_version for rows before cutoff (default = 1)
#' @return                Modified data frame
adjust_survey_version_by_date <- function(df, before_date, new_version = 1) {
  cutoff <- as.POSIXct(before_date, format = "%m/%d/%Y %I:%M:%S %p")
  parsed_dates <- as.POSIXct(df$Collection.Date, format = "%m/%d/%Y %I:%M:%S %p")
  # Option 1: Using filter() to create a new filtered dataframe
  df_before_cutoff <- df %>%
    filter(as.POSIXct(Collection.Date, format = "%m/%d/%Y %I:%M:%S %p") < cutoff)
  
  df_after_cutoff <- df %>%
    filter(as.POSIXct(Collection.Date, format = "%m/%d/%Y %I:%M:%S %p") >= cutoff)
    
  cat("=== Date Analysis ===\n")
  cat("Cutoff date:", format(cutoff), "\n")
  cat("All dates length:", length(parsed_dates), "\n")
  cat("Dates before cutoff length:", nrow(df_before_cutoff), "\n")

  df_before_cutoff_new<- df_before_cutoff
  #Set survey version =1 before cutoff
  df_before_cutoff_new$survey_version <- new_version

  df_result<- rbind(df_before_cutoff_new, df_after_cutoff)


  #Print how many rows have different values in survey_version collumn before and after adjustment
  cat("Rows with survey_version =", new_version, "before cutoff and adjustment:", sum(df$survey_version == new_version, na.rm = TRUE), "\n")
  cat("Rows with survey_version =", new_version, "before cutoff, after adjustment:", sum(df_result$survey_version == new_version, na.rm = TRUE), "\n")

  return(df_result)
}
