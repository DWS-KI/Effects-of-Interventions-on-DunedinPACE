# =============================================================================
# DISPLAY NAMES FUNCTION FOR READER-FRIENDLY VARIABLE NAMES
# =============================================================================
# This script provides a centralized function to convert internal variable names
# to reader-friendly display names for use in tables, figures, and text.
#
# Usage:
#   source(here::here("Program", "Functions", "display_names.R"))
#   clean_name <- format_display_name("Decimal.Chronological.Age")
#   # Returns: "Chronological Age"
#
#   # For vectors/columns:
#   clean_names <- sapply(original_names, format_display_name)
#
#   # For data frames (rename columns):
#   df <- rename_columns_for_display(df)
#
#   # For data frames (rename values in a specific column):
#   df <- format_display_column(df, "Treatment")
# =============================================================================

#' Format a single variable name for display
#'
#' Converts internal variable names (with dots, underscores) to reader-friendly
#' display names (with spaces, proper formatting).
#'
#' @param name Character string of the original variable name
#' @return Character string of the formatted display name
#' @export
format_display_name <- function(name) {
  
  # Handle tableone-style suffixes like "(%)","(median [IQR])", etc.
  # Extract suffix if present
  suffix_pattern <- " \\((%|median \\[IQR\\]|mean \\(SD\\)|mean \\[sd\\])\\)$"
  suffix_match <- regmatches(name, regexpr(suffix_pattern, name, ignore.case = TRUE))
  base_name <- sub(suffix_pattern, "", name, ignore.case = TRUE)

  # Define the mapping of original names to display names
  name_mapping <- c(
    # Covariate / Table 1 names
    "Decimal.Chronological.Age" = "Chronological Age",
    "Biological.Sex" = "Biological Sex",
    "Level.of.Education" = "Education Level",
    "Tobacco.Use.Binary" = "Tobacco Use",
    "Alcohol.per.week" = "Alcohol per Week",
    "Exercise.per.week" = "Exercise per Week",
    "BMI" = "BMI",
    "Stress.Level" = "Stress Level",
    "Hours.of.sleep.per.night" = "Hours of Sleep per Night",
    "Ethnicity.Recoded" = "Ethnicity",
    "DunedinPACE" = "DunedinPACE",
    "DunedinPACE_group" = "DunedinPACE Group",
    "DunedinPACE_under_one" = "DunedinPACE < 1",
    "Main.Diet.Factor" = "Main Diet",
    "Exercise.Type" = "Exercise Type",
    
    # Treatment names
    "Metformin" = "Metformin",
    "Antioxidants" = "Antioxidants",
    "Fish.oil.omega" = "Fish Oil / Omega",
    "Workout.supplements" = "Workout Supplements",
    "Herbal.supplements" = "Herbal Supplements",
    "Vitamins" = "Vitamins",
    "Minerals" = "Minerals",
    "TA.65" = "TA-65",
    "NR.NAD" = "NR / NAD+",
    "Sulphoraphane.broccoli.extract" = "Sulforaphane / Broccoli Extract",
    "Rapamycin" = "Rapamycin",
    "DHEA" = "DHEA",
    "Resveratrol" = "Resveratrol",
    "Antiaging.Exosome" = "Exosome Therapy",
    "Antiaging.Hormone.Replacement" = "Hormone Replacement Therapy",
    "Antiaging.Senolytics" = "Senolytics",
    "Antiaging.Stem.Cell" = "Stem Cell Therapy",
    "Antiaging.NAD" = "NAD Therapy",
    
    # Exercise frequency labels
    "1to2 per week" = "Exercise 1-2 Times per Week",
    "3to4 per week" = "Exercise 3-4 Times per Week",
    "5to7 per week" = "Exercise 5-7 Times per Week",
    "8plus per week" = "Exercise 8+ Times per Week",
    "1to2.per.week" = "Exercise 1-2 Times per Week",
    "3to4.per.week" = "Exercise 3-4 Times per Week",
    "5to7.per.week" = "Exercise 5-7 Times per Week",
    "8plus.per.week" = "Exercise 8+ Times per Week",
    "Exercise.1to2.per.week_vs_Never" = "Exercise 1-2 Times per Week",
    "Exercise.3to4.per.week_vs_Never" = "Exercise 3-4 Times per Week",
    "Exercise.5to7.per.week_vs_Never" = "Exercise 5-7 Times per Week",
    "Exercise.8plus.per.week_vs_Never" = "Exercise 8+ Times per Week",
    
    # Analysis / Subgroup names
    "Main" = "Main Analysis",
    "Males" = "Men",
    "Females" = "Women",
    "Younger" = "Younger (< 52.6 Years)",
    "Older" = "Older (\u2265 52.6 Years)",
    "European" = "European",
    "Non-European" = "Non-European",
    
    # Exercise subgroup names (for Metformin in exercise subgroups)
    "Exercise: Never" = "Exercise: Never",
    "Exercise: 1-2/week" = "Exercise: 1-2 per Week",
    "Exercise: 3-4/week" = "Exercise: 3-4 per Week", 
    "Exercise: 5-7/week" = "Exercise: 5-7 per Week",
    "Exercise: 8+/week" = "Exercise: 8+ per Week",
    
    # Survey identification variables (for inline text)
    "Hours.Sedentary.Remaining.Awake" = "Hours Sedentary Remaining Awake",
    "Primary.Diet.Past.Year" = "Primary Diet Past Year",
    "Strictly.Followed.Red.Meat.times.per.week" = "Strictly Followed Red Meat Times per Week",
    "Processed.Food.times.per.week" = "Processed Food Times per Week",
    "Feel.Well.Rested.days.per.week" = "Feel Well Rested Days per Week",
    "Screens.before.bed" = "Screens Before Bed",
    "Track.Sleep.Wearable.Device" = "Track Sleep Wearable Device",
    "Height.in" = "Height (in)",
    "Weight" = "Weight",
    "Lab.Fasting" = "Lab Fasting",
    "Anti.Aging.Interventions" = "Anti-Aging Interventions",
    "Given.Birth" = "Given Birth",
    "Menopause" = "Menopause",
    
    # Epigenetic clock names
    "Hannum.PC" = "Hannum PC",
    "Horvath.PC" = "Horvath PC",
    "GrimAge.PC" = "GrimAge PC",
    "PhenoAge.PC" = "PhenoAge PC"
  )
  
  # Check if exact match exists for the original name
  if (name %in% names(name_mapping)) {
    return(unname(name_mapping[name]))
  }
  
  # Check if base name (without suffix) has an exact match
  if (length(suffix_match) > 0 && base_name %in% names(name_mapping)) {
    return(paste0(unname(name_mapping[base_name]), suffix_match))
  }
  
  # Handle categorical variable levels (e.g., "Level.of.Education: Bachelor's")
  # Split by ": " to check if it's a level indicator
  if (grepl(": ", name)) {
    parts <- strsplit(name, ": ", fixed = TRUE)[[1]]
    var_name <- parts[1]
    level_name <- paste(parts[-1], collapse = ": ")  # In case level has ": " in it
    
    # Format the variable name part
    if (var_name %in% names(name_mapping)) {
      formatted_var <- unname(name_mapping[var_name])
    } else {
      # Apply generic formatting
      formatted_var <- gsub("[._]", " ", var_name)
      formatted_var <- tools::toTitleCase(tolower(formatted_var))
    }
    
    return(paste0(formatted_var, ": ", level_name))
  }
  
  # If no exact match, apply generic formatting:
  # Replace dots and underscores with spaces, then title case
  formatted <- gsub("[._]", " ", name)
  formatted <- tools::toTitleCase(tolower(formatted))
  
  # Preserve known abbreviations that should stay uppercase
  abbreviations <- c("BMI", "DHEA", "NAD", "NR", "PC", "DNA", "RNA", "HRT")
  for (abbr in abbreviations) {
    # Case-insensitive replacement back to uppercase
    formatted <- gsub(paste0("\\b", tolower(abbr), "\\b"), abbr, formatted, ignore.case = TRUE)
  }
  
  return(formatted)
}


#' Format all values in a column for display
#'
#' Applies format_display_name to all values in a specified column of a data frame.
#'
#' @param df Data frame
#' @param column_name Name of the column to format
#' @return Data frame with formatted column values
#' @export
format_display_column <- function(df, column_name) {
  if (!column_name %in% names(df)) {
    warning(paste("Column", column_name, "not found in data frame"))
    return(df)
  }
  
  df[[column_name]] <- sapply(df[[column_name]], format_display_name)
  return(df)
}


#' Rename data frame columns for display
#'
#' Applies format_display_name to all column names of a data frame.
#'
#' @param df Data frame
#' @return Data frame with formatted column names
#' @export
rename_columns_for_display <- function(df) {
  names(df) <- sapply(names(df), format_display_name)
  return(df)
}


#' Format a vector of names for display
#'
#' Convenience function to format multiple names at once.
#'
#' @param names Character vector of original variable names
#' @return Character vector of formatted display names
#' @export
format_display_names <- function(names) {
  sapply(names, format_display_name, USE.NAMES = FALSE)
}

# Display-only category labels for figure axes; data and table labels stay intact.
format_figure_category_labels <- function(labels) {
  labels <- as.character(labels)
  categories <- sub("^[^:]*: ", "", labels)
  replacements <- c(
    "Did not complete high school" = "Did Not Complete High School",
    "High school or equivalent" = "High School or Equivalent",
    "Technical or occupational certificate" = "Technical or Occupational Certificate",
    "Some college coursework completed" = "Some College Coursework Completed",
    "Associate degree" = "Associate Degree",
    "Bachelor’s degree" = "Bachelor’s Degree",
    "Master’s degree" = "Master’s Degree",
    "Bachelor's degree" = "Bachelor's Degree",
    "Master's degree" = "Master's Degree",
    "6 hours or less" = "6 Hours or Less",
    "6 to 8 hours" = "6 to 8 Hours",
    "More than 8 hours" = "More Than 8 Hours",
    "On special occasions" = "On Special Occasions",
    "Once per week" = "Once per Week",
    "1-2 times per week" = "1-2 Times per Week",
    "3-4 times per week" = "3-4 Times per Week",
    "3-5 times per week" = "3-5 Times per Week",
    "5-7 times per week" = "5-7 Times per Week",
    "8 or more times per week" = "8 or More Times per Week"
  )
  matched <- categories %in% names(replacements)
  prefix <- substr(labels[matched], 1L, nchar(labels[matched]) - nchar(categories[matched]))
  labels[matched] <- paste0(prefix, unname(replacements[categories[matched]]))
  labels
}
