#' Get exclusion criteria for study population
#'
#' This function returns a tibble with exclusion criteria that can be used
#' with khsmisc::make_exclusions() to generate flowcharts and filter data.
#' This ensures consistency between flowchart visualization and data filtering.
#'
#' @return A tibble with columns: left (label after previous exclusion), 
#'   right (label of excluded group), and filter (expression for inclusion)
#' @export
#'
#' @examples
#' criteria <- get_exclusion_criteria()
#' exd <- khsmisc::make_exclusions(criteria = criteria, data = my_data)
#' filtered_data <- exd$data  # Extract the filtered dataset
get_exclusion_criteria <- function() {
  
  # --- Define combined filters here ---
  
  # Combine metabolic disease exclusions (Diabetes, Gestational, PCOS, Prediabetes)
  filter_metabolic <- rlang::expr(
    (Diabetes2 == 0 | is.na(Diabetes2)) & 
    (Gestational.Diabetes == 0 | is.na(Gestational.Diabetes)) &
    (Polycystic.Ovary == 0 | is.na(Polycystic.Ovary)) &
    (Prediabetes == 0 | is.na(Prediabetes))
  )
  
  # Combine age exclusions (<18 and >110)
  filter_age <- rlang::expr(Decimal.Chronological.Age >= 18 & Decimal.Chronological.Age <= 110)
  
  # --- Create the criteria table ---
  
  criteria <- tibble::tribble(
    ~left,                                    ~right,                                  ~filter,
    "Survey Version 1 Only",                  "Excluded: EPICv2 Arrays",               rlang::expr(!is.na(Array) & Array == "EPICv1"),
    "EPICv1 Array",                           "Excluded: Missing or Other Biological Sex", rlang::expr(!is.na(Biological.Sex) & Biological.Sex != "Other"),
    "Valid Biological Sex",                   "Excluded: Indications for Metformin (PCOS, Type 2 Diabetes, Gestational Diabetes, or Prediabetes)", filter_metabolic,
    "No Metformin Indication",                "Excluded: Age < 18 or > 110 Years",     filter_age,
    "Age 18-110 Years",                       "Excluded: Biologically Implausible Zero or Negative DunedinPACE", rlang::expr(!is.na(DunedinPACE) & DunedinPACE > 0),
    "Valid Epigenetic Clock Values",          "Excluded: Extreme Underweight (BMI < 15)", rlang::expr(is.na(BMI) | BMI >= 15),
    "Individuals Without Extremely Low BMI",  "Excluded: Missing Data for >= 1 Model Covariate", rlang::expr(!is.na(Decimal.Chronological.Age) &
                                                                                            !is.na(Biological.Sex) &
                                                                                            !is.na(Level.of.Education) &
                                                                                            !is.na(Tobacco.Use.Binary) &
                                                                                            !is.na(Alcohol.per.week) &
                                                                                            !is.na(Exercise.per.week) &
                                                                                            !is.na(BMI) &
                                                                                            !is.na(Stress.Level) &
                                                                                            !is.na(Hours.of.sleep.per.night)),
    "Complete Covariate Data",                "",                                      rlang::expr(TRUE)  # final row
  )
  
  return(criteria)
}

