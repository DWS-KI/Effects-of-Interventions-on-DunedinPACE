# STEP 20: Run independently or as the final step of Master_parallel.R.
# Requires saved outputs from steps 4-6, but no objects in the calling session.
# This preparation step reads individual data; Manuscript.qmd reads only its output.
# Save only explicitly selected aggregate tables and summaries, never the workspace.
local({
  input_files <- c(
    "data_after_step4", "exd_table.rds", "table1_detailed_df.rds",
    "table1_excluded_df", "n_excluded_missing_covariate"
  )
  missing_files <- input_files[!file.exists(here::here("Output", input_files))]
  if (length(missing_files) > 0) {
    stop(
      "Missing metadata inputs in Output/: ", paste(missing_files, collapse = ", "),
      ". Run the relevant preparation steps (4: data/exclusion counts; ",
      "5: detailed Table 1; 6: excluded Table 1/count). No full master run is required."
    )
  }

  # Isolate each saved object so missing inputs cannot fall back to session variables.
  read_saved_object <- function(name) {
    input_env <- new.env(parent = emptyenv())
    loaded <- load(here::here("Output", name), envir = input_env)
    if (!name %in% loaded) stop("Expected object missing from Output/", name)
    input_env[[name]]
  }
  data_after_step4 <- read_saved_object("data_after_step4")
  table1_excluded_df <- read_saved_object("table1_excluded_df")
  n_excluded_missing_covariate <- read_saved_object("n_excluded_missing_covariate")
  exd_table <- readRDS(here::here("Output", "exd_table.rds"))
  table1_detailed_df <- readRDS(here::here("Output", "table1_detailed_df.rds"))

  # The exclusion object can contain participant data in list columns/attributes.
  # Rebuild only the count columns used by the manuscript as a plain data frame.
  exclusion_counts <- data.frame(
    n_left = as.numeric(exd_table$n_left),
    n_right = as.numeric(exd_table$n_right)
  )

  median_age <- median(data_after_step4$Decimal.Chronological.Age, na.rm = TRUE)

  # Preserve the summary definitions previously used in Manuscript.qmd.
  dunedin_stats <- list(
    median = median(data_after_step4$DunedinPACE),
    q1 = quantile(data_after_step4$DunedinPACE, 0.25),
    q3 = quantile(data_after_step4$DunedinPACE, 0.75),
    min = min(data_after_step4$DunedinPACE),
    max = max(data_after_step4$DunedinPACE),
    n_slow = sum(data_after_step4$DunedinPACE < 1),
    n_fast = sum(data_after_step4$DunedinPACE >= 1),
    pct_slow = sum(data_after_step4$DunedinPACE < 1) / nrow(data_after_step4) * 100,
    pct_fast = sum(data_after_step4$DunedinPACE >= 1) / nrow(data_after_step4) * 100
  )

  # Sample sizes for subgroups
  n_male <- sum(data_after_step4$Biological.Sex == "Male")
  n_female <- sum(data_after_step4$Biological.Sex == "Female")
  n_european <- sum(data_after_step4$Ethnicity.Recoded == "Euro", na.rm = TRUE)
  n_noneuropean <- sum(data_after_step4$Ethnicity.Recoded != "Euro", na.rm = TRUE)
  n_missing_ethnicity <- sum(is.na(data_after_step4$Ethnicity.Recoded))
  median_dunedinpace <- median(data_after_step4$DunedinPACE, na.rm = TRUE)
  n_dunedinpace_below_median <- sum(data_after_step4$DunedinPACE < median_dunedinpace, na.rm = TRUE)
  n_dunedinpace_above_median <- sum(data_after_step4$DunedinPACE >= median_dunedinpace, na.rm = TRUE)

  # Additional exclusions if sensitivity covariates were required in main analysis
  n_excluded_main_diet <- sum(!complete.cases(data_after_step4[, "Main.Diet.Factor", drop = FALSE]))
  n_excluded_exercise_type <- sum(!complete.cases(data_after_step4[, "Exercise.Type", drop = FALSE]))
  pct_excluded_main_diet <- n_excluded_main_diet / nrow(data_after_step4) * 100
  pct_excluded_exercise_type <- n_excluded_exercise_type / nrow(data_after_step4) * 100

  manuscript_metadata <- list(
    exd_table = exclusion_counts,
    table1_detailed_df = table1_detailed_df,
    table1_excluded_df = table1_excluded_df,
    n_excluded_missing_covariate = n_excluded_missing_covariate,
    median_age = median_age,
    dunedin_stats = dunedin_stats,
    n_male = n_male,
    n_female = n_female,
    n_european = n_european,
    n_noneuropean = n_noneuropean,
    n_missing_ethnicity = n_missing_ethnicity,
    median_dunedinpace = median_dunedinpace,
    n_dunedinpace_below_median = n_dunedinpace_below_median,
    n_dunedinpace_above_median = n_dunedinpace_above_median,
    n_excluded_main_diet = n_excluded_main_diet,
    n_excluded_exercise_type = n_excluded_exercise_type,
    pct_excluded_main_diet = pct_excluded_main_diet,
    pct_excluded_exercise_type = pct_excluded_exercise_type
  )

  saveRDS(manuscript_metadata, here::here("Output", "manuscript_metadata.rds"))
  message("Saved de-identified manuscript metadata to Output/manuscript_metadata.rds")
})
