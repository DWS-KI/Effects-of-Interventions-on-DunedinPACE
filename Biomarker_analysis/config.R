# Run from biological_calendar_aging: Rscript --vanilla Biomarker_analysis/run.R
biomarker_config <- list(
  input_file = "Data/EPICv1 and v2 data/All_metrics_crossectional_samples.csv",
  data_kind = "real",               # Change to "real" and set input_file locally.
  n_bootstrap = 2L,                  # Change to 1000L for the final analysis.
  residual_method = "linear",       # "linear", "quadratic", "spline"
  spline_df = 3L,
  reference_sd_file = NULL,          # Optional CSV: biomarker,residual_sd; SD of the residual or raw measure.
  biomarker_set = "original",       # Fallback only when biomarkers = NULL.
  # EDIT THIS LIST: delete/comment out any unwanted row; add new columns here.
  # Names must match the R column names in your local input CSV.
  # This is NOT a verified inventory of your current private CSV.
  # "age_years": age residual -> z-score; raw value must be > 0.
  # "raw": direct z-score, NO age residualization.
  # "positive_raw": direct z-score, NO residualization; raw must be > 0.
  # Named entries override biomarker_set. New columns need no edits elsewhere.
  # Keep commas between active rows; the last active row must have no trailing comma.
  biomarkers = c(
    # Biological age in years (23):
    # "Extrinsic.Age" = "age_years",
    # "Intrinsic.Age" = "age_years",
    "Hannum.PC" = "age_years",
    "Horvath.PC" = "age_years",
    "GrimAge.PC" = "age_years",
    "PhenoAge.PC" = "age_years",
    "OMICmAge" = "age_years",
    "DNAmFitAge" = "age_years",
    "SystemsAge.Blood" = "age_years",
    "SystemsAge.Brain" = "age_years",
    "SystemsAge.Inflammation" = "age_years",
    "SystemsAge.Heart" = "age_years",
    "SystemsAge.Hormone" = "age_years",
    "SystemsAge.Immune" = "age_years",
    "SystemsAge.Kidney" = "age_years",
    "SystemsAge.Liver" = "age_years",
    "SystemsAge.Metabolic" = "age_years",
    "SystemsAge.Lung" = "age_years",
    "SystemsAge.MusculoSkeletal" = "age_years",
    "SystemsAge" = "age_years",
    # Other original measures (11):
    "DunedinPACE" = "positive_raw",
    # "Telomere.Values" = "positive_raw",
    "Telomere.Values.PC" = "age_years",
   # "Full_CumulativeStemCellDivisions" = "raw",
  #  "Avg_LifeTime_IntrinsicStemCellDivisionRate" = "raw",
   # "Median_Lifetime_IntrinsicStemCellDivisionRate" = "raw",
    "DNAmVO2max" = "age_years"

    # Optional existing derived measures (31); excluded by the earlier method choice.
    # Uncomment a row INCLUDING its leading comma to include it. Do not residualize again.
    # , "IEAA_PC" = "raw"
    # , "EEAA_PC" = "raw"
    # , "Intrinsic_IEAA" = "raw"
    # , "Extrinsic_IEAA" = "raw"
    # , "Extrinsic_EEAA" = "raw"
    # , "FitAgeAccel" = "raw"
    # , "SystemsAge.Blood.Residuals" = "raw"
    # , "SystemsAge.Brain.Residuals" = "raw"
    # , "SystemsAge.Inflammation.Residuals" = "raw"
    # , "SystemsAge.Heart.Residuals" = "raw"
    # , "SystemsAge.Hormone.Residuals" = "raw"
    # , "SystemsAge.Immune.Residuals" = "raw"
    # , "SystemsAge.Kidney.Residuals" = "raw"
    # , "SystemsAge.Liver.Residuals" = "raw"
    # , "SystemsAge.Metabolic.Residuals" = "raw"
    # , "SystemsAge.Lung.Residuals" = "raw"
    # , "SystemsAge.MusculoSkeletal.Residuals" = "raw"
    # , "SystemsAge.Residuals" = "raw"
    # , "SystemsAge.Blood.Percentile" = "raw"
    # , "SystemsAge.Brain.Percentile" = "raw"
    # , "SystemsAge.Inflammation.Percentile" = "raw"
    # , "SystemsAge.Heart.Percentile" = "raw"
    # , "SystemsAge.Hormone.Percentile" = "raw"
    # , "SystemsAge.Immune.Percentile" = "raw"
    # , "SystemsAge.Kidney.Percentile" = "raw"
    # , "SystemsAge.Liver.Percentile" = "raw"
    # , "SystemsAge.Metabolic.Percentile" = "raw"
    # , "SystemsAge.Lung.Percentile" = "raw"
    # , "SystemsAge.MusculoSkeletal.Percentile" = "raw"
    # , "SystemsAge.Percentile" = "raw"
    # , "DunedinPACE.Percentile" = "raw"
  ),
  seed = 20260920L,
  n_workers = NULL,                 # Auto: available cores - 1 (at least 1). An integer limits this further.
  output_dir = "Output/biomarker_analysis",
  heatmap_limit = 1,
  include_exercise_in_heatmap = TRUE
)
