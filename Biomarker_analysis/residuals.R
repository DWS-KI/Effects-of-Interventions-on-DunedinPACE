# Standalone residual calculation; no pipeline files are rewritten.
biomarker_catalog <- function() {
  clocks <- c("Extrinsic.Age", "Intrinsic.Age", "Hannum.PC", "Horvath.PC",
    "GrimAge.PC", "PhenoAge.PC", "OMICmAge", "OMICmAge_cor", "DNAmFitAge",
    "OMICmAge_cor_v2", "DNAmFitAge_cor_v2", paste0("SystemsAge.", c("Blood",
    "Brain", "Inflammation", "Heart", "Hormone", "Immune", "Kidney", "Liver",
    "Metabolic", "Lung", "MusculoSkeletal")), "SystemsAge", "DunedinPACE")
  other <- c("Telomere.Values", "Telomere.Values.PC", "Full_CumulativeStemCellDivisions",
    "Avg_LifeTime_IntrinsicStemCellDivisionRate", "Median_Lifetime_IntrinsicStemCellDivisionRate",
    "MitoticScore_OldEpitoc", "HypoClock_Score_Old", "DNAmGait_noAge", "DNAmGrip_noAge", "DNAmVO2max")
  derived <- c("IEAA_PC", "EEAA_PC", "Intrinsic_IEAA", "Extrinsic_IEAA", "Extrinsic_EEAA",
    "FitAgeAccel", paste0(clocks[grepl("^SystemsAge", clocks)], ".Residuals"),
    paste0(clocks[grepl("^SystemsAge", clocks)], ".Percentile"), "DunedinPACE.Percentile")
  data.frame(biomarker = c(clocks, other, derived),
    group = rep(c("clocks", "other", "derived"), c(length(clocks), length(other), length(derived))),
    positive_raw = c(rep(TRUE, length(clocks)), rep(FALSE, length(other) + length(derived))),
    age_in_years = c(clocks != "DunedinPACE", rep(FALSE, length(other) + length(derived))))
}

# Named config entries specify selection AND units, including private-data columns
# absent from the mock schema. Old unnamed lists and biomarker_set still work.
resolve_biomarker_selection <- function(config, columns) {
  catalog <- biomarker_catalog()
  requested <- config$biomarkers
  if (is.null(requested)) {
    selected <- switch(config$biomarker_set,
      clocks = catalog$group == "clocks", original = catalog$group != "derived",
      all = rep(TRUE, nrow(catalog)), stop("Unknown biomarker_set"))
    result <- catalog[selected, ]
  } else if (!is.null(names(requested))) {
    if (!is.character(requested) || anyNA(requested) || anyNA(names(requested)) ||
        any(!nzchar(names(requested))) || anyDuplicated(names(requested)))
      stop("biomarkers must have unique, nonempty column names and transformation values.")
    allowed <- c("age_years", "raw", "positive_raw")
    if (any(!requested %in% allowed))
      stop("Unknown biomarker transformation. Use age_years, raw or positive_raw.")
    result <- data.frame(biomarker = names(requested), group = "configured",
      positive_raw = unname(requested %in% c("age_years", "positive_raw")),
      age_in_years = unname(requested == "age_years"))
  } else {
    if (!is.character(requested) || anyNA(requested) || anyDuplicated(requested))
      stop("biomarkers must be a character vector of unique column names.")
    unknown <- setdiff(requested, catalog$biomarker)
    if (length(unknown)) stop("Specify units in config.R for new columns, e.g. ",
      'biomarkers = c("MyClock" = "age_years", "MyOtherMeasure" = "raw"): ',
      paste(unknown, collapse = ", "))
    result <- catalog[match(requested, catalog$biomarker), ]
  }
  if (!nrow(result)) stop("Select at least one biomarker in config.R.")
  if (any(result$biomarker %in% c(".", "..")) || any(grepl("[/\\\\]", result$biomarker)))
    stop("Biomarker column names cannot contain path separators.")
  missing <- setdiff(result$biomarker, columns)
  if (length(missing)) stop("Missing selected biomarker columns in input CSV: ",
    paste(missing, collapse = ", "), ". Edit biomarkers in config.R to match your file.")
  rownames(result) <- NULL
  result
}

# Fit and SD reference use the same eligible main cohort, separately per biomarker.
# Predictions are also stored for other rows with valid age/biomarker, preserving IDs/NA.
add_age_residuals <- function(data, biomarkers, eligible, method = "linear", spline_df = 3L,
                              reference_sd = NULL, positive_raw = character(), age_in_years = biomarkers) {
  stopifnot(is.data.frame(data), is.logical(eligible), length(eligible) == nrow(data),
            !anyNA(eligible), length(biomarkers) > 0, !anyDuplicated(biomarkers))
  if (!method %in% c("linear", "quadratic", "spline")) stop("Unknown residual method: ", method)
  age <- data$Decimal.Chronological.Age
  if (!is.numeric(age)) stop("Chronological age must be numeric.")
  metadata <- models <- list()
  for (biomarker in biomarkers) {
    y <- data[[biomarker]]
    if (!is.numeric(y)) stop("Biomarker must be numeric: ", biomarker)
    valid <- is.finite(y) & is.finite(age) & age >= 18 & age <= 110
    if (biomarker %in% positive_raw) valid <- valid & y > 0
    fit_rows <- eligible & valid
    residualize <- biomarker %in% age_in_years
    if (sum(fit_rows) < 10L || (residualize && length(unique(age[fit_rows])) < 5L))
      stop("Insufficient reference observations/ages for ", biomarker)
    formula <- switch(method, linear = y ~ age, quadratic = y ~ age + I(age^2),
                      spline = stats::as.formula(sprintf("y ~ splines::ns(age, df = %d)", spline_df)))
    fit <- if (residualize) stats::lm(formula, data = data.frame(y = y[fit_rows], age = age[fit_rows])) else NULL
    if (anyNA(stats::coef(fit))) stop("Rank-deficient residual model: ", biomarker)
    residual <- rep(NA_real_, nrow(data))
    residual[valid] <- if (residualize) y[valid] - stats::predict(fit, newdata = data.frame(age = age[valid])) else y[valid]
    center <- mean(residual[fit_rows])
    cohort_sd <- stats::sd(residual[fit_rows])
    divisor <- if (is.null(reference_sd)) cohort_sd else unname(reference_sd[biomarker])
    if (length(divisor) != 1L || !is.finite(divisor) || divisor <= sqrt(.Machine$double.eps))
      stop("Missing/zero residual SD for ", biomarker)
    residual_col <- if (residualize) paste0(biomarker, ".AgeResidual") else NA_character_
    z_col <- paste0(biomarker, if (residualize) ".AgeResidualZ" else ".Z")
    if (any(c(residual_col, z_col) %in% names(data))) stop("Output columns already exist: ", biomarker)
    if (residualize) data[[residual_col]] <- residual
    data[[z_col]] <- (residual - center) / divisor
    metadata[[biomarker]] <- data.frame(biomarker, method = if (residualize) method else "raw_z", residualized = residualize, n_reference = sum(fit_rows),
      n_valid = sum(valid), centering_mean = center, cohort_sd = cohort_sd,
      scaling_sd = divisor, scaling_source = if (is.null(reference_sd)) "analysis_cohort" else "external_reference",
      residual_column = residual_col, z_column = z_col,
      z_reference_mean = mean(data[[z_col]][fit_rows]), z_reference_sd = stats::sd(data[[z_col]][fit_rows]))
    models[biomarker] <- list(fit)
  }
  list(data = data, metadata = do.call(rbind, metadata), models = models)
}
