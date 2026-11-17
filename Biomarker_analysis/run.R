# Run from the repository root. No existing pipeline or input files are changed.
# Run from terminal with:
# Rscript --vanilla Biomarker_analysis/run.R

main <- function() {
  if (!file.exists("biological_calendar_aging.Rproj")) stop("Run from biological_calendar_aging/.")
  root <- normalizePath(getwd())
  args <- commandArgs(trailingOnly = TRUE)
  config_file <- if (length(args)) args[1] else "Biomarker_analysis/config.R"
  if (length(args) > 1L) stop("Usage: Rscript Biomarker_analysis/run.R [config.R]")
  source(config_file)
  config <- biomarker_config
  stopifnot(config$data_kind %in% c("mock", "real"), length(config$n_bootstrap) == 1L,
            is.finite(config$n_bootstrap), config$n_bootstrap >= 2L,
            config$n_bootstrap == as.integer(config$n_bootstrap))
  source("Biomarker_analysis/residuals.R")
  source("Biomarker_analysis/runtime.R")
  source("Biomarker_analysis/results.R")
  source("Biomarker_analysis/parallel.R")
  source("Biomarker_analysis/compare_heatmap_clusters.R")
  # Reuse installed repo-local packages, without changing the old dependency helper.
  local_lib <- file.path(root, "Output", "mockdata", "library")
  if (dir.exists(local_lib)) .libPaths(c(local_lib, .libPaths()))
  required <- c("callr", "parallelly", "pacman", "here", "tidyverse", "lubridate", "DiagrammeR", "DiagrammeRsvg",
    "rsvg", "UpSetR", "patchwork", "png", "future", "future.apply", "khsmisc", "tableone", "writexl", "readxl")
  missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Install required packages first (see README): ", paste(missing, collapse = ", "))
  raw <- read.csv(config$input_file, stringsAsFactors = FALSE)
  if (config$data_kind == "mock" && (!"Patient.ID" %in% names(raw) || !all(grepl("^MOCK-", raw$Patient.ID))))
    stop("data_kind='mock' requires MOCK- IDs. Set data_kind='real' for your local data.")
  if ("DunedinPoAm" %in% names(raw) && !"DunedinPACE" %in% names(raw))
    names(raw)[names(raw) == "DunedinPoAm"] <- "DunedinPACE"
  if (!"Patient.ID" %in% names(raw)) stop("Missing Patient.ID column.")
  if (anyDuplicated(raw$Patient.ID) || anyNA(raw$Patient.ID)) stop("Patient.ID must be unique/nonmissing for cross-sectional data.")
  catalog <- resolve_biomarker_selection(config, names(raw))
  biomarkers <- catalog$biomarker
  config$n_workers <- biomarker_worker_count(length(biomarkers), config$n_workers)
  run_id <- paste0(format(Sys.time(), "%Y%m%dT%H%M%S"), "_", Sys.getpid())
  run <- file.path(root, config$output_dir, run_id)
  dir.create(run, recursive = TRUE)
  config$input_file <- normalizePath(config$input_file)
  saveRDS(config, file.path(run, "config.rds"))
  write.csv(catalog, file.path(run, "biomarker_selection.csv"), row.names = FALSE)
  source_files <- c("Master_parallel.R", list.files("Program", pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
  hashes <- tools::md5sum(source_files)
  write.csv(data.frame(file = source_files, md5 = unname(hashes)), file.path(run, "source_hashes.csv"), row.names = FALSE)
  input_hash <- tools::md5sum(config$input_file)
  on.exit({
    stopifnot(identical(hashes, tools::md5sum(source_files)), identical(input_hash, tools::md5sum(config$input_file)))
  }, add = TRUE)
  message("Run directory: ", run)
  message("Biomarker workers: ", config$n_workers, " (available cores: ",
    parallelly::availableCores(omit = 0L), "; reserve one core, minimum one worker).")
  message("Main analyses only: steps 2-4, 7, 8 and 10 per biomarker; auxiliary plots disabled.")
  prep <- file.path(run, "preparation")
  prepare_biomarker_project(root, prep)
  raw_dir <- file.path(prep, "Data", "EPICv1 and v2 data")
  dir.create(raw_dir, recursive = TRUE)
  stopifnot(file.copy(config$input_file, file.path(raw_dir, "All_metrics_crossectional_samples.csv")))
  without_process_clone_message(callr::r(biomarker_worker, args = list(root, prep, "Preparation", config$n_bootstrap, config$seed, "prepare"),
    libpath = .libPaths(), wd = prep, stdout = file.path(prep, "prepare.log"), stderr = "2>&1",
    user_profile = FALSE, system_profile = FALSE, env = biomarker_process_env()))
  step1 <- read.csv(file.path(prep, "Output", "data_after_step1.csv"), stringsAsFactors = FALSE)
  step3 <- read_pipeline_object(file.path(prep, "Output", "data_after_step3"))
  stopifnot(identical(step1$Patient.ID, step3$Patient.ID))
  source("Program/Functions/get_exclusion_criteria.R")
  criteria <- get_exclusion_criteria()
  eligible <- rep(TRUE, nrow(step3))
  for (expression in criteria$filter) {
    # Biomarker-specific validity is applied separately by add_age_residuals().
    if ("DunedinPACE" %in% all.vars(expression)) next
    valid <- rlang::eval_tidy(expression, data = step3)
    eligible <- eligible & !is.na(valid) & valid
  }
  reference_sd <- NULL
  if (!is.null(config$reference_sd_file)) {
    ref <- read.csv(config$reference_sd_file)
    stopifnot(all(c("biomarker", "residual_sd") %in% names(ref)), !anyDuplicated(ref$biomarker))
    reference_sd <- setNames(ref$residual_sd, ref$biomarker)
  }
  prepared <- add_age_residuals(step1, biomarkers, eligible, config$residual_method, config$spline_df,
    reference_sd, catalog$biomarker[catalog$positive_raw], catalog$biomarker[catalog$age_in_years])
  # Retain all original raw rows/columns, including surveys later excluded by step 1.
  # Derived values are NA outside survey 1, since those rows do not enter this pipeline.
  full <- raw
  new_columns <- setdiff(names(prepared$data), names(step1))
  for (column in new_columns) full[[column]] <- prepared$data[[column]][match(raw$Patient.ID, prepared$data$Patient.ID)]
  saveRDS(full, file.path(run, "data_with_residuals.rds"))
  write.csv(full, file.path(run, "data_with_residuals.csv"), row.names = FALSE)
  write.csv(prepared$metadata, file.path(run, "residual_metadata.csv"), row.names = FALSE)
  saveRDS(prepared$models, file.path(run, "residual_models.rds"))
  start_job <- function(i) {
    biomarker <- biomarkers[i]
    message(sprintf("[%d/%d] %s", i, length(biomarkers), biomarker))
    project <- file.path(run, biomarker)
    prepare_biomarker_project(root, project)
    meta <- prepared$metadata[prepared$metadata$biomarker == biomarker, ]
    engine_data <- prepared$data
    engine_data$Original.DunedinPACE <- engine_data$DunedinPACE
    engine_data$DunedinPACE <- engine_data[[meta$z_column]]
    write.csv(engine_data, file.path(project, "Output", "data_after_step1.csv"), row.names = FALSE)
    without_process_clone_message(callr::r_bg(biomarker_worker, args = list(root, project, biomarker, config$n_bootstrap, config$seed, "analysis"),
      libpath = .libPaths(), wd = project, stdout = file.path(project, "master.log"), stderr = "2>&1",
      user_profile = FALSE, system_profile = FALSE, env = biomarker_process_env(), supervise = TRUE))
  }
  collect_job <- function(i) {
    biomarker <- biomarkers[i]
    project <- file.path(run, biomarker)
    meta <- prepared$metadata[prepared$metadata$biomarker == biomarker, ]
    log <- readLines(file.path(project, "master.log"), warn = FALSE)
    caught <- grep("ERROR in |Skipping this subgroup|Could not create Love plot", log, value = TRUE)
    if (length(caught)) stop(paste(caught, collapse = "\n"))
    z <- setNames(prepared$data[[meta$z_column]], prepared$data$Patient.ID)
    ids <- prepared$data$Patient.ID[eligible & is.finite(z)]
    x <- validate_biomarker_outputs(file.path(project, "Output"), config$n_bootstrap, z, ids)
    rows <- attr(x, "validated_rows")
    x$outcome <- x$biomarker <- biomarker
    x$outcome_column <- meta$z_column
    x$residualized <- meta$residualized
    x$residual_method <- meta$method
    x$scaling_sd <- meta$scaling_sd
    x$scaling_source <- meta$scaling_source
    x$n_reference <- meta$n_reference
    x$n_bootstrap_requested <- config$n_bootstrap
    x$data_kind <- config$data_kind
    x$units <- "SD"
    export_biomarker_names(file.path(project, "Output"), biomarker, meta$z_column)
    saveRDS(x, file.path(project, "main_results.rds"))
    write.csv(x, file.path(project, "main_results.csv"), row.names = FALSE)
    # Full method-specific tables and exercise contrasts are preserved separately.
    writeLines("Steps 2-4, 7, 8 and 10 and all 44 main-analysis rows validated.", file.path(project, "SUCCESS.txt"))
    message("Validated: ", biomarker, " (", rows, " rows)")
    x
  }
  checkpoint <- function(all_results, status) {
    write.csv(status, file.path(run, "status.csv"), row.names = FALSE)
    if (length(all_results)) {
      combined <- dplyr::bind_rows(all_results)
      saveRDS(combined, file.path(run, "main_results.rds"))
      write.csv(combined, file.path(run, "main_results.csv"), row.names = FALSE)
    }
  }
  completed <- tryCatch(run_biomarker_queue(biomarkers, config$n_workers, start_job, collect_job, checkpoint),
    error = function(e) stop(conditionMessage(e), "\nSee status.csv and biomarker master.log files in ", run, call. = FALSE))
  combined <- dplyr::bind_rows(completed$results)
  status <- completed$status
  stopifnot(nrow(combined) == length(biomarkers) * 44L)
  # Correlate original values with age in the same biomarker-specific analysis cohort.
  age_correlations <- do.call(rbind, lapply(biomarkers, function(biomarker) {
    meta <- prepared$metadata[prepared$metadata$biomarker == biomarker, ]
    values <- step1[[biomarker]]
    age <- step1$Decimal.Chronological.Age
    rows <- eligible & is.finite(prepared$data[[meta$z_column]]) & is.finite(values) & is.finite(age)
    values <- values[rows]
    age <- age[rows]
    defined <- length(values) >= 2L && length(unique(values)) > 1L && length(unique(age)) > 1L
    data.frame(biomarker = biomarker,
      pearson_r = if (defined) stats::cor(values, age, method = "pearson") else NA_real_,
      spearman_rho = if (defined) stats::cor(values, age, method = "spearman") else NA_real_)
  }))
  plot_biomarker_heatmaps(combined, file.path(run, "heatmaps"), config, age_correlations = age_correlations)
  # Check source/input integrity before replacing the manuscript's stable outputs.
  stopifnot(identical(hashes, tools::md5sum(source_files)), identical(input_hash, tools::md5sum(config$input_file)))
  success_lines <- c("SUCCESS", paste("Biomarkers:", length(biomarkers)),
    paste("Main result rows:", nrow(combined)), paste("All validated result rows:", sum(status$validated_rows)),
    paste("Bootstrap iterations:", config$n_bootstrap))
  current <- publish_biomarker_outputs(run, config, success_lines)
  message("Manuscript figures and supplementary data: ", current)
  comparison <- compare_heatmap_clusters(current_dir = current)
  comparison_files <- c(comparison$report, comparison$workbook)
  copied <- file.copy(comparison_files, file.path(current, basename(comparison_files)), overwrite = TRUE)
  if (!all(copied)) stop("Could not copy cluster comparison reports to ", current, ": ",
    paste(basename(comparison_files[!copied]), collapse = ", "), call. = FALSE)
  message("Latest cluster comparison reports: ", current)
  message("Complete: ", run)
}
if (sys.nframe() == 0L) main()
