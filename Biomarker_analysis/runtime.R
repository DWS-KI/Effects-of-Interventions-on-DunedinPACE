# Runtime adapter. Original R files are copied byte-for-byte into an isolated
# project; source() evaluates adapted expressions in memory. Nothing is patched
# on disk. The adapter is scoped to a fresh callr process for each biomarker.
rewrite_biomarker_expression <- function(x, file, label, n_bootstrap, analysis_steps = c(7L, 8L, 10L), auxiliary = FALSE) {
  master <- basename(file) == "Master_parallel.R"
  file_name <- basename(file)
  if (is.call(x)) {
    # Preparation already ran step 1; preserve each biomarker's standardized input.
    # Suppress only the master's call, leaving the preparation worker unchanged.
    if (master && identical(x, quote(source(here::here("Program", "Set_survey_versions_step1.R")))))
      return(quote(message("Skipped by biomarker wrapper: Set_survey_versions_step1.R (already completed during preparation).")))
    # Omit the master's step-20 prerequisite guard when metadata is disabled.
    if (master && !20L %in% analysis_steps && identical(x[[1]], as.name("if")) &&
        any(grepl("Step 6 must succeed before manuscript metadata", deparse(x), fixed = TRUE)))
      return(quote(invisible(NULL)))
    # Suppress incidental graphics, without changing exclusions or estimators.
    if (!auxiliary) {
      head <- paste(deparse(x[[1]]), collapse = "")
      if (head == "create_love_plot" ||
          (file_name == "Make_flowchart_and_exclusions_step4.R" &&
           head %in% c("exclusion_flowchart", "DiagrammeRsvg::export_svg", "rsvg::rsvg_png")))
        return(quote(invisible(NULL)))
    }
    if (master && identical(x[[1]], as.name("<-")) && is.symbol(x[[2]])) {
      key <- as.character(x[[2]])
      overrides <- list(n_bootstrap = n_bootstrap, n_workers = 1L)
      if (key %in% names(overrides)) x[[3]] <- overrides[[key]]
      if (key == "parallel_steps") x[[3]] <- substitute(
        Filter(function(info) info$step %in% selected, original),
        list(selected = analysis_steps, original = x[[3]]))
    }
    # Residuals can be negative: only missing/nonfinite outcomes are excluded.
    if (identical(x, quote(DunedinPACE > 0))) return(quote(is.finite(DunedinPACE)))
    # Table 1 and aggregate metadata split at the reference mean (z = 0).
    if (as.character(x[[1]])[1] %in% c("<", ">=") && length(x) == 3L &&
        identical(x[[3]], 1) && grepl("DunedinPACE$", paste(deparse(x[[2]]), collapse = ""))) x[[3]] <- 0
    # Existing forest/table code reports ATE * 100 as %. Standardized effects are SD.
    if (file_name %in% c("forest_plot_functions.R", "Metformin_table_and_figures_step18.R") &&
        identical(x[[1]], as.name("*")) && length(x) == 3L && identical(x[[3]], 100))
      return(rewrite_biomarker_expression(x[[2]], file, label, n_bootstrap, analysis_steps, auxiliary))
    # Dependencies are checked before execution; suppress unconditional reinstall in step 1.
    if (identical(x[[1]], as.name("install.packages"))) return(quote(invisible(NULL)))
  }
  if (is.character(x)) {
    x <- gsub("Excluded: Biologically implausible zero or negative DunedinPACE",
              "Excluded: Missing or invalid DunedinPACE", x, fixed = TRUE)
    x <- gsub("DunedinPACE < 1", "DunedinPACE z < 0", x, fixed = TRUE)
    x <- gsub("DunedinPACE ≥ 1", "DunedinPACE z >= 0", x, fixed = TRUE)
    x <- gsub("DunedinPACE >= 1", "DunedinPACE z >= 0", x, fixed = TRUE)
    x <- gsub("DunedinPACE (ATE, %)", "DunedinPACE (ATE, SD)", x, fixed = TRUE)
    # Preserve exact engine names and file names until the pipeline has completed.
    display <- grepl("DunedinPACE", x, fixed = TRUE) & grepl(" ", x, fixed = TRUE)
    x[display] <- gsub("DunedinPACE", label, x[display], fixed = TRUE)
    if (file_name == "Metformin_table_and_figures_step18.R") x <- gsub("%%", "", x, fixed = TRUE)
    return(x)
  }
  if (is.call(x) || is.expression(x) || is.pairlist(x)) {
    for (i in seq_along(x)) {
      # Missing function arguments must stay missing (e.g. df[, cols]).
      if (is.null(x[[i]]) || identical(x[[i]], quote(expr = ))) next
      x[[i]] <- rewrite_biomarker_expression(x[[i]], file, label, n_bootstrap, analysis_steps, auxiliary)
    }
  }
  x
}

install_biomarker_source <- function(label, n_bootstrap, log_dir, analysis_steps = c(7L, 8L, 10L), auxiliary = FALSE) {
  adapter <- new.env(parent = globalenv())
  adapter$analysis_steps <- analysis_steps
  adapter$auxiliary <- auxiliary
  adapter$label <- label
  adapter$n_bootstrap <- n_bootstrap
  adapter$log_dir <- log_dir
  adapter$rewrite <- rewrite_biomarker_expression
  adapter$step_status <- data.frame(script = character(), success = logical(), seconds = numeric(), error = character())
  fun <- function(file, local = FALSE, ...) {
    # Step 4b describes the primary cohort using original PACE, not biomarker z-scores.
    # Its letter suffix also falls outside the numbered-step selection below.
    if (identical(basename(file), "Table1_afflictions_step4b.R")) {
      message("Skipped by biomarker wrapper: ", basename(file),
              " (table for the primary DunedinPACE analysis).")
      return(invisible(NULL))
    }
    target <- if (is.environment(local)) local else if (isTRUE(local)) parent.frame() else globalenv()
    top_step <- grepl("step[0-9]+\\.R$", basename(file), ignore.case = TRUE)
    number <- if (top_step) as.integer(sub(".*step([0-9]+)\\.R$", "\\1", basename(file), ignore.case = TRUE)) else NA_integer_
    if (top_step && number >= 5L && !number %in% analysis_steps) {
      message("Skipped by wrapper configuration: ", basename(file))
      return(invisible(NULL))
    }
    expressions <- parse(file, keep.source = FALSE)
    expressions <- rewrite(expressions, file, label, n_bootstrap, analysis_steps, auxiliary)
    started <- Sys.time()
    top_step <- grepl("step[0-9]+\\.R$", basename(file), ignore.case = TRUE)
    error <- ""
    value <- tryCatch({
      result <- NULL
      for (expression in expressions) result <- withVisible(eval(expression, envir = target))
      result
    }, error = function(e) { error <<- conditionMessage(e); NULL })
    if (top_step) {
      step_status <<- rbind(step_status, data.frame(script = basename(file), success = !nzchar(error),
        seconds = as.numeric(difftime(Sys.time(), started, units = "secs")), error = error))
      write.csv(step_status, file.path(log_dir, "steps.csv"), row.names = FALSE)
    }
    if (nzchar(error)) stop(basename(file), ": ", error, call. = FALSE)
    invisible(value)
  }
  environment(fun) <- adapter
  assign("source", fun, envir = globalenv())
  # Both calls in the unchanged master resolve to this sequential-only adapter.
  assign("plan", function(...) future::plan(future::sequential), envir = globalenv())
  assign("future_lapply", function(X, FUN, ..., future.seed = NULL) lapply(X, FUN, ...), envir = globalenv())
  invisible(adapter)
}

prepare_biomarker_project <- function(root, project) {
  dir.create(project, recursive = TRUE, showWarnings = FALSE)
  writeLines(character(), file.path(project, ".here"))
  file.copy(file.path(root, "biological_calendar_aging.Rproj"), project)
  file.copy(file.path(root, "Master_parallel.R"), project)
  # Only executable R sources are needed, not legacy figures/documentation.
  scripts <- list.files(file.path(root, "Program"), pattern = "\\.R$", recursive = TRUE)
  for (script in scripts) {
    destination <- file.path(project, "Program", script)
    dir.create(dirname(destination), recursive = TRUE, showWarnings = FALSE)
    stopifnot(file.copy(file.path(root, "Program", script), destination))
  }
  dir.create(file.path(project, "Output"))
  invisible(project)
}

biomarker_worker <- function(root, project, label, n_bootstrap, seed, mode = "analysis", analysis_steps = c(7L, 8L, 10L), auxiliary = FALSE) {
  setwd(project)
  base::source(file.path(root, "Biomarker_analysis", "runtime.R"), local = globalenv())
  base::source(file.path(root, "Biomarker_analysis", "steps.R"), local = globalenv())
  options(n_bootstrap = n_bootstrap, repos = c(CRAN = "https://cloud.r-project.org"), warn = 1)
  set.seed(seed)
  dir.create("logs", showWarnings = FALSE)
  options(device = function(...) grDevices::pdf(file.path(project, "logs", "graphics_device.pdf")))
  adapter <- install_biomarker_source(label, n_bootstrap, file.path(project, "logs"), analysis_steps, auxiliary)
  # here must be initialized after setting the isolated working directory.
  stopifnot(normalizePath(here::here()) == normalizePath(project))
  if (mode == "prepare") {
    for (script in c("Set_survey_versions_step1.R", "Set_datatypes_step2.R", "Reduce_dimensions_step3.R"))
      source(file.path("Program", script))
  } else {
    source("Master_parallel.R")
    if (exists("failed_steps", globalenv()) && get("failed_steps", globalenv()) != 0L)
      stop("Master reported failed analysis steps.")
    validate_step_log(file.path(project, "logs", "steps.csv"), analysis_spec(analysis_steps, auxiliary))
  }
  grDevices::graphics.off()
  writeLines(capture.output(sessionInfo()), "logs/sessionInfo.txt")
  invisible(TRUE)
}
