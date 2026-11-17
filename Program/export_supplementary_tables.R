# Quarto hooks for the single supplementary-table workbook.
# This script reads display tables and the rendered DOCX; it never runs analyses.

run_supplementary_export_hook <- function(mode, root = normalizePath(".", winslash = "/")) {
  source(file.path(root, "Program", "Functions", "supplementary_table_export.R"), local = TRUE)
  paths <- supplementary_export_paths(root)
  if (!mode %in% c("prepare", "finalize")) {
    stop("Usage: Rscript Program/export_supplementary_tables.R prepare|finalize", call. = FALSE)
  }
  dependencies <- c("openxlsx", "xml2")
  missing <- dependencies[!vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Supplementary Excel export requires these R packages: ", paste(missing, collapse = ", "),
         ". Install them in your local rendering environment before rendering; no packages were installed.",
         call. = FALSE)
  }
  if (mode == "prepare") {
    inputs <- strsplit(Sys.getenv("QUARTO_PROJECT_INPUT_FILES"), "\n", fixed = TRUE)[[1]]
    if (length(inputs) && !"Manuscript.qmd" %in% basename(inputs)) return(invisible(NULL))
    dir.create(paths$output, recursive = TRUE, showWarnings = FALSE)
    # Only clear this workflow's staging files, never previous final workbooks.
    unlink(c(paths$state, paths$bundle))
    if (file.exists(paths$state) || file.exists(paths$bundle)) {
      stop("Could not clear stale supplementary export staging files.", call. = FALSE)
    }
    state <- list(run_id = paste(format(Sys.time(), "%Y%m%dT%H%M%OS6"), Sys.getpid(), sep = "-"),
                  started_at = Sys.time(), fingerprint = supplementary_export_fingerprint(root))
    if (anyNA(state$fingerprint)) stop("A required supplementary export source file is missing.", call. = FALSE)
    saveRDS(state, paths$state)
    return(invisible(NULL))
  }

  outputs <- strsplit(Sys.getenv("QUARTO_PROJECT_OUTPUT_FILES"), "\n", fixed = TRUE)[[1]]
  docx <- outputs[basename(outputs) == "Manuscript.docx"]
  if (!length(docx) && length(outputs)) return(invisible(NULL))
  if (length(docx) != 1L || !file.exists(docx)) {
    stop("Finalizing supplementary tables requires the Manuscript.docx produced by the current Quarto render.",
         call. = FALSE)
  }
  if (!file.exists(paths$state) || !file.exists(paths$bundle)) {
    stop("Missing current supplementary table bundle. Render the manuscript with execution and project hooks enabled.",
         call. = FALSE)
  }
  state <- readRDS(paths$state)
  bundle <- readRDS(paths$bundle)
  if (!identical(bundle$state, state) ||
      !identical(state$fingerprint, supplementary_export_fingerprint(root)) ||
      file.info(docx)$mtime < state$started_at - 2 ||
      !setequal(names(bundle$tables), supplementary_export_ids())) {
    stop("The supplementary table bundle, sources, and rendered manuscript do not belong to one current render.",
         call. = FALSE)
  }
  references <- read_supplementary_docx(docx, names(bundle$tables))
  numbers <- vapply(references, function(x) x$number, integer(1))
  filename <- paste0("Supplementary_Tables_", supplementary_number_ranges(numbers), ".xlsx")
  destination <- file.path(paths$output, filename)
  previous <- if (file.exists(paths$manifest)) readRDS(paths$manifest) else NULL
  if (!is.null(previous)) {
    pattern <- "^Supplementary_Tables_S[0-9]+(-[0-9]+)?(,S[0-9]+(-[0-9]+)?)*\\.xlsx$"
    if (!is.character(previous$file) || length(previous$file) != 1L ||
        is.na(previous$file) || !grepl(pattern, previous$file) ||
        basename(previous$file) != previous$file) {
      stop("Invalid previous export manifest; no files were replaced.", call. = FALSE)
    }
  }
  if (file.exists(destination) && (is.null(previous) || previous$file != filename)) {
    stop("The new workbook filename already exists outside the recorded export: ", destination,
         ". No files were replaced.", call. = FALSE)
  }

  # Build, save, and verify the entire workbook before replacing a published export.
  staged_workbook <- tempfile(".tables-", tmpdir = paths$output, fileext = ".xlsx")
  staged_manifest <- tempfile(".manifest-", tmpdir = paths$output, fileext = ".rds")
  workbook_backup <- tempfile(".previous-tables-", tmpdir = paths$output, fileext = ".xlsx")
  manifest_backup <- tempfile(".previous-manifest-", tmpdir = paths$output, fileext = ".rds")
  published <- FALSE
  committed <- FALSE
  on.exit({
    if (!committed) {
      if (published) unlink(destination)
      if (file.exists(workbook_backup)) file.rename(workbook_backup, destination)
      if (file.exists(manifest_backup)) {
        unlink(paths$manifest)
        file.rename(manifest_backup, paths$manifest)
      }
    }
    unlink(c(staged_workbook, staged_manifest))
  }, add = TRUE)
  wb <- build_supplementary_workbook(bundle$tables, references)
  openxlsx::saveWorkbook(wb, staged_workbook, overwrite = FALSE)
  verify_supplementary_workbook(staged_workbook, bundle$tables, references)
  manifest <- list(file = filename, generated_at = Sys.time(), run_id = state$run_id,
                   fingerprint = state$fingerprint, docx_md5 = unname(tools::md5sum(docx)),
                   references = references)
  saveRDS(manifest, staged_manifest)
  if (file.exists(destination) && !file.rename(destination, workbook_backup)) {
    stop("Could not replace the previous workbook. Close it in Excel and render again.", call. = FALSE)
  }
  if (!file.rename(staged_workbook, destination)) stop("Could not publish the supplementary workbook.", call. = FALSE)
  published <- TRUE
  if (file.exists(paths$manifest) && !file.rename(paths$manifest, manifest_backup)) {
    stop("Could not replace the export manifest; restoring the previous workbook.", call. = FALSE)
  }
  if (!file.rename(staged_manifest, paths$manifest)) {
    stop("Could not publish the export manifest; restoring the previous workbook.", call. = FALSE)
  }
  committed <- TRUE
  unlink(c(workbook_backup, manifest_backup, paths$state, paths$bundle))
  if (!is.null(previous) && previous$file != filename) {
    old_path <- file.path(paths$output, previous$file)
    unlink(old_path)
    if (file.exists(old_path)) warning("Previous generated workbook could not be removed: ", old_path, call. = FALSE)
  }
  message("Exported ", length(references), " unchanged supplementary tables to ", destination)
  invisible(destination)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L) stop("Specify prepare or finalize.", call. = FALSE)
  run_supplementary_export_hook(args[1])
}
