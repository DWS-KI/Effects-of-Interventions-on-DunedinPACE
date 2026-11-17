# Step-log validation used by biomarker_worker() in runtime.R.
# These helpers do not execute the pipeline or change its analysis scripts.
analysis_spec <- function(analysis_steps = c(7L, 8L, 10L), auxiliary = FALSE) {
  if (!is.numeric(analysis_steps) || anyNA(analysis_steps) ||
      anyDuplicated(analysis_steps) || !all(analysis_steps %in% 5:21))
    stop("analysis_steps must contain unique step numbers from 5 through 21.", call. = FALSE)
  if (!is.logical(auxiliary) || length(auxiliary) != 1L || is.na(auxiliary))
    stop("auxiliary must be TRUE or FALSE.", call. = FALSE)
  # Every biomarker run repeats preparation steps 2-4. Step 1 was run once
  # in the separate preparation project and is not part of this log.
  list(steps = c(2:4, sort(as.integer(analysis_steps))), auxiliary = auxiliary)
}

validate_step_log <- function(path, spec = analysis_spec()) {
  if (!file.exists(path)) stop("Missing pipeline step log: ", path, call. = FALSE)
  log <- read.csv(path, stringsAsFactors = FALSE)
  required <- c("script", "success", "seconds", "error")
  if (!all(required %in% names(log)))
    stop("Invalid pipeline step log; missing columns: ",
         paste(setdiff(required, names(log)), collapse = ", "), call. = FALSE)
  if (!nrow(log)) stop("Pipeline step log is empty: ", path, call. = FALSE)
  if (anyNA(log$script) ||
      !all(grepl("step[0-9]+\\.R$", log$script, ignore.case = TRUE)))
    stop("Pipeline step log contains missing or unrecognized script names.", call. = FALSE)

  steps <- as.integer(sub(".*step([0-9]+)\\.R$", "\\1", log$script, ignore.case = TRUE))
  missing <- setdiff(spec$steps, steps)
  unexpected <- setdiff(steps, spec$steps)
  repeated <- unique(steps[duplicated(steps)])
  problems <- character()
  if (length(missing)) problems <- c(problems, paste("missing steps:", paste(missing, collapse = ", ")))
  if (length(unexpected)) problems <- c(problems, paste("unexpected steps:", paste(unexpected, collapse = ", ")))
  if (length(repeated)) problems <- c(problems, paste("repeated steps:", paste(repeated, collapse = ", ")))
  if (length(problems))
    stop("Incomplete pipeline step log (", paste(problems, collapse = "; "), "). See ", path, call. = FALSE)

  if (!is.logical(log$success))
    stop("Pipeline step log has an invalid success column: ", path, call. = FALSE)
  failed <- is.na(log$success) | !log$success |
    (!is.na(log$error) & nzchar(trimws(log$error)))
  if (any(failed))
    stop("Pipeline steps failed: ", paste(log$script[failed], collapse = ", "),
         ". See ", path, call. = FALSE)
  invisible(log)
}
