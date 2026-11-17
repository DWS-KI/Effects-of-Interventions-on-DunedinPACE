# Parallelism is only between isolated biomarker processes. The statistical
# engine, its sequential adapter and its RNG remain unchanged inside each one.
biomarker_worker_count <- function(n_biomarkers, requested = NULL,
                                   capacity = parallelly::availableCores(omit = 1L)) {
  positive_integer <- function(x) is.numeric(x) && length(x) == 1L &&
    !is.na(x) && is.finite(x) && x >= 1 && x <= .Machine$integer.max && x == floor(x)
  if (!positive_integer(n_biomarkers) || !positive_integer(capacity))
    stop("Biomarker count and worker capacity must be positive integers.", call. = FALSE)
  if (!is.null(requested) && !positive_integer(requested))
    stop("n_workers must be NULL or one positive integer.", call. = FALSE)
  as.integer(min(n_biomarkers, capacity, if (is.null(requested)) capacity else requested))
}

# Set before R starts: numerical libraries must not multiply the worker count.
biomarker_process_env <- function() {
  c(callr::rcmd_safe_env(), OMP_NUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1",
    MKL_NUM_THREADS = "1", VECLIB_MAXIMUM_THREADS = "1", BLIS_NUM_THREADS = "1",
    RCPP_PARALLEL_NUM_THREADS = "1")
}

# R6 disables an unused clone method in some callr/processx combinations.
# Muffle only that exact informational message during process construction.
without_process_clone_message <- function(expr) {
  notice <- paste0("Superclass process has cloneable=FALSE, but subclass r_process has cloneable=TRUE. ",
    "A subclass cannot be cloneable when its superclass is not cloneable, ",
    "so cloning will be disabled for r_process.")
  withCallingHandlers(expr, message = function(m) {
    if (identical(trimws(conditionMessage(m)), notice)) invokeRestart("muffleMessage")
  })
}

# start_job returns a callr process; collect_job validates/exports in the parent.
# Callbacks and process handles also allow deterministic tests without analyses.
# Only the parent invokes checkpoint, always in the configured biomarker order.
run_biomarker_queue <- function(biomarkers, n_workers, start_job, collect_job,
                                checkpoint, wait = function() Sys.sleep(0.1)) {
  n_workers <- biomarker_worker_count(length(biomarkers), n_workers, capacity = n_workers)
  n <- length(biomarkers)
  active <- vector("list", n)
  values <- statuses <- setNames(vector("list", n), biomarkers)
  started <- rep(as.POSIXct(NA), n)
  next_job <- 1L
  nonnull <- function(x) !vapply(x, is.null, logical(1))
  status_table <- function() {
    x <- do.call(rbind, statuses[nonnull(statuses)])
    rownames(x) <- NULL
    x
  }
  save_checkpoint <- function() checkpoint(values[nonnull(values)], status_table())
  record <- function(i, success, error = "", rows = 0L) {
    statuses[[i]] <<- data.frame(biomarker = biomarkers[i], success = success,
      validated_rows = rows, seconds = as.numeric(difftime(Sys.time(), started[i], units = "secs")),
      error = error)
  }
  on.exit({
    remaining <- which(nonnull(active))
    # Stop all owned children before waiting; never touch unrelated R processes.
    for (i in remaining) try(active[[i]]$kill_tree(), silent = TRUE)
    for (i in remaining) {
      try(active[[i]]$wait(timeout = 5000), silent = TRUE)
      if (is.null(statuses[[i]])) record(i, FALSE, "Cancelled: run failed or was interrupted.")
    }
    if (length(remaining)) try(save_checkpoint(), silent = TRUE)
  }, add = TRUE)

  repeat {
    finished <- which(vapply(active, function(p) !is.null(p) && !p$is_alive(), logical(1)))
    for (i in finished) {
      result <- tryCatch({
        active[[i]]$get_result()  # Surface nonzero exits and remote R errors.
        collect_job(i)
      }, error = identity)
      if (inherits(result, "error")) {
        record(i, FALSE, conditionMessage(result))
        save_checkpoint()
        stop(biomarkers[i], ": ", conditionMessage(result), call. = FALSE)
      }
      values[i] <- list(result)
      record(i, TRUE, rows = attr(result, "validated_rows"))
      active[i] <- list(NULL)
      save_checkpoint()
    }
    if (next_job > n && !any(nonnull(active))) break
    if (next_job <= n && sum(nonnull(active)) < n_workers) {
      i <- next_job
      started[i] <- Sys.time()
      # Do not lose a newly created process handle if Ctrl-C arrives at launch.
      error <- tryCatch({
        suspendInterrupts({ active[i] <- list(start_job(i)) })
        NULL
      }, error = identity)
      if (inherits(error, "error")) {
        record(i, FALSE, conditionMessage(error))
        save_checkpoint()
        stop(biomarkers[i], ": ", conditionMessage(error), call. = FALSE)
      }
      next_job <- next_job + 1L
    } else {
      wait()
    }
  }
  invisible(list(results = values, status = status_table()))
}
