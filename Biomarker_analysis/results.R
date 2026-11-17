read_pipeline_object <- function(path, object = basename(path)) {
  env <- new.env(parent = emptyenv())
  objects <- load(path, envir = env)
  if (!object %in% objects) stop("Missing object ", object, " in ", path)
  env[[object]]
}

# Exact contrasts expected from steps 7, 8 and 10, for each estimation method.
main_biomarker_contrasts <- function() {
  list(
    "Main interventions" = c("Metformin", "Antioxidants", "Fish.oil.omega", "Workout.supplements",
      "Herbal.supplements", "Vitamins", "Minerals", "TA.65", "NR.NAD", "Sulphoraphane.broccoli.extract",
      "Rapamycin", "DHEA", "Resveratrol", "Antiaging.Exosome", "Antiaging.Hormone.Replacement",
      "Antiaging.Senolytics", "Antiaging.Stem.Cell", "Antiaging.NAD"),
    "Main exercise" = paste0(c("Exercise.1to2.per.week", "Exercise.3to4.per.week",
      "Exercise.5to7.per.week", "Exercise.8plus.per.week"), "_vs_Never")
  )
}

validate_main_biomarker_results <- function(x, n_bootstrap, outcome = "DunedinPACE") {
  required <- c("treatment", "outcome", "method", "analysis", "n_bootstrap", "n_treated", "n_control",
                "ate_estimate", "ci_lower", "ci_upper", "std_error")
  if (!is.data.frame(x) || !all(required %in% names(x)))
    stop("Main results must contain: ", paste(required, collapse = ", "), call. = FALSE)
  stopifnot(nrow(x) == 44L, !anyNA(x[required]), all(x$outcome == outcome),
            all(x$n_bootstrap == n_bootstrap), all(x$n_treated > 0), all(x$n_control > 0),
            !anyDuplicated(x[c("method", "treatment")]))
  for (col in c("ate_estimate", "ci_lower", "ci_upper", "std_error"))
    if (!all(is.finite(x[[col]]))) stop("Nonfinite result: ", col)
  stopifnot(all(x$ci_lower <= x$ci_upper))
  contrasts <- main_biomarker_contrasts()
  stopifnot(setequal(x$method, c("IPW", "G-comp")), setequal(x$analysis, names(contrasts)))
  for (method in c("IPW", "G-comp")) {
    for (analysis in names(contrasts)) {
      treatments <- x$treatment[x$method == method & x$analysis == analysis]
      stopifnot(length(treatments) == length(contrasts[[analysis]]),
                setequal(treatments, contrasts[[analysis]]))
    }
  }
  invisible(x)
}

read_main_biomarker_outputs <- function(output, n_bootstrap, outcome = "DunedinPACE") {
  expected <- c("IPW_results_df", "Standardization_results_df")
  result_files <- list.files(output, pattern = "^(IPW|Standardization)_.*_df$")
  if (!setequal(result_files, expected))
    stop("Expected only main intervention result files. Missing: ",
         paste(setdiff(expected, result_files), collapse = ", "), "; unexpected: ",
         paste(setdiff(result_files, expected), collapse = ", "), call. = FALSE)
  main <- list()
  for (file in expected) {
    x <- read_pipeline_object(file.path(output, file))
    x$method <- if (file == "IPW_results_df") "IPW" else "G-comp"
    x$analysis <- "Main interventions"
    main[[file]] <- x
  }
  for (object in c("IPW_results", "Standardization_results")) {
    x <- read_pipeline_object(file.path(output, "training_analysis_results.RData"), object)
    x$method <- if (object == "IPW_results") "IPW" else "G-comp"
    x$analysis <- "Main exercise"
    main[[object]] <- x
  }
  # Expected count from the current complete pipeline: 18 interventions, 4 exercise contrasts.
  # Both methods now contribute 44 rows total; no subgroup/sensitivity outputs are required.
  data <- dplyr::bind_rows(main)
  validate_main_biomarker_results(data, n_bootstrap, outcome)
  attr(data, "validated_rows") <- nrow(data)
  data
}

validate_biomarker_outputs <- function(output, n_bootstrap, expected_z, ids) {
  prepared <- read_pipeline_object(file.path(output, "data_after_step4"))
  stopifnot(!anyNA(prepared$Patient.ID), !anyDuplicated(prepared$Patient.ID),
    setequal(prepared$Patient.ID, ids),
    isTRUE(all.equal(prepared$DunedinPACE, unname(expected_z[match(prepared$Patient.ID, names(expected_z))]),
                     tolerance = 1e-10)), any(prepared$DunedinPACE < 0), any(prepared$DunedinPACE > 0))
  read_main_biomarker_outputs(output, n_bootstrap)
}

# Final export pass occurs only after every original pipeline consumer has finished.
# Aliases in saved tables/data frames are replaced; source copies remain untouched.
relabel_biomarker_object <- function(x, biomarker, z_column) {
  replace <- function(v) {
    # Preserve labels already containing the target name, e.g. DunedinPACE.Percentile.
    protected <- grepl("DunedinPACE", biomarker, fixed = TRUE) && biomarker != "DunedinPACE"
    if (protected) v <- gsub(biomarker, "\001", v, fixed = TRUE)
    v <- gsub("DunedinPACE", biomarker, v, fixed = TRUE)
    if (protected) v <- gsub("\001", biomarker, v, fixed = TRUE)
    v
  }
  if (is.data.frame(x)) {
    if ("DunedinPACE" %in% names(x)) {
      alias <- z_column
      x[[alias]] <- x$DunedinPACE
      x$DunedinPACE <- NULL
    }
    for (key in names(x)) {
      if (key %in% c("outcome", "Outcome", "Variable", "variable", "Biomarker"))
        x[[key]] <- replace(as.character(x[[key]]))
    }
    alias_names <- names(x) %in% c("DunedinPACE_group", "DunedinPACE_under_one")
    names(x)[alias_names] <- replace(names(x)[alias_names])
    rownames(x) <- replace(rownames(x))
    return(x)
  }
  if (is.character(x)) return(replace(x))
  if (is.list(x) && !inherits(x, c("ggplot", "gg", "lm", "tableone"))) {
    x <- lapply(x, relabel_biomarker_object, biomarker = biomarker, z_column = z_column)
    if (!is.null(names(x))) names(x) <- replace(names(x))
  }
  x
}

export_biomarker_names <- function(output, biomarker, z_column) {
  files <- list.files(output, recursive = TRUE, full.names = TRUE)
  for (file in files) {
    ext <- tolower(tools::file_ext(file))
    if (ext == "rds") {
      saveRDS(relabel_biomarker_object(readRDS(file), biomarker, z_column), file)
    } else if (ext %in% c("", "rdata")) {
      env <- new.env(parent = emptyenv())
      objects <- load(file, envir = env)
      for (object in objects) env[[object]] <- relabel_biomarker_object(env[[object]], biomarker, z_column)
      save(list = objects, file = file, envir = env)
    } else if (ext == "csv") {
      x <- read.csv(file, check.names = FALSE)
      write.csv(relabel_biomarker_object(x, biomarker, z_column), file, row.names = FALSE)
    } else if (ext == "xlsx") {
      sheets <- readxl::excel_sheets(file)
      x <- setNames(lapply(sheets, function(sheet) {
        relabel_biomarker_object(as.data.frame(readxl::read_excel(file, sheet = sheet)), biomarker, z_column)
      }), sheets)
      writexl::write_xlsx(x, file)
    }
    dest <- file.path(dirname(file), gsub("DunedinPACE", biomarker, basename(file), fixed = TRUE))
    if (dest != file && !file.rename(file, dest)) stop("Cannot rename output: ", file)
  }
}

# Internal plotting helper: leaf positions follow hclust$order; heights stay in distance units.
heatmap_dendrogram_segments <- function(tree) {
  empty <- data.frame(x = numeric(), y = numeric(), xend = numeric(), yend = numeric())
  if (is.null(tree)) return(empty)
  leaves <- match(seq_along(tree$order), tree$order)
  positions <- numeric(length(tree$height))
  segments <- vector("list", length(tree$height))
  for (i in seq_along(tree$height)) {
    children <- tree$merge[i, ]
    x <- vapply(children, function(j) if (j < 0L) leaves[-j] else positions[j], numeric(1))
    y <- vapply(children, function(j) if (j < 0L) 0 else tree$height[j], numeric(1))
    positions[i] <- mean(x)
    segments[[i]] <- data.frame(x = c(x, x[1]), y = c(y, tree$height[i]),
      xend = c(x, x[2]), yend = rep(tree$height[i], 3L))
  }
  do.call(rbind, segments)
}

# Mark only valid finite bootstrap intervals strictly excluding zero, on the original scale.
heatmap_ci_excludes_zero <- function(results) {
  required <- c("ci_lower", "ci_upper")
  if (!all(required %in% names(results)))
    stop("Heatmap results require CI columns: ", paste(setdiff(required, names(results)), collapse = ", "),
         call. = FALSE)
  if (!all(vapply(results[required], function(x) is.numeric(x) || all(is.na(x)), logical(1))))
    stop("Heatmap CI columns must be numeric.", call. = FALSE)
  lower <- as.numeric(results$ci_lower)
  upper <- as.numeric(results$ci_upper)
  is.finite(lower) & is.finite(upper) & lower <= upper & (lower > 0 | upper < 0)
}

# Export only aggregate fields from the exact dataframe used to draw a heatmap.
write_heatmap_workbook <- function(data, file, config, age_correlations = NULL) {
  fields <- c("biomarker", "treatment", "method", "analysis", "ate_estimate", "ci_lower", "ci_upper",
              "sign_multiplier", "heatmap_ate", "ci_excludes_zero", "n_treated", "n_control",
              "n_bootstrap", "n_bootstrap_requested", "residualized", "residual_method",
              "scaling_sd", "scaling_source", "n_reference", "data_kind", "units")
  # Older standalone result tables may lack metadata; never infer successful bootstrap counts.
  for (field in setdiff(fields, names(data))) data[[field]] <- NA
  estimates <- data[fields]
  estimates$biomarker <- as.character(estimates$biomarker)
  estimates$biomarker_label <- ifelse(estimates$sign_multiplier == -1,
    paste0(estimates$biomarker, "\u2020"), estimates$biomarker)
  estimates$treatment_label <- as.character(data$treatment_label)
  estimates$plot_row <- match(data$treatment_label, rev(levels(data$treatment_label)))
  estimates$plot_column <- match(data$biomarker, levels(data$biomarker))
  correlation_fields <- c("Pearson\u2019s r" = "pearson_r", "Spearman\u2019s rho" = "spearman_rho")
  for (field in names(correlation_fields)) {
    values <- rep(NA_real_, nrow(estimates))
    if (!is.null(age_correlations))
      values <- age_correlations[[correlation_fields[[field]]]][match(estimates$biomarker, age_correlations$biomarker)]
    values[!is.finite(values)] <- NA_real_
    estimates[[field]] <- round(values, 2L)
  }
  definitions <- c(
    biomarker = "Original biomarker identifier; biomarker_label matches the figure, including the dagger.",
    treatment = "Original intervention/contrast identifier; treatment_label matches the figure. Exercise contrasts use never exercising as reference.",
    method = "IPW: inverse probability weighting; G-comp: G-computation (standardization), which averages predicted potential outcomes over participants. This is separate from scaling outcomes to SD units before modelling.",
    analysis = "Main interventions or Main exercise; only rows included in this figure are exported.",
    ate_estimate = "Adjusted mean difference in outcome SD units, obtained by standardizing outcomes once before modelling. ATEs are not standardized again for plotting or clustering. Original means before sign adjustment, not raw biomarker units, years, or percentages. Values are not capped at the heatmap color limits.",
    ci_lower = "Lower limit of the original 95% percentile bootstrap CI, in the same SD units as ate_estimate; not sign-reversed.",
    ci_upper = "Upper limit of the original 95% percentile bootstrap CI, in the same SD units as ate_estimate; not sign-reversed.",
    sign_multiplier = "-1 only for Telomere.Values, Telomere.Values.PC and DNAmVO2max; +1 otherwise. A dagger identifies reversed measures.",
    heatmap_ate = "ate_estimate * sign_multiplier, used for both colors and clustering. Sign adjustment preserves magnitudes and outcome SD units; no further standardization or capping is applied to these exported values. Negative is green; positive is red; zero is white. Favorable direction is assumed, not proof of clinical benefit.",
    ci_excludes_zero = "TRUE gives a cell star: finite, ordered original CI strictly excludes zero. No multiplicity adjustment; not based on p-values.",
    n_treated = "Number of intervention users or participants in the indicated exercise category in this comparison.",
    n_control = "Number of non-users or never-exercising participants in this comparison.",
    n_bootstrap = "Actual successful bootstrap resamples for this estimate. Two iterations are for technical testing, not scientific uncertainty estimation.",
    n_bootstrap_requested = "Requested bootstrap resamples; new pipeline runs require the successful count to match.",
    residualized = "TRUE: regress the age-in-years biomarker on chronological age, then standardize residuals. FALSE: standardize the raw measure directly. In both cases, outcomes are standardized once before fitting the treatment-effect models.",
    residual_method = "linear, quadratic or spline for age residualization; raw_z for direct standardization. Age remains a model covariate.",
    scaling_sd = "SD denominator of the residuals or raw measure. Centering and scaling use the biomarker-specific eligible sample and remain fixed during bootstrap resampling.",
    scaling_source = "analysis_cohort or external_reference; identifies the source of the SD denominator.",
    n_reference = "Biomarker-specific eligible reference sample used to fit the transformation, pooling users and non-users.",
    data_kind = "real or mock, as recorded in the source results.",
    units = "SD: differences in units of the SD used to standardize each biomarker's residuals or raw values before modelling, not the SD of the ATEs. Original refers to the estimate before heatmap sign adjustment.",
    plot_row = "Row position from top to bottom in the heatmap; worksheet rows retain the original result order.",
    plot_column = "Column position from left to right in the heatmap.",
    clustering = "Euclidean distance and complete linkage, independently for rows and columns and for each method, using full sign-adjusted ATEs without capping or further standardization.",
    color_limits = paste0("Only the heatmap color scale is capped at +/-", config$heatmap_limit, " SD. Values at or below the negative limit share the maximum green intensity; values at or above the positive limit share the maximum red intensity. Numerical ATEs, confidence intervals and clustering values retain their full values."),
    missing_metadata = "Blank metadata cells mean the field was unavailable in the supplied result table; no values or confidence intervals are imputed.",
    precision = "Numeric values other than the age correlations are exported without deliberate rounding. Original confidence intervals retain their original direction.",
    "Pearson\u2019s r" = "Pearson correlation of original biomarker values, before age adjustment or standardization, with Decimal.Chronological.Age in the biomarker-specific eligible analysis cohort, pooling intervention users and non-users. Uses finite paired observations; rounded to two decimal places without sign reversal. Blank if undefined or unavailable.",
    "Spearman\u2019s rho" = "Spearman rank correlation of original biomarker values, before age adjustment or standardization, with Decimal.Chronological.Age in the biomarker-specific eligible analysis cohort, pooling intervention users and non-users. Uses finite paired observations; rounded to two decimal places without sign reversal. Blank if undefined or unavailable."
  )
  writexl::write_xlsx(list(Estimates = estimates,
    Definitions = data.frame(field = names(definitions), definition = unname(definitions))), file)
}

# Publish a complete aggregate bundle, with rollback of current/ and LATEST.txt on failure.
publish_biomarker_outputs <- function(run, config, success_lines) {
  parent <- dirname(run)
  current <- file.path(parent, "current")
  latest <- file.path(parent, "LATEST.txt")
  lock <- file.path(parent, ".publish-lock")
  if (!dir.create(lock, showWarnings = FALSE))
    stop("Another export is publishing, or a previous process left ", lock,
         ". Check that no publisher is running before removing the lock.", call. = FALSE)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  stage <- tempfile(".current-stage-", tmpdir = parent)
  backup <- tempfile(".current-backup-", tmpdir = parent)
  latest_stage <- tempfile(".latest-stage-", tmpdir = parent)
  latest_backup <- tempfile(".latest-backup-", tmpdir = parent)
  committed <- moved_current <- installed_current <- moved_latest <- installed_latest <- FALSE
  success <- file.path(run, "SUCCESS.txt")
  on.exit(suspendInterrupts({
    if (!committed) {
      if (installed_latest) unlink(latest)
      if (moved_latest && !file.rename(latest_backup, latest))
        warning("Could not restore LATEST.txt; retained backup: ", latest_backup)
      if (installed_current) unlink(current, recursive = TRUE)
      if (moved_current && !file.rename(backup, current))
        warning("Could not restore current/; retained backup: ", backup)
      unlink(success)
    } else {
      unlink(backup, recursive = TRUE)
      unlink(latest_backup)
    }
    unlink(stage, recursive = TRUE)
    unlink(latest_stage)
  }), add = TRUE, after = FALSE)
  dir.create(stage)
  figure_files <- unlist(lapply(c("IPW", "G-comp"), function(method)
    paste0("main_heatmap_", method, c(".png", ".pdf", ".rds", ".csv"))), use.names = FALSE)
  bundle_files <- c(figure_files, "Supplementary_Data_S1_IPW.xlsx", "Supplementary_Data_S2_G-comp.xlsx")
  sources <- c(file.path(run, "heatmaps", bundle_files),
               file.path(run, c("biomarker_selection.csv", "residual_metadata.csv")))
  if (any(!file.exists(sources)) || any(is.na(file.info(sources)$size) | file.info(sources)$size <= 0))
    stop("Cannot publish an incomplete heatmap bundle from ", run, call. = FALSE)
  if (!all(file.copy(sources, stage))) stop("Could not stage the heatmap bundle.", call. = FALSE)
  copies <- file.path(stage, basename(sources))
  if (!identical(unname(tools::md5sum(sources)), unname(tools::md5sum(copies))))
    stop("Staged heatmap bundle differs from archived outputs.", call. = FALSE)
  metadata <- data.frame(run_id = basename(run),
    exported_utc = format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
    data_kind = config$data_kind, n_bootstrap_requested = config$n_bootstrap,
    residual_method = config$residual_method, spline_df = config$spline_df,
    scaling_source = if (is.null(config$reference_sd_file)) "analysis_cohort" else "external_reference",
    heatmap_limit = config$heatmap_limit, include_exercise_in_heatmap = config$include_exercise_in_heatmap,
    seed = config$seed, n_workers = config$n_workers)
  write.csv(metadata, file.path(stage, "export_metadata.csv"), row.names = FALSE)
  writeLines(success_lines, file.path(stage, "SUCCESS.txt"))
  writeLines(run, latest_stage)
  # Suspend interrupts only during the short replacement/rollback transaction.
  suspendInterrupts({
    if (file.exists(current)) {
      if (!file.rename(current, backup)) stop("Could not back up current/.", call. = FALSE)
      moved_current <- TRUE
    }
    if (!file.rename(stage, current)) stop("Could not install current/.", call. = FALSE)
    installed_current <- TRUE
    if (file.exists(latest)) {
      if (!file.rename(latest, latest_backup)) stop("Could not back up LATEST.txt.", call. = FALSE)
      moved_latest <- TRUE
    }
    if (!file.rename(latest_stage, latest)) stop("Could not update LATEST.txt.", call. = FALSE)
    installed_latest <- TRUE
    writeLines(success_lines, success)
    committed <- TRUE
  })
  invisible(current)
}

plot_biomarker_heatmaps <- function(results, directory, config, age_correlations = NULL) {
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("Heatmap plotting requires the 'patchwork' package.")
  if (!requireNamespace("writexl", quietly = TRUE)) stop("Heatmap exports require the 'writexl' package.")
  ci_excludes_zero <- heatmap_ci_excludes_zero(results)
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  source("Program/Functions/display_names.R")
  x <- results
  x$ci_excludes_zero <- ci_excludes_zero
  if (!config$include_exercise_in_heatmap) x <- x[x$analysis == "Main interventions", ]
  biomarkers <- unique(x$biomarker)
  treatments <- unique(x$treatment)
  # Exact names only: assumed favorable increases are green after multiplying by -1.
  # Apply this convention to BOTH colours and Euclidean clustering; keep original ATEs intact.
  reversed_biomarkers <- c("Telomere.Values", "Telomere.Values.PC", "DNAmVO2max")
  x$sign_multiplier <- ifelse(x$biomarker %in% reversed_biomarkers, -1, 1)
  x$heatmap_ate <- x$ate_estimate * x$sign_multiplier
  limit <- config$heatmap_limit
  if (!is.finite(limit) || limit <= 0) stop("heatmap_limit must be positive")
  for (method in c("IPW", "G-comp")) {
    d <- x[x$method == method, ]
    # Cluster each method's sign-adjusted ATEs before colour clipping, without rescaling.
    estimates <- matrix(NA_real_, nrow = length(treatments), ncol = length(biomarkers),
      dimnames = list(treatments, biomarkers))
    estimates[cbind(match(d$treatment, treatments), match(d$biomarker, biomarkers))] <- d$heatmap_ate
    if (!length(estimates) || any(!is.finite(estimates)))
      stop("Incomplete or nonfinite heatmap estimates for ", method)
    row_tree <- if (nrow(estimates) > 1L)
      stats::hclust(stats::dist(estimates, method = "euclidean"), method = "complete") else NULL
    column_tree <- if (ncol(estimates) > 1L)
      stats::hclust(stats::dist(t(estimates), method = "euclidean"), method = "complete") else NULL
    row_order <- if (is.null(row_tree)) seq_along(treatments) else row_tree$order
    column_order <- if (is.null(column_tree)) seq_along(biomarkers) else column_tree$order
    d$biomarker <- factor(d$biomarker, levels = biomarkers[column_order])
    # ggplot's y-axis runs bottom to top; reverse levels to show cluster order top to bottom.
    d$treatment_label <- factor(vapply(d$treatment, format_display_name, character(1)),
      levels = rev(vapply(treatments[row_order], format_display_name, character(1))))
    heatmap <- ggplot2::ggplot(d, ggplot2::aes(biomarker, treatment_label, fill = heatmap_ate)) +
      ggplot2::geom_tile() +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(ci_excludes_zero, "*", "")),
                         colour = "black", size = 4, hjust = 0.5, vjust = 0.5) +
      ggplot2::scale_x_discrete(labels = function(labels)
        ifelse(labels %in% reversed_biomarkers, paste0(labels, "\u2020"), labels)) +
      ggplot2::scale_fill_gradient2(low = "#00FF00", mid = "white", high = "#FF0000",
        midpoint = 0, limits = c(-limit, limit), oob = scales::squish,
        breaks = seq(-limit, limit, length.out = 5), name = "ATE After Sign Adjustment",
        guide = ggplot2::guide_colourbar(direction = "horizontal", reverse = TRUE,
                                        barwidth = grid::unit(60, "mm"))) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(), legend.position = "top",
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5),
        plot.caption = ggplot2::element_text(hjust = 0),
        panel.border = ggplot2::element_rect(fill = NA, colour = "grey40", linewidth = 0.4))
    # Match the heatmap's discrete-axis expansion (0.6) so tree leaves align with tile centres.
    column_plot <- ggplot2::ggplot(heatmap_dendrogram_segments(column_tree),
      ggplot2::aes(x, y, xend = xend, yend = yend)) +
      ggplot2::geom_segment(colour = "grey40", linewidth = 0.4) +
      ggplot2::scale_x_continuous(limits = c(.4, length(biomarkers) + .6), expand = c(0, 0)) +
      ggplot2::scale_y_continuous(limits = c(0, max(c(column_tree$height, 1e-8))),
        expand = ggplot2::expansion(mult = c(0, .05))) + ggplot2::theme_void()
    row_plot <- ggplot2::ggplot(heatmap_dendrogram_segments(row_tree),
      ggplot2::aes(y, length(treatments) + 1 - x, xend = yend, yend = length(treatments) + 1 - xend)) +
      ggplot2::geom_segment(colour = "grey40", linewidth = 0.4) +
      ggplot2::scale_x_reverse(limits = c(max(c(row_tree$height, 1e-8)), 0),
        expand = ggplot2::expansion(mult = c(.05, 0))) +
      ggplot2::scale_y_continuous(limits = c(.4, length(treatments) + .6), expand = c(0, 0)) +
      ggplot2::theme_void()
    plot <- patchwork::wrap_plots(column_plot, row_plot, heatmap, design = "#A\nBC",
      widths = c(1, 8), heights = c(1, 5), guides = "collect") +
      patchwork::plot_annotation(theme = ggplot2::theme_minimal(base_size = 10) +
          ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0))) &
      ggplot2::theme(legend.position = "top")
    stem <- file.path(directory, paste0("main_heatmap_", method))
    ggplot2::ggsave(paste0(stem, ".png"), plot, width = max(11, length(biomarkers) * .28 + 3) + 1.2,
                   height = max(7, length(treatments) * .25 + 2.8) + 1.8, dpi = 200)
    ggplot2::ggsave(paste0(stem, ".pdf"), plot, width = max(11, length(biomarkers) * .28 + 3) + 1.2,
                   height = max(7, length(treatments) * .25 + 2.8) + 1.8)
    saveRDS(plot, paste0(stem, ".rds"))
    write.csv(d[c("biomarker", "treatment", "method", "analysis", "ate_estimate", "sign_multiplier", "heatmap_ate",
                  "ci_lower", "ci_upper", "ci_excludes_zero")],
              paste0(stem, ".csv"), row.names = FALSE)
    workbook <- if (method == "IPW") "Supplementary_Data_S1_IPW.xlsx" else "Supplementary_Data_S2_G-comp.xlsx"
    write_heatmap_workbook(d, file.path(directory, workbook), config, age_correlations = age_correlations)
  }
}
