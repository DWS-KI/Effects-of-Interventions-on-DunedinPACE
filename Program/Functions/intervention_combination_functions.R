# Shared exact-intersection definitions for the UpSet plot and sensitivity analysis.
# Callers source display_names.R before constructing readable combination labels.

intervention_pattern_columns <- function() {
  c("Metformin", "Antioxidants", "Fish.oil.omega", "Workout.supplements",
    "Herbal.supplements", "Vitamins", "Minerals", "TA.65", "NR.NAD",
    "Sulphoraphane.broccoli.extract", "Rapamycin", "DHEA", "Resveratrol",
    "Antiaging.Exosome", "Antiaging.Hormone.Replacement", "Antiaging.Senolytics",
    "Antiaging.Stem.Cell", "Antiaging.NAD")
}

prepare_intervention_pattern_data <- function(data) {
  columns <- intervention_pattern_columns()
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop("Missing intervention-pattern columns: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  binary <- data[columns]
  for (column in columns) {
    values <- binary[[column]]
    if (is.factor(values)) values <- as.character(values)
    if (!all(is.na(values) | values %in% c(0, 1))) {
      stop("Intervention-pattern column is not binary: ", column, call. = FALSE)
    }
    # Match the UpSet plot's convention; do not recode the main-analysis data.
    values[is.na(values)] <- 0
    binary[[column]] <- as.integer(values)
  }
  binary
}

intervention_pattern_keys <- function(binary) {
  do.call(paste0, binary)
}

count_intervention_patterns <- function(data) {
  binary <- prepare_intervention_pattern_data(data)
  columns <- names(binary)
  # UpSetR orders sets by prevalence, then initially counts patterns in binary
  # lexicographic order. Its frequency sort is stable, preserving that tie order.
  set_order <- columns[order(colSums(binary), decreasing = TRUE)]
  keys <- intervention_pattern_keys(binary[set_order])
  frequencies <- table(keys)
  patterns <- binary[match(names(frequencies), keys), , drop = FALSE]
  patterns$frequency <- as.integer(frequencies)
  patterns$n_interventions <- rowSums(patterns[columns])
  patterns <- patterns[patterns$n_interventions > 0, , drop = FALSE]
  patterns <- patterns[order(patterns$frequency, decreasing = TRUE), , drop = FALSE]
  rownames(patterns) <- NULL
  patterns
}

select_intervention_combinations <- function(data, n_top) {
  if (!is.numeric(n_top) || length(n_top) != 1L || is.na(n_top) ||
      !is.finite(n_top) || n_top < 1 || n_top != floor(n_top)) {
    stop("n_top_combinations must be one positive whole number.", call. = FALSE)
  }
  binary <- prepare_intervention_pattern_data(data)
  columns <- names(binary)
  patterns <- count_intervention_patterns(data)
  eligible <- patterns[patterns$n_interventions >= 2L, , drop = FALSE]
  if (nrow(eligible) < n_top) {
    stop("Requested ", n_top, " combinations, but only ", nrow(eligible),
         " multi-intervention patterns were observed.", call. = FALSE)
  }
  selected <- head(eligible, n_top)
  selected$rank <- seq_len(nrow(selected))
  # IDs encode membership in the canonical 18-column order, independently of rank.
  selected_keys <- intervention_pattern_keys(selected[columns])
  selected$treatment <- paste0("Combination.", selected_keys)
  selected$label <- vapply(seq_len(nrow(selected)), function(i) {
    components <- columns[as.integer(selected[i, columns]) == 1L]
    paste0("Exact: ", paste(vapply(components, format_display_name, character(1)),
                            collapse = " + "))
  }, character(1))
  selected$n_control <- nrow(data) - selected$frequency
  if (any(selected$n_control == 0L)) {
    stop("An exact combination has no reference participants.", call. = FALSE)
  }
  if (any(selected$treatment %in% names(data))) {
    stop("Combination exposure columns already exist in the input data.", call. = FALSE)
  }
  keys <- intervention_pattern_keys(binary)
  for (i in seq_len(nrow(selected))) {
    data[[selected$treatment[i]]] <- as.integer(keys == selected_keys[i])
  }
  stopifnot(all(colSums(data[selected$treatment]) == selected$frequency))
  component_treatments <- columns[colSums(selected[columns]) > 0L]
  rownames(selected) <- NULL
  list(data = data, selection = list(
    n_top_combinations = as.integer(n_top), n_participants = nrow(data),
    intervention_columns = columns, definitions = selected,
    component_treatments = component_treatments
  ))
}

# Presentation only: reuse the original individual-intervention estimates and
# join by identifiers because each estimator sorts its result rows independently.
prepare_combination_comparison <- function(ipw_main, std_main, ipw_combinations,
                                           std_combinations, selection) {
  components <- selection$component_treatments
  definitions <- selection$definitions
  combination_ids <- definitions$treatment
  if (!length(components) || anyDuplicated(components) ||
      length(combination_ids) != selection$n_top_combinations ||
      anyDuplicated(combination_ids)) {
    stop("Invalid saved combination selection.", call. = FALSE)
  }
  validate_results <- function(results, expected) {
    required <- c("treatment", "outcome", "ate_estimate", "ci_lower", "ci_upper",
                  "n_treated", "n_control")
    if (!all(required %in% names(results)) || nrow(results) != length(expected) ||
        anyNA(results[c("treatment", "outcome")]) || anyDuplicated(results$treatment) ||
        !setequal(results$treatment, expected) || !all(results$outcome == "DunedinPACE")) {
      stop("Expected one DunedinPACE result per selected exposure in each method.",
           call. = FALSE)
    }
    numeric_values <- results[c("ate_estimate", "ci_lower", "ci_upper",
                                 "n_treated", "n_control")]
    if (!all(vapply(numeric_values, function(x) is.numeric(x) && all(is.finite(x)), logical(1))) ||
        any(results$ci_lower > results$ci_upper) ||
        any(results$n_treated <= 0 | results$n_control <= 0) ||
        any(results$n_treated + results$n_control != selection$n_participants)) {
      stop("Invalid estimates, confidence limits, or cohort counts in combination results.",
           call. = FALSE)
    }
  }
  ipw_main <- ipw_main[ipw_main$treatment %in% components, , drop = FALSE]
  std_main <- std_main[std_main$treatment %in% components, , drop = FALSE]
  validate_results(ipw_main, components)
  validate_results(std_main, components)
  validate_results(ipw_combinations, combination_ids)
  validate_results(std_combinations, combination_ids)
  for (results in list(ipw_combinations, std_combinations)) {
    if (!all(results$n_treated == definitions$frequency[match(results$treatment, combination_ids)])) {
      stop("Combination analysis counts differ from the selected exact intersections.", call. = FALSE)
    }
  }
  ipw <- dplyr::bind_rows(ipw_main, ipw_combinations)
  std <- dplyr::bind_rows(std_main, std_combinations)
  comparison <- dplyr::inner_join(ipw, std, by = c("treatment", "outcome"),
                                   suffix = c("_ipw", "_std"))
  if (!all(comparison$n_treated_ipw == comparison$n_treated_std) ||
      !all(comparison$n_control_ipw == comparison$n_control_std)) {
    stop("IPW and G-computation group counts disagree.", call. = FALSE)
  }
  is_combination <- comparison$treatment %in% combination_ids
  labels <- c(stats::setNames(vapply(components, format_display_name, character(1)), components),
              stats::setNames(definitions$label, combination_ids))
  comparison$label <- unname(labels[comparison$treatment])
  comparison$exposure_type <- ifelse(is_combination, "Exact combination", "Individual intervention (main)")
  comparison$analysis <- ifelse(is_combination, "sensitivity_combinations", "main")
  comparison <- comparison[order(comparison$ate_estimate_ipw, comparison$treatment), , drop = FALSE]
  rownames(comparison) <- NULL
  comparison
}
