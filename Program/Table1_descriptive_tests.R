# Unadjusted descriptive comparisons for the manuscript's Table 1.
# This file defines functions only. The hidden, uncached manuscript chunk calls
# them locally; it does not source Table1_DunedinPACE_step5.R or rerun the pipeline.

run_table1_descriptive_tests <- function(cohort_path, dunedin_stats) {
  fail <- function(...) stop(..., call. = FALSE)
  if (!is.character(cohort_path) || length(cohort_path) != 1L ||
      is.na(cohort_path) || !file.exists(cohort_path)) {
    fail("Baseline comparisons require the locally saved Output/data_after_step4 cohort. No fallback or pipeline rerun is permitted.")
  }

  # Loading into a private environment prevents saved objects from overwriting
  # manuscript metadata or other objects in the knitting environment.
  cohort_env <- new.env(parent = emptyenv())
  loaded_names <- load(cohort_path, envir = cohort_env)
  if (!"data_after_step4" %in% loaded_names) {
    fail("The cohort file must contain an object named data_after_step4.")
  }
  cohort <- get("data_after_step4", envir = cohort_env, inherits = FALSE)
  if (!is.data.frame(cohort) || nrow(cohort) == 0L ||
      anyDuplicated(names(cohort))) {
    fail("data_after_step4 must be a nonempty data frame with unique column names.")
  }

  labels <- c(
    Decimal.Chronological.Age = "Chronological age",
    Biological.Sex = "Biological sex",
    Level.of.Education = "Education",
    Tobacco.Use.Binary = "Tobacco use",
    Alcohol.per.week = "Alcohol consumption",
    Exercise.per.week = "Exercise frequency",
    BMI = "BMI",
    Stress.Level = "Stress",
    Hours.of.sleep.per.night = "Sleep duration",
    Ethnicity.Recoded = "Ethnicity"
  )
  variables <- names(labels)
  required <- c("DunedinPACE", variables)
  absent <- setdiff(required, names(cohort))
  if (length(absent)) {
    fail("Missing required cohort variables: ", paste(absent, collapse = ", "), ".")
  }
  input_types <- vapply(variables, function(v) {
    x <- cohort[[v]]
    paste0("class=", paste(class(x), collapse = "/"), "; typeof=", typeof(x),
           "; ordered_factor=", is.ordered(x),
           if (is.factor(x)) paste0("; declared_factor_levels=", nlevels(x)) else "")
  }, character(1))
  main_variables <- setdiff(required, "Ethnicity.Recoded")
  missing_counts <- vapply(main_variables, function(v) {
    sum(is.na(cohort[[v]]))
  }, numeric(1))
  if (any(missing_counts > 0)) {
    bad <- missing_counts[missing_counts > 0]
    fail("Unexpected missing main covariates/outcome: ",
         paste(paste0(names(bad), " (n=", bad, ")"), collapse = "; "),
         ". Only missing ethnicity may be omitted, within its comparison.")
  }

  numeric_variables <- c("DunedinPACE", "Decimal.Chronological.Age", "BMI", "Stress.Level")
  for (v in numeric_variables) {
    x <- cohort[[v]]
    if (!is.numeric(x) || is.factor(x) || !is.null(dim(x)) ||
        any(!is.finite(x))) {
      fail(v, " must be a finite numeric vector with no missing values.")
    }
  }
  if (any(cohort$DunedinPACE <= 0)) fail("DunedinPACE must be strictly positive.")
  if (any(cohort$Decimal.Chronological.Age < 18 |
          cohort$Decimal.Chronological.Age > 110)) {
    fail("Chronological age must be within the cohort eligibility range, 18 to 110 years.")
  }
  if (any(cohort$BMI < 15)) fail("BMI must be at least 15 kg/m^2, as required by cohort eligibility.")
  if (any(cohort$Stress.Level < 0 | cohort$Stress.Level > 10)) {
    fail("Stress.Level must be between 0 and 10.")
  }

  # These are the survey categories in the supplied manuscript's Table 1.
  # Typographic apostrophes are normalized to straight apostrophes in both the
  # allowed labels and the local comparison data. This changes punctuation only;
  # substantive categories are never pooled. Normalization counts are retained.
  normalize_apostrophes <- function(x) chartr("‘’", "''", as.character(x))
  allowed_levels <- list(
    Biological.Sex = c("Female", "Male"),
    Level.of.Education = c(
      "Did not complete high school", "High school or equivalent",
      "Technical or occupational certificate", "Some college coursework completed",
      "Associate degree", "Bachelor’s degree", "Master’s degree",
      "Professional (MD, DO, DDS, JD)", "Doctorate (PhD)"
    ),
    Tobacco.Use.Binary = c("None", "Smokers"),
    Alcohol.per.week = c("Never", "On special occasions", "Once per week",
                        "3-5 times per week", "Regularly"),
    Exercise.per.week = c("Never", "1-2 times per week", "3-4 times per week",
                         "5-7 times per week", "8 or more times per week"),
    Hours.of.sleep.per.night = c("6 hours or less", "6 to 8 hours", "More than 8 hours"),
    Ethnicity.Recoded = c("Euro", "Asia", "Latin", "Other Ethnicity")
  )
  allowed_levels <- lapply(allowed_levels, normalize_apostrophes)
  normalization_counts <- setNames(integer(length(allowed_levels)), names(allowed_levels))
  for (v in names(allowed_levels)) {
    x <- cohort[[v]]
    if (!(is.factor(x) || is.character(x)) || !is.null(dim(x))) {
      fail(v, " must be a factor or character vector using the expected survey categories.")
    }
    normalized <- normalize_apostrophes(x)
    normalization_counts[v] <- sum(normalized != as.character(x), na.rm = TRUE)
    unexpected <- setdiff(unique(normalized[!is.na(normalized)]), allowed_levels[[v]])
    if (length(unexpected)) {
      fail("Unexpected categories in ", v, ": ",
           paste(dQuote(unexpected), collapse = ", "), ". Review the coding before testing.")
    }
    cohort[[v]] <- normalized
  }

  valid_count <- function(x) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) &&
      x > 0 && x == floor(x)
  }
  if (!is.list(dunedin_stats) ||
      !all(c("n_slow", "n_fast") %in% names(dunedin_stats)) ||
      !valid_count(dunedin_stats$n_slow) || !valid_count(dunedin_stats$n_fast)) {
    fail("dunedin_stats must contain positive integer n_slow and n_fast metadata counts.")
  }
  group <- factor(ifelse(cohort$DunedinPACE < 1.0, "slow", "fast"),
                  levels = c("slow", "fast"))
  group_counts <- table(group)
  if (unname(group_counts["slow"]) != dunedin_stats$n_slow ||
      unname(group_counts["fast"]) != dunedin_stats$n_fast) {
    fail("Cohort/metadata count mismatch: cohort has n_slow=", group_counts["slow"],
         ", n_fast=", group_counts["fast"], "; manuscript metadata has n_slow=",
         dunedin_stats$n_slow, ", n_fast=", dunedin_stats$n_fast,
         ". Use the matching real cohort and metadata; do not combine supplied 11,101-person results with repository 5,210-person summaries.")
  }
  if (any(group_counts < 2L)) fail("Each DunedinPACE group must contain at least two participants.")

  # Validate every categorical comparison before running any test. Remove only
  # unused categories; never pool categories or switch the specified test.
  categorical_tables <- list()
  expected_tables <- list()
  unsuitable <- character()
  for (v in names(allowed_levels)) {
    keep <- !is.na(cohort[[v]])
    categories <- droplevels(factor(cohort[[v]][keep], levels = allowed_levels[[v]]))
    observed <- table(category = categories, group = group[keep])
    if (nrow(observed) < 2L || any(colSums(observed) == 0L)) {
      unsuitable <- c(unsuitable, paste0(v, ": requires at least two observed categories and participants in both groups"))
      next
    }
    expected <- outer(rowSums(observed), colSums(observed)) / sum(observed)
    dimnames(expected) <- dimnames(observed)
    small <- which(expected < 5, arr.ind = TRUE)
    if (nrow(small)) {
      cells <- vapply(seq_len(nrow(small)), function(i) {
        paste0(rownames(expected)[small[i, 1]], "/", colnames(expected)[small[i, 2]],
               "=", formatC(expected[small[i, 1], small[i, 2]], format = "f", digits = 2))
      }, character(1))
      unsuitable <- c(unsuitable, paste0(v, ": expected cell counts below five (",
                                         paste(cells, collapse = "; "), ")"))
    }
    categorical_tables[[v]] <- observed
    expected_tables[[v]] <- expected
  }
  if (length(unsuitable)) {
    fail("Planned Pearson chi-square comparisons are unsuitable:\n",
         paste(unsuitable, collapse = "\n"),
         "\nNo alternative test was substituted. Review these comparisons before proceeding.")
  }

  rows <- lapply(variables, function(v) {
    keep <- !is.na(cohort[[v]])
    x <- cohort[[v]][keep]
    g <- group[keep]
    observed <- expected <- NULL
    planned_method <- if (v %in% c("Decimal.Chronological.Age", "BMI")) {
      "two-sided Wilcoxon rank-sum test (exact = FALSE, correct = TRUE)"
    } else if (v == "Stress.Level") {
      "two-sided Welch two-sample t-test (var.equal = FALSE)"
    } else {
      observed <- categorical_tables[[v]]
      expected <- expected_tables[[v]]
      "Pearson chi-square test (correct = FALSE)"
    }
    test <- tryCatch({
      if (v %in% c("Decimal.Chronological.Age", "BMI")) {
        stats::wilcox.test(x[g == "slow"], x[g == "fast"],
                          alternative = "two.sided", exact = FALSE, correct = TRUE)
      } else if (v == "Stress.Level") {
        stats::t.test(x[g == "slow"], x[g == "fast"],
                      alternative = "two.sided", var.equal = FALSE)
      } else {
        stats::chisq.test(observed, correct = FALSE, simulate.p.value = FALSE)
      }
    }, error = function(e) {
      fail("The planned ", planned_method, " failed for ", v, ": ",
           conditionMessage(e), ". No alternative test was substituted.")
    })
    if (length(test$p.value) != 1L || !is.finite(test$p.value) ||
        test$p.value < 0 || test$p.value > 1) {
      fail("The planned ", planned_method, " for ", v,
           " did not produce a valid p-value. No alternative test was substituted.")
    }
    row <- data.frame(
      variable = v, label = unname(labels[v]), p_value = unname(test$p.value),
      method = test$method, n_slow = sum(g == "slow"), n_fast = sum(g == "fast"),
      n_missing_slow = sum(!keep & group == "slow"),
      n_missing_fast = sum(!keep & group == "fast"), stringsAsFactors = FALSE
    )
    row$test_object <- I(list(test))
    row$observed_counts <- I(list(observed))
    row$expected_counts <- I(list(expected))
    row
  })
  comparisons <- do.call(rbind, rows)
  rownames(comparisons) <- NULL
  attr(comparisons, "cohort_path") <- normalizePath(cohort_path, mustWork = TRUE)
  attr(comparisons, "group_definitions") <- c(slow = "DunedinPACE <1.0", fast = "DunedinPACE >=1.0")
  attr(comparisons, "cohort_group_counts") <- group_counts
  attr(comparisons, "category_label_normalization") <- list(
    rule = "Curly single quotation marks (U+2018/U+2019) converted to straight apostrophes (U+0027); no substantive categories pooled.",
    changed_values_by_variable = normalization_counts
  )
  attr(comparisons, "purpose") <- "Unadjusted descriptive baseline comparisons; no multiplicity adjustment."
  attr(comparisons, "variable_diagnostics") <- setNames(lapply(variables, function(v) {
    describe_baseline_variable(cohort[[v]], group, v, input_types[[v]],
                               category_levels = allowed_levels[[v]])
  }), variables)
  comparisons
}

describe_baseline_variable <- function(x, group, variable, input_type,
                                       category_levels = NULL) {
  # Retain aggregate diagnostics, never participant-level data, with the results.
  # Measurement scale is distinct from R storage type and observed granularity.
  measurement_scales <- c(
    Decimal.Chronological.Age = "continuous (chronological age in years)",
    Biological.Sex = "categorical, nominal, binary",
    Level.of.Education = "categorical (qualifications; a strict ordering needs substantive justification)",
    Tobacco.Use.Binary = "categorical, nominal, binary",
    Alcohol.per.week = "categorical, ordered frequency categories; equal spacing is not assumed",
    Exercise.per.week = "categorical, ordered frequency categories; equal spacing is not assumed",
    BMI = "continuous (kg/m^2)",
    Stress.Level = "bounded 0-10 score; ordinal versus interval-scale interpretation requires survey documentation",
    Hours.of.sleep.per.night = "categorical, ordered duration bands; equal spacing is not assumed",
    Ethnicity.Recoded = "categorical, nominal"
  )
  lines <- c(paste0("  R representation before label normalization: ", input_type),
             paste0("  Measurement scale: ", measurement_scales[[variable]]))
  add <- function(...) lines <<- c(lines, paste0(...))
  fmt <- function(z) formatC(z, format = "g", digits = 5)
  pct <- function(n, total) {
    if (total == 0L) return("NA")
    paste0(formatC(100 * n / total, format = "f", digits = 1), "%")
  }
  subsets <- list(overall = rep(TRUE, length(x)), slow = group == "slow",
                  fast = group == "fast")
  for (s in names(subsets)) {
    values <- x[subsets[[s]]]
    n_missing <- sum(is.na(values))
    z <- values[!is.na(values)]
    add("  ", s, ": n_total=", length(values), "; non_missing=", length(z),
        "; NA_including_NaN=", n_missing, " (", pct(n_missing, length(values)),
        "); unique_non_missing=", length(unique(z)))
    if (is.null(category_levels)) {
      finite <- z[is.finite(z)]
      add("    NaN=", sum(is.nan(values)), "; infinite=", sum(is.infinite(values)))
      if (!length(finite)) {
        add("    Numeric summaries unavailable: no finite observations.")
        next
      }
      q <- stats::quantile(finite, c(0, .01, .05, .25, .5, .75, .95, .99, 1),
                           names = FALSE, type = 7)
      centered <- finite - mean(finite)
      m2 <- mean(centered^2)
      skewness <- if (m2 > 0) mean(centered^3) / m2^1.5 else NA_real_
      frequencies <- tabulate(match(finite, unique(finite)))
      n_tied <- sum(frequencies[frequencies > 1L])
      spread <- q[6] - q[4]
      lower_fence <- q[4] - 1.5 * spread
      upper_fence <- q[6] + 1.5 * spread
      add("    Observed granularity: ",
          if (all(finite == trunc(finite))) "integer-valued (discrete observed values)" else "fractional values present",
          "; observations in tied values=", n_tied, " (", pct(n_tied, length(finite)), ")")
      add("    Range=[", fmt(q[1]), ", ", fmt(q[9]), "]; mean=", fmt(mean(finite)),
          "; SD=", fmt(stats::sd(finite)), "; median=", fmt(q[5]), "; IQR=", fmt(spread),
          "; skewness=", fmt(skewness))
      add("    Quantiles: p01=", fmt(q[2]), "; p05=", fmt(q[3]), "; p25=", fmt(q[4]),
          "; p50=", fmt(q[5]), "; p75=", fmt(q[6]), "; p95=", fmt(q[7]), "; p99=", fmt(q[8]))
      add("    Tukey fences=[", fmt(lower_fence), ", ", fmt(upper_fence),
          "]; n_below=", sum(finite < lower_fence), "; n_above=", sum(finite > upper_fence))
    }
  }

  # Print all survey categories, including unobserved ones. For numeric variables
  # with few distinct values (e.g. stress), show the complete value distribution.
  displayed_values <- if (!is.null(category_levels)) {
    add("  Range: not numeric; survey categories are listed below.")
    unobserved <- setdiff(category_levels, unique(x[!is.na(x)]))
    add("  Survey categories=", length(category_levels),
        "; unobserved categories omitted from test=", length(unobserved),
        if (length(unobserved)) paste0(" (", paste(unobserved, collapse = "; "), ")") else "")
    category_levels
  } else {
    values <- sort(unique(x[!is.na(x) & is.finite(x)]))
    if (length(values) <= 20L) values else NULL
  }
  if (length(displayed_values)) {
    add("  Frequencies: n (% of non-missing observations in the indicated group)")
    for (value in displayed_values) {
      counts <- vapply(subsets, function(index) {
        z <- x[index & !is.na(x)]
        n <- sum(z == value)
        paste0(n, " (", pct(n, length(z)), ")")
      }, character(1))
      add("    ", as.character(value), ": ",
          paste(paste0(names(counts), "=", counts), collapse = "; "))
    }
  }
  lines
}

print_baseline_significant <- function(x) {
  # Write directly to stderr, not to captured chunk output or a rendered table.
  # Keep the full object (including nonsignificant and unrounded p-values) intact.
  required <- c("variable", "label", "p_value", "method", "n_slow", "n_fast",
                "observed_counts", "expected_counts")
  if (!is.data.frame(x) || nrow(x) != 10L || !all(required %in% names(x)) ||
      any(!is.finite(x$p_value))) {
    stop("Expected the complete ten-comparison baseline results object.", call. = FALSE)
  }
  diagnostics <- attr(x, "variable_diagnostics", exact = TRUE)
  if (!is.list(diagnostics) || !all(x$variable %in% names(diagnostics))) {
    stop("Variable diagnostics are missing. Recreate the results with run_table1_descriptive_tests() before printing.",
         call. = FALSE)
  }
  write_line <- function(...) cat(..., "\n", sep = "", file = stderr())
  counts <- attr(x, "cohort_group_counts", exact = TRUE)
  normalization <- attr(x, "category_label_normalization", exact = TRUE)
  write_line("\n=== TABLE 1: VARIABLE DIAGNOSTICS FOR TEST REVIEW ===")
  write_line("Saved analysis cohort: n=", sum(counts), "; n_slow=", counts["slow"],
             "; n_fast=", counts["fast"])
  write_line("Groups: slow = DunedinPACE <1.0; fast = DunedinPACE >=1.0.")
  write_line("Missingness below concerns the saved analysis cohort, after upstream exclusions, not the original sample.")
  write_line("Main variables must be complete; missing ethnicity is omitted only from its comparison.")
  write_line("Numeric summaries use finite observations; quantiles use R type 7; skewness is the standardized third central moment.")
  write_line("Tied observations share a value with at least one other observation in the indicated group.")
  write_line("Tukey fences are Q1 - 1.5*IQR and Q3 + 1.5*IQR; values outside them are not automatically errors or excluded.")
  write_line("Design check: confirm independent participants and no pairing, repeated measurements or unaccounted clustering.")
  write_line("Measurement-scale interpretation and independence cannot be established from these summaries alone.")
  for (i in seq_len(nrow(x))) {
    v <- x$variable[i]
    write_line("\n", x$label[i], " [", v, "]")
    write_line("  Current test: ", x$method[i])
    write_line(paste(diagnostics[[v]], collapse = "\n"))
    observed <- x$observed_counts[[i]]
    expected <- x$expected_counts[[i]]
    if (!is.null(expected)) {
      write_line("  Category labels normalized (apostrophes only): n=",
                 normalization$changed_values_by_variable[v])
      write_line("  Contingency table used in test: ", nrow(observed), " categories x ",
                 ncol(observed), " groups; observed zero cells=", sum(observed == 0))
      write_line("  Expected counts under independence (row total * column total / grand total):")
      for (j in seq_len(nrow(expected))) {
        write_line("    ", rownames(expected)[j], ": slow=",
                   formatC(expected[j, "slow"], format = "f", digits = 3),
                   "; fast=", formatC(expected[j, "fast"], format = "f", digits = 3))
      }
      write_line("  Expected-cell check: minimum=", formatC(min(expected), format = "f", digits = 3),
                 "; cells <1=", sum(expected < 1), "/", length(expected),
                 "; cells <5=", sum(expected < 5), "/", length(expected),
                 " (", formatC(100 * mean(expected < 5), format = "f", digits = 1), "%).")
      write_line("  Script criterion: all expected counts >=5; Pearson test uses correct=FALSE and treats categories as nominal.")
    } else if (v == "Stress.Level") {
      write_line("  Test target: mean score difference; Welch allows unequal variances. Review whether mean comparisons suit this score.")
    } else {
      write_line("  Test target: rank-based group comparison (two-sided, exact=FALSE, correct=TRUE); a median-shift interpretation needs similar distribution shapes.")
    }
  }
  write_line("\n=== SIGNIFICANT UNADJUSTED COMPARISONS (p <0.05) ===")
  write_line("All ten variables were described above; only significant comparisons are listed below. No multiplicity adjustment.")
  significant <- x[x$p_value < 0.05, , drop = FALSE]
  for (i in seq_len(nrow(significant))) {
    cat(significant$label[i], ": p <0.05; ", significant$method[i],
        "; n_slow=", significant$n_slow[i], ", n_fast=", significant$n_fast[i],
        "\n", sep = "", file = stderr())
  }
  invisible(x)
}
