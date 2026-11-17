# Presentation summaries of saved estimates only. Sourcing this file defines
# functions; it never loads participant data, fits models, or starts a pipeline.

review_number <- function(n, capital = FALSE) {
  if (length(n) != 1L || !is.numeric(n) || !is.finite(n) || n < 0 || n != floor(n))
    stop("A manuscript count must be one nonnegative integer.", call. = FALSE)
  words <- c("zero", "one", "two", "three", "four", "five", "six", "seven",
             "eight", "nine", "ten", "eleven", "twelve")
  value <- if (n <= 12L) words[n + 1L] else format(n, scientific = FALSE, trim = TRUE)
  if (capital) value <- paste0(toupper(substr(value, 1L, 1L)), substring(value, 2L))
  value
}

review_required_columns <- function(x, fields, label) {
  if (!is.data.frame(x) || anyDuplicated(names(x)) || !all(fields %in% names(x)))
    stop(label, " requires uniquely named columns: ", paste(fields, collapse = ", "),
         call. = FALSE)
  if (!nrow(x)) stop(label, " contains no results.", call. = FALSE)
}

review_finite_columns <- function(x, fields, label) {
  invalid <- fields[!vapply(x[fields], function(v) is.numeric(v) && !is.complex(v) &&
                             all(is.finite(v)), logical(1))]
  if (length(invalid)) stop(label, " has invalid numeric values in: ",
                            paste(invalid, collapse = ", "), call. = FALSE)
}

review_intervention_ids <- function() c(
  "Antiaging.Exosome", "Antiaging.Hormone.Replacement", "Antiaging.NAD",
  "Antiaging.Senolytics", "Antiaging.Stem.Cell", "Antioxidants", "DHEA",
  "Fish.oil.omega", "Herbal.supplements", "Metformin", "Minerals", "NR.NAD",
  "Rapamycin", "Resveratrol", "Sulphoraphane.broccoli.extract", "TA.65",
  "Vitamins", "Workout.supplements"
)

review_match_methods <- function(ipw, gcomp, label) {
  validate <- function(x, method) {
    fields <- c("treatment", "outcome", "ate_estimate", "ci_lower", "ci_upper",
                "n_treated", "n_control")
    review_required_columns(x, fields, paste(label, method))
    if (anyNA(x[c("treatment", "outcome")]) || any(!nzchar(trimws(x$treatment))) ||
        anyDuplicated(x$treatment) || any(x$outcome != "DunedinPACE"))
      stop(label, " ", method, " requires one DunedinPACE row per intervention.", call. = FALSE)
    review_finite_columns(x, c("ate_estimate", "ci_lower", "ci_upper", "n_treated", "n_control"),
                          paste(label, method))
    for (field in c("n_treated", "n_control")) {
      if (any(x[[field]] <= 0 | x[[field]] != floor(x[[field]])))
        stop(label, " ", method, " has invalid group sample sizes.", call. = FALSE)
    }
    if (any(x$ci_lower > x$ci_upper))
      stop(label, " ", method, " has reversed confidence limits.", call. = FALSE)
    x
  }
  ipw <- validate(ipw, "IPW")
  gcomp <- validate(gcomp, "G-computation")
  if (!setequal(ipw$treatment, gcomp$treatment))
    stop(label, " intervention identifiers differ between methods.", call. = FALSE)
  gcomp <- gcomp[match(ipw$treatment, gcomp$treatment), , drop = FALSE]
  if (any(ipw$n_treated != gcomp$n_treated | ipw$n_control != gcomp$n_control))
    stop(label, " group sample sizes differ between methods.", call. = FALSE)
  list(IPW = ipw, Gcomp = gcomp)
}

summarize_main_review <- function(ipw, gcomp) {
  matched <- review_match_methods(ipw, gcomp, "Main results")
  ipw <- matched$IPW
  gcomp <- matched$Gcomp
  if (!setequal(ipw$treatment, review_intervention_ids()))
    stop("The saved main intervention set differs from the reviewed 18 interventions; review the manuscript before knitting.",
         call. = FALSE)
  lower <- ipw$ci_upper < 0 & gcomp$ci_upper < 0
  higher <- ipw$ci_lower > 0 & gcomp$ci_lower > 0
  if (!setequal(c("Sulphoraphane.broccoli.extract", "NR.NAD", "Antioxidants", "Fish.oil.omega",
                  "Resveratrol", "Minerals", "Herbal.supplements", "Vitamins"), ipw$treatment[lower]) ||
      !setequal(c("Metformin", "Antiaging.Exosome", "Antiaging.Stem.Cell"), ipw$treatment[higher]))
    stop("Named main-analysis findings changed; review the intervention-specific wording before knitting.",
         call. = FALSE)
  workout <- match("Workout.supplements", ipw$treatment)
  if (!(ipw$ate_estimate[workout] < 0 && ipw$ci_lower[workout] < 0 &&
        ipw$ci_upper[workout] >= 0 &&
        gcomp$ci_upper[workout] < 0))
    stop("Workout-supplement findings changed; review the IPW/G-computation wording before knitting.",
         call. = FALSE)
  list(n_interventions = nrow(ipw), n_lower = sum(lower), n_higher = sum(higher),
       lower_ids = as.character(ipw$treatment[lower]),
       higher_ids = as.character(ipw$treatment[higher]), matched = matched)
}

review_balance_flags <- function(x, label) {
  review_required_columns(x, "SMD", label)
  smd <- as.character(x$SMD)
  if (anyNA(smd) || any(!nzchar(trimws(smd))))
    stop(label, " has missing balance summaries.", call. = FALSE)
  # Existing analysis exports use '<0.1' for balanced rows; otherwise SMD lists
  # the covariates with absolute post-weighting SMD > 0.1.
  trimws(smd) != "<0.1"
}

summarize_mutual_review <- function(ipw, gcomp) {
  matched <- review_match_methods(ipw, gcomp, "Mutually adjusted results")
  ipw <- matched$IPW
  gcomp <- matched$Gcomp
  expected <- c("Antioxidants", "Fish.oil.omega", "Workout.supplements",
    "Herbal.supplements", "Vitamins", "Minerals", "NR.NAD",
    "Sulphoraphane.broccoli.extract", "Resveratrol", "Metformin",
    "Antiaging.Exosome", "Antiaging.Stem.Cell")
  if (!setequal(ipw$treatment, expected))
    stop("The mutually adjusted intervention set differs from the reviewed fixed set of 12.", call. = FALSE)
  lower <- ipw$ci_upper < 0 & gcomp$ci_upper < 0
  imbalanced <- review_balance_flags(ipw, "Mutually adjusted results")
  if (!setequal(ipw$treatment[lower], c("Fish.oil.omega", "Antioxidants")) ||
      !setequal(ipw$treatment[lower & imbalanced], "Antioxidants"))
    stop("Mutually adjusted findings or balance changed; review the fish oil/antioxidant wording.", call. = FALSE)
  list(n_interventions = nrow(ipw), n_lower = sum(lower),
       lower_ids = as.character(ipw$treatment[lower]),
       lower_imbalanced_ids = as.character(ipw$treatment[lower & imbalanced]),
       n_imbalanced = sum(imbalanced), matched = matched)
}

summarize_combination_review <- function(results) {
  fields <- c("treatment", "outcome", "exposure_type", "label", "ci_lower_ipw", "ci_upper_ipw",
              "ci_lower_std", "ci_upper_std", "SMD")
  review_required_columns(results, fields, "Combination results")
  if (anyNA(results[c("treatment", "outcome", "exposure_type", "label")]) ||
      any(!nzchar(trimws(results$label))) || anyDuplicated(results$treatment) ||
      any(results$outcome != "DunedinPACE") ||
      any(!results$exposure_type %in% c("Exact combination", "Individual intervention (main)")))
    stop("Combination results have missing, duplicate, or unexpected exposure identifiers.", call. = FALSE)
  x <- results[results$exposure_type == "Exact combination", , drop = FALSE]
  review_required_columns(x, fields, "Exact-combination results")
  if (nrow(x) != 10L)
    stop("The exact-combination set differs from the reviewed ten comparisons; review the manuscript.", call. = FALSE)
  review_finite_columns(x, c("ci_lower_ipw", "ci_upper_ipw", "ci_lower_std", "ci_upper_std"),
                        "Exact-combination results")
  if (any(x$ci_lower_ipw > x$ci_upper_ipw | x$ci_lower_std > x$ci_upper_std))
    stop("Exact-combination results have reversed confidence limits.", call. = FALSE)
  lower <- x$ci_upper_ipw < 0 & x$ci_upper_std < 0
  higher <- x$ci_lower_ipw > 0 & x$ci_lower_std > 0
  imbalanced <- review_balance_flags(x, "Exact-combination results")
  excludes_both <- (x$ci_upper_ipw < 0 | x$ci_lower_ipw > 0) &
    (x$ci_upper_std < 0 | x$ci_lower_std > 0)
  expected_lower_label <- paste0("Exact: Antioxidants + Fish Oil / Omega + Workout Supplements + ",
                                 "Herbal Supplements + Vitamins + Minerals")
  expected_higher_label <- "Exact: Herbal Supplements + Vitamins"
  if (!setequal(x$label[lower], expected_lower_label) ||
      !setequal(x$label[higher], expected_higher_label) ||
      any(excludes_both != (lower | higher)) || !all(imbalanced[excludes_both]))
    stop("Named exact-combination findings or their balance changed; review the combination wording.", call. = FALSE)
  list(n_combinations = nrow(x), n_excluding_zero = sum(excludes_both),
       n_lower = sum(lower), n_higher = sum(higher), n_imbalanced = sum(imbalanced),
       lower_ids = as.character(x$treatment[lower]), higher_ids = as.character(x$treatment[higher]),
       imbalanced_ids = as.character(x$treatment[imbalanced]), results = x)
}

review_biomarker_ids <- function() c(
  "DNAmFitAge", "DNAmVO2max", "DunedinPACE", "GrimAge.PC", "Hannum.PC",
  "Horvath.PC", "OMICmAge", "PhenoAge.PC", "SystemsAge", "SystemsAge.Blood",
  "SystemsAge.Brain", "SystemsAge.Heart", "SystemsAge.Hormone", "SystemsAge.Immune",
  "SystemsAge.Inflammation", "SystemsAge.Kidney", "SystemsAge.Liver", "SystemsAge.Lung",
  "SystemsAge.Metabolic", "SystemsAge.MusculoSkeletal", "Telomere.Values.PC"
)

# Extract all identical branches, screen every pair in each branch, and retain
# maximal qualifying groups. These correlations describe saved effect profiles.
review_shared_groups <- function(ipw, gcomp, r_threshold = 0.80) {
  branches <- function(x) {
    tree <- stats::hclust(stats::dist(x, method = "euclidean"), method = "complete")
    membership <- vector("list", nrow(tree$merge))
    for (i in seq_along(membership)) {
      membership[[i]] <- sort(unlist(lapply(tree$merge[i, ], function(child) {
        if (child < 0L) -child else membership[[child]]
      }), use.names = FALSE))
    }
    names(membership) <- vapply(membership, paste, character(1), collapse = ",")
    membership
  }
  if (any(apply(ipw, 1L, stats::sd) == 0) || any(apply(gcomp, 1L, stats::sd) == 0))
    stop("A heatmap profile has zero variance; shared-cluster claims need review.", call. = FALSE)
  ipw_branches <- branches(ipw)
  gcomp_branches <- branches(gcomp)
  shared <- ipw_branches[names(ipw_branches) %in% names(gcomp_branches)]
  r_ipw <- stats::cor(t(ipw), method = "pearson")
  r_gcomp <- stats::cor(t(gcomp), method = "pearson")
  qualifies <- vapply(shared, function(members) {
    a <- r_ipw[members, members, drop = FALSE]
    b <- r_gcomp[members, members, drop = FALSE]
    a <- a[upper.tri(a)]
    b <- b[upper.tri(b)]
    all(is.finite(a)) && all(is.finite(b)) && all(a >= r_threshold) && all(b >= r_threshold)
  }, logical(1))
  qualifying <- shared[qualifies]
  maximal <- vapply(qualifying, function(members) {
    !any(vapply(qualifying, function(other) length(other) > length(members) &&
                  all(members %in% other), logical(1)))
  }, logical(1))
  lapply(qualifying[maximal], function(members) rownames(ipw)[members])
}

summarize_heatmap_review <- function(bundle_dir) {
  if (!requireNamespace("readxl", quietly = TRUE))
    stop("Reading the saved heatmap workbooks requires readxl.", call. = FALSE)
  expected_biomarkers <- review_biomarker_ids()
  expected_treatments <- c(review_intervention_ids(), "Exercise.1to2.per.week_vs_Never",
    "Exercise.3to4.per.week_vs_Never", "Exercise.5to7.per.week_vs_Never",
    "Exercise.8plus.per.week_vs_Never")
  read_estimates <- function(filename, method) {
    path <- file.path(bundle_dir, filename)
    if (!file.exists(path)) stop("Missing saved heatmap workbook: ", path, call. = FALSE)
    x <- as.data.frame(readxl::read_excel(path, sheet = "Estimates"))
    required <- c("biomarker", "treatment", "method", "ate_estimate", "ci_lower", "ci_upper",
      "heatmap_ate", "sign_multiplier", "ci_excludes_zero", "n_bootstrap",
      "n_bootstrap_requested", "n_treated", "n_control", "data_kind", "Pearson\u2019s r")
    review_required_columns(x, required, method)
    if (anyNA(x[c("biomarker", "treatment", "method", "data_kind")]) ||
        any(x$method != method) || any(x$data_kind != "real") ||
        anyDuplicated(x[c("biomarker", "treatment")]))
      stop(method, " heatmap workbook must contain unique real-data cells with valid identifiers.", call. = FALSE)
    if (!setequal(x$biomarker, expected_biomarkers) ||
        !setequal(x$treatment, expected_treatments) ||
        nrow(x) != length(expected_biomarkers) * length(expected_treatments))
      stop(method, " heatmap panel differs from the reviewed 21 biomarkers and 22 intervention/exercise profiles; review the manuscript before knitting.",
           call. = FALSE)
    numeric_fields <- c("ate_estimate", "ci_lower", "ci_upper", "heatmap_ate",
      "sign_multiplier", "n_bootstrap", "n_bootstrap_requested", "n_treated", "n_control", "Pearson\u2019s r")
    review_finite_columns(x, numeric_fields, method)
    multiplier <- ifelse(x$biomarker %in% c("Telomere.Values.PC", "DNAmVO2max"), -1, 1)
    if (any(x$sign_multiplier != multiplier) ||
        any(abs(x$heatmap_ate - x$ate_estimate * multiplier) > 1e-12) ||
        any(x$ci_lower > x$ci_upper))
      stop(method, " heatmap display directions or confidence limits are inconsistent.", call. = FALSE)
    if (any(x$n_bootstrap != 1000 | x$n_bootstrap_requested != 1000))
      stop(method, " heatmap bootstrap counts do not match the confirmed 1,000 completed iterations.", call. = FALSE)
    flag <- as.character(x$ci_excludes_zero)
    if (anyNA(flag) || any(!flag %in% c("TRUE", "FALSE", "1", "0")))
      stop(method, " has invalid CI-exclusion flags.", call. = FALSE)
    significant <- x$ci_upper < 0 | x$ci_lower > 0
    if (any(significant != (flag %in% c("TRUE", "1"))))
      stop(method, " CI-exclusion flags disagree with the unrounded confidence limits.", call. = FALSE)
    x$ci_excludes_zero <- significant
    x
  }
  ipw <- read_estimates("Supplementary_Data_S1_IPW.xlsx", "IPW")
  gcomp <- read_estimates("Supplementary_Data_S2_G-comp.xlsx", "G-comp")
  # Explicitly align identifiers before comparing methods, regardless of row order.
  cell_key <- function(x) paste(x$biomarker, x$treatment, sep = "\r")
  gcomp <- gcomp[match(cell_key(ipw), cell_key(gcomp)), , drop = FALSE]
  if (!identical(as.character(ipw$biomarker), as.character(gcomp$biomarker)) ||
      !identical(as.character(ipw$treatment), as.character(gcomp$treatment)))
    stop("Saved heatmap identifiers do not match between methods.", call. = FALSE)
  metformin <- function(x, method) {
    x <- x[x$treatment == "Metformin", , drop = FALSE]
    favorable <- x$heatmap_ate < 0
    unfavorable <- x$heatmap_ate > 0
    if (any(x$heatmap_ate == 0) || !setequal(x$biomarker[favorable],
        c("Horvath.PC", "PhenoAge.PC", "SystemsAge.Immune")))
      stop(method, " metformin direction classifications changed; review the biomarker wording.", call. = FALSE)
    expected_significant <- if (method == "IPW") character() else "Horvath.PC"
    if (!setequal(x$biomarker[favorable & x$ci_excludes_zero], expected_significant))
      stop(method, " favorable metformin CI classifications changed; review the uncertainty wording.", call. = FALSE)
    list(favorable = sum(favorable), favorable_not_significant = sum(favorable & !x$ci_excludes_zero),
      unfavorable = sum(unfavorable), unfavorable_not_significant = sum(unfavorable & !x$ci_excludes_zero),
      results = x)
  }
  for (x in list(ipw, gcomp)) {
    senolytics <- x[x$treatment == "Antiaging.Senolytics", , drop = FALSE]
    if (any(senolytics$ci_excludes_zero) || !any(senolytics$heatmap_ate < 0) ||
        !any(senolytics$heatmap_ate > 0))
      stop("Senolytic heatmap findings changed; review the mixed-direction/uncertainty wording.", call. = FALSE)
  }
  age_correlations <- function(x) {
    values <- lapply(split(x[["Pearson\u2019s r"]], x$biomarker), unique)
    if (any(lengths(values) != 1L) || any(abs(unlist(values)) > 1))
      stop("Saved biomarker age correlations are inconsistent.", call. = FALSE)
    unlist(values)
  }
  age_ipw <- age_correlations(ipw)
  age_gcomp <- age_correlations(gcomp)
  if (!isTRUE(all.equal(age_ipw, age_gcomp, tolerance = 1e-12)))
    stop("Saved biomarker age correlations differ between methods.", call. = FALSE)
  if (!setequal(names(age_ipw)[age_ipw < 0], c("Telomere.Values.PC", "DNAmVO2max")))
    stop("Negative biomarker age correlations changed; review the residualization wording.", call. = FALSE)
  other <- abs(age_ipw[names(age_ipw) != "DunedinPACE"])
  build_matrix <- function(x) {
    mat <- matrix(NA_real_, length(expected_treatments), length(expected_biomarkers),
      dimnames = list(unique(ipw$treatment), unique(ipw$biomarker)))
    mat[cbind(match(x$treatment, rownames(mat)), match(x$biomarker, colnames(mat)))] <- x$heatmap_ate
    mat
  }
  ipw_matrix <- build_matrix(ipw)
  gcomp_matrix <- build_matrix(gcomp)
  clusters <- list(biomarkers = review_shared_groups(t(ipw_matrix), t(gcomp_matrix)),
                   interventions = review_shared_groups(ipw_matrix, gcomp_matrix))
  expected_clusters <- list(
    biomarkers = list(c("DNAmFitAge", "SystemsAge.Blood", "SystemsAge.Liver"),
      c("SystemsAge.Inflammation", "SystemsAge.Metabolic"), c("SystemsAge.Heart", "SystemsAge"),
      c("SystemsAge.Brain", "SystemsAge.Kidney"), c("GrimAge.PC", "SystemsAge.Lung")),
    interventions = list(c("NR.NAD", "Antioxidants"), c("Herbal.supplements", "Minerals")))
  group_keys <- function(groups) vapply(groups, function(x) paste(sort(x), collapse = "\r"), character(1))
  for (axis in names(clusters)) {
    if (!setequal(group_keys(clusters[[axis]]), group_keys(expected_clusters[[axis]])))
      stop("The complete maximal shared ", axis, " groups changed at Pearson r >= 0.80; review the exhaustive clustering statement.",
           call. = FALSE)
  }
  list(n_biomarkers = length(expected_biomarkers), n_profiles = length(expected_treatments),
    metformin = list(IPW = metformin(ipw, "IPW"), Gcomp = metformin(gcomp, "G-comp")),
    age_correlations = list(other_min_abs = min(other), other_max_abs = max(other),
                           dunedin = unname(age_ipw["DunedinPACE"])),
    clusters = clusters, estimates = list(IPW = ipw, Gcomp = gcomp))
}
