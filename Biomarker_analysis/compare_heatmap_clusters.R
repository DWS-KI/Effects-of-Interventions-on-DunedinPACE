# Standalone comparison of the published heatmap estimates; never starts run.R.
# From the repository root:
# Rscript --vanilla Biomarker_analysis/compare_heatmap_clusters.R [r_threshold]
# Or source this file, then call compare_heatmap_clusters(r_threshold = 0.80).

heatmap_comparison_threshold <- function(r_threshold) {
  if (!is.numeric(r_threshold) || is.complex(r_threshold) || length(r_threshold) != 1L ||
      !is.finite(r_threshold) || r_threshold <= 0 || r_threshold > 1)
    stop("r_threshold must be one finite number greater than 0 and at most 1.", call. = FALSE)
  invisible(r_threshold)
}

heatmap_comparison_matrices <- function(ipw, gcomp) {
  validate <- function(x, method) {
    required <- c("biomarker", "treatment", "method", "heatmap_ate")
    if (!is.data.frame(x) || !all(required %in% names(x)) || anyDuplicated(names(x)))
      stop(method, " CSV must have unique column names including: ",
           paste(required, collapse = ", "), call. = FALSE)
    if (!nrow(x)) stop(method, " CSV contains no estimates.", call. = FALSE)
    for (field in c("biomarker", "treatment", "method")) {
      x[[field]] <- as.character(x[[field]])
      if (anyNA(x[[field]]) || any(!nzchar(trimws(x[[field]]))))
        stop(method, " CSV has missing or empty ", field, " identifiers.", call. = FALSE)
    }
    if (any(x$method != method))
      stop(method, " CSV contains unexpected method identifiers.", call. = FALSE)
    if (anyDuplicated(x[c("biomarker", "treatment")]))
      stop(method, " CSV contains duplicate biomarker-treatment cells.", call. = FALSE)
    if (!is.numeric(x$heatmap_ate) || any(!is.finite(x$heatmap_ate)))
      stop(method, " heatmap_ate must contain only finite numeric estimates.", call. = FALSE)
    x
  }
  ipw <- validate(ipw, "IPW")
  gcomp <- validate(gcomp, "G-comp")
  biomarkers <- unique(ipw$biomarker)
  treatments <- unique(ipw$treatment)
  if (!setequal(biomarkers, gcomp$biomarker) || !setequal(treatments, gcomp$treatment))
    stop("IPW and G-comp must contain the same biomarker and treatment identifiers.", call. = FALSE)
  build <- function(x, method) {
    estimates <- matrix(NA_real_, length(treatments), length(biomarkers),
      dimnames = list(treatments, biomarkers))
    estimates[cbind(match(x$treatment, treatments), match(x$biomarker, biomarkers))] <- x$heatmap_ate
    if (any(!is.finite(estimates)))
      stop(method, " CSV has an incomplete biomarker-treatment matrix.", call. = FALSE)
    estimates
  }
  list(IPW = build(ipw, "IPW"), `G-comp` = build(gcomp, "G-comp"))
}

# Sorted integer memberships avoid relying on leaf order or delimiters in names.
heatmap_comparison_branches <- function(tree) {
  if (is.null(tree)) return(list())
  branches <- vector("list", nrow(tree$merge))
  for (i in seq_along(branches)) {
    members <- lapply(tree$merge[i, ], function(child) {
      if (child < 0L) -child else branches[[child]]
    })
    branches[[i]] <- sort(as.integer(unlist(members, use.names = FALSE)))
  }
  names(branches) <- vapply(branches, paste, character(1), collapse = ",")
  branches
}

heatmap_comparison_empty_clusters <- function() {
  data.frame(axis = character(), cluster_id = character(), members = character(),
    n_members = integer(), n_profile_coordinates = integer(),
    min_r_ipw = numeric(), mean_r_ipw = numeric(), min_r_gcomp = numeric(),
    mean_r_gcomp = numeric(), sign_agreement = numeric(), is_maximal = logical(),
    stringsAsFactors = FALSE)
}

heatmap_comparison_empty_pairs <- function() {
  data.frame(axis = character(), member_1 = character(), member_2 = character(),
    n_profile_coordinates = integer(), r_ipw = numeric(), r_gcomp = numeric(),
    min_r_both = numeric(), stringsAsFactors = FALSE)
}

# Rows are the items being compared; columns are the coordinates of their profiles.
heatmap_comparison_axis <- function(ipw, gcomp, axis, r_threshold) {
  heatmap_comparison_threshold(r_threshold)
  stopifnot(is.matrix(ipw), is.matrix(gcomp), identical(dimnames(ipw), dimnames(gcomp)),
            identical(dim(ipw), dim(gcomp)), all(is.finite(ipw)), all(is.finite(gcomp)))
  clusters <- heatmap_comparison_empty_clusters()
  pairs <- heatmap_comparison_empty_pairs()
  n <- nrow(ipw)
  coordinates <- ncol(ipw)
  diagnostics <- data.frame(axis = axis, n_members = n, n_profile_coordinates = coordinates,
    shared_branches = 0L, branches_undefined = 0L, branches_below_threshold = 0L,
    qualifying_branches = 0L, maximal_clusters = 0L, total_pairs = choose(n, 2),
    pairs_undefined = 0L, pairs_below_threshold = 0L, qualifying_pairs = 0L,
    undefined_profiles_ipw = "", undefined_profiles_gcomp = "", status = "screened",
    stringsAsFactors = FALSE)
  if (n < 2L || coordinates < 3L) {
    diagnostics$status <- if (n < 2L) "Skipped: fewer than two members." else
      "Skipped: fewer than three profile coordinates for Pearson correlation."
    return(list(clusters = clusters, pairs = pairs, diagnostics = diagnostics))
  }

  # Keep all profiles in the Euclidean trees, including constant profiles.
  tree <- function(x) stats::hclust(stats::dist(x, method = "euclidean"), method = "complete")
  ipw_branches <- heatmap_comparison_branches(tree(ipw))
  gcomp_branches <- heatmap_comparison_branches(tree(gcomp))
  shared <- ipw_branches[names(ipw_branches) %in% names(gcomp_branches)]
  diagnostics$shared_branches <- length(shared)

  correlations <- function(x) {
    deviations <- apply(x, 1L, stats::sd)
    defined <- is.finite(deviations) & deviations > 0
    r <- matrix(NA_real_, nrow(x), nrow(x), dimnames = list(rownames(x), rownames(x)))
    if (any(defined)) r[defined, defined] <- stats::cor(t(x[defined, , drop = FALSE]), method = "pearson")
    list(r = r, undefined = rownames(x)[!defined])
  }
  ipw_cor <- correlations(ipw)
  gcomp_cor <- correlations(gcomp)
  diagnostics$undefined_profiles_ipw <- paste(ipw_cor$undefined, collapse = "; ")
  diagnostics$undefined_profiles_gcomp <- paste(gcomp_cor$undefined, collapse = "; ")
  indices <- which(upper.tri(ipw_cor$r), arr.ind = TRUE)
  ri <- ipw_cor$r[indices]
  rg <- gcomp_cor$r[indices]
  defined <- is.finite(ri) & is.finite(rg)
  keep <- defined & ri >= r_threshold & rg >= r_threshold
  diagnostics$pairs_undefined <- sum(!defined)
  diagnostics$pairs_below_threshold <- sum(defined & !keep)
  diagnostics$qualifying_pairs <- sum(keep)
  if (any(keep)) {
    pairs <- data.frame(axis = axis, member_1 = rownames(ipw)[indices[keep, 1L]],
      member_2 = rownames(ipw)[indices[keep, 2L]], n_profile_coordinates = coordinates,
      r_ipw = ri[keep], r_gcomp = rg[keep], min_r_both = pmin(ri[keep], rg[keep]),
      stringsAsFactors = FALSE)
    pairs <- pairs[order(-pairs$min_r_both, pairs$member_1, pairs$member_2), , drop = FALSE]
  }

  qualifying_members <- list()
  rows <- list()
  for (members in shared) {
    within_ipw <- ipw_cor$r[members, members, drop = FALSE]
    within_gcomp <- gcomp_cor$r[members, members, drop = FALSE]
    ri <- within_ipw[upper.tri(within_ipw)]
    rg <- within_gcomp[upper.tri(within_gcomp)]
    if (any(!is.finite(ri)) || any(!is.finite(rg))) {
      diagnostics$branches_undefined <- diagnostics$branches_undefined + 1L
      next
    }
    if (any(ri < r_threshold) || any(rg < r_threshold)) {
      diagnostics$branches_below_threshold <- diagnostics$branches_below_threshold + 1L
      next
    }
    index <- length(rows) + 1L
    qualifying_members[[index]] <- members
    rows[[index]] <- data.frame(axis = axis,
      cluster_id = sprintf("%s_%03d", axis, index),
      members = paste(rownames(ipw)[members], collapse = "; "), n_members = length(members),
      n_profile_coordinates = coordinates, min_r_ipw = min(ri), mean_r_ipw = mean(ri),
      min_r_gcomp = min(rg), mean_r_gcomp = mean(rg),
      sign_agreement = mean(sign(ipw[members, , drop = FALSE]) == sign(gcomp[members, , drop = FALSE])),
      is_maximal = FALSE, stringsAsFactors = FALSE)
  }
  if (length(rows)) {
    clusters <- do.call(rbind, rows)
    clusters$is_maximal <- vapply(qualifying_members, function(members) {
      !any(vapply(qualifying_members, function(other) {
        length(other) > length(members) && all(members %in% other)
      }, logical(1)))
    }, logical(1))
    clusters <- clusters[order(-clusters$n_members, -pmin(clusters$min_r_ipw, clusters$min_r_gcomp),
                              clusters$cluster_id), , drop = FALSE]
  }
  diagnostics$qualifying_branches <- nrow(clusters)
  diagnostics$maximal_clusters <- sum(clusters$is_maximal)
  rownames(clusters) <- rownames(pairs) <- NULL
  list(clusters = clusters, pairs = pairs, diagnostics = diagnostics)
}

heatmap_comparison_report <- function(clusters, pairs, diagnostics, run_id, current_dir,
                                      r_threshold, generated_utc, workbook, metadata) {
  escape <- function(x) gsub("|", "\\|", gsub("[\r\n]", " ", x), fixed = TRUE)
  cutoff <- format(r_threshold, digits = 15, trim = TRUE)
  lines <- c("# Shared IPW and G-comp heatmap clusters", "",
    paste0("Source run: `", run_id, "`. Generated: ", generated_utc, "."),
    paste0("Input bundle: `", current_dir, "`."),
    paste0("Workbook: [", basename(workbook), "](", basename(workbook), ")."), "",
    paste0("A shared cluster has identical branch membership in both Euclidean-distance, ",
      "complete-linkage trees and Pearson r >= ", cutoff, " for every member pair in both methods. ",
      "At least three profile coordinates are required. Membership ignores leaf order."), "",
    "Values are the full exported, sign-adjusted heatmap_ate estimates, without clipping or additional scaling. All exported exercise contrasts are included.", "",
    "These are descriptive correlations between estimated effect profiles, not participant-level biomarker correlations or tests of statistical significance. High correlation does not guarantee matching effect magnitudes or signs. Confidence-interval stars are not used as a filter.", "",
    "The tables below show maximal qualifying shared clusters: those not contained in a larger qualifying shared cluster. The workbook also retains nested qualifying branches and all strongly correlated pairs, including pairs that do not form shared branches.", "",
    "Sign agreement is the fraction of aligned cluster cells whose displayed ATE signs agree between IPW and G-comp. Zero is a separate sign. This is descriptive and does not affect eligibility. Report values are rounded; workbook values are not deliberately rounded.")
  if ("data_kind" %in% names(metadata))
    lines <- c(lines, "", paste0("Source data kind: `", metadata$data_kind[1L], "`."))
  for (axis in c("biomarkers", "interventions")) {
    d <- diagnostics[diagnostics$axis == axis, , drop = FALSE]
    selected <- clusters[clusters$axis == axis & clusters$is_maximal, , drop = FALSE]
    lines <- c(lines, "", if (axis == "biomarkers") "## Biomarker clusters" else
      "## Intervention/exercise clusters", "",
      if (axis == "biomarkers") "Profiles compare each biomarker across exported intervention/exercise contrasts." else
        "Profiles compare each intervention/exercise contrast across exported biomarkers.", "",
      sprintf("%d members; %d profile coordinates per member. %s", d$n_members, d$n_profile_coordinates, d$status))
    if (nrow(selected)) {
      lines <- c(lines, "", "| Members | n | Min r IPW | Mean r IPW | Min r G-comp | Mean r G-comp | Sign agreement |",
        "| --- | ---: | ---: | ---: | ---: | ---: | ---: |")
      for (i in seq_len(nrow(selected))) {
        x <- selected[i, ]
        lines <- c(lines, sprintf("| %s | %d | %.4f | %.4f | %.4f | %.4f | %.1f%% |",
          escape(x$members), x$n_members, x$min_r_ipw, x$mean_r_ipw,
          x$min_r_gcomp, x$mean_r_gcomp, 100 * x$sign_agreement))
      }
    } else lines <- c(lines, "", "No shared clusters qualified.")
    if (!any(pairs$axis == axis)) lines <- c(lines, "", "No strongly correlated pairs qualified.") else
      lines <- c(lines, "", sprintf("%d strongly correlated pairs qualified; see the Strong_pairs workbook sheet.",
        sum(pairs$axis == axis)))
    if (d$status == "screened") {
      lines <- c(lines, "",
        sprintf("Of %d shared branches, %d qualified (%d maximal), %d were excluded for undefined correlations, and %d failed the correlation threshold.",
          d$shared_branches, d$qualifying_branches, d$maximal_clusters, d$branches_undefined, d$branches_below_threshold),
        sprintf("Of %d candidate pairs, %d had undefined correlations and %d failed the threshold in at least one method.",
          d$total_pairs, d$pairs_undefined, d$pairs_below_threshold))
      for (method in c("ipw", "gcomp")) {
        excluded <- d[[paste0("undefined_profiles_", method)]]
        if (nzchar(excluded)) lines <- c(lines,
          paste0("Profiles with zero or nonfinite SD in ", if (method == "ipw") "IPW" else "G-comp",
                 ": ", escape(excluded), ". These remain in the dendrograms."))
      }
    }
  }
  lines
}

compare_heatmap_clusters <- function(r_threshold = 0.80,
    current_dir = "Output/biomarker_analysis/current", output_dir = NULL) {
  heatmap_comparison_threshold(r_threshold)
  if (!file.exists("biological_calendar_aging.Rproj"))
    stop("Run from the biological_calendar_aging repository root.", call. = FALSE)
  if (!is.character(current_dir) || length(current_dir) != 1L || is.na(current_dir) || !nzchar(current_dir))
    stop("current_dir must be a nonempty directory path.", call. = FALSE)
  required <- c("main_heatmap_IPW.csv", "main_heatmap_G-comp.csv", "export_metadata.csv", "SUCCESS.txt")
  sources <- file.path(current_dir, required)
  missing <- required[!file.exists(sources)]
  if (length(missing)) stop("Missing completed heatmap bundle files in ", current_dir, ": ",
    paste(missing, collapse = ", "), ". Supply an existing published current bundle; this script does not run the pipeline.",
    call. = FALSE)
  if (!identical(readLines(sources[4L], n = 1L, warn = FALSE), "SUCCESS"))
    stop("SUCCESS.txt does not identify a completed heatmap bundle.", call. = FALSE)
  current_dir <- normalizePath(current_dir, winslash = "/", mustWork = TRUE)
  sources <- file.path(current_dir, required)
  hashes <- tools::md5sum(sources)
  metadata <- read.csv(sources[3L], stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(metadata) != 1L || !"run_id" %in% names(metadata) || anyDuplicated(names(metadata)))
    stop("export_metadata.csv must contain one row with a run_id.", call. = FALSE)
  run_id <- as.character(metadata$run_id)
  if (is.na(run_id) || !nzchar(trimws(run_id)) || run_id %in% c(".", "..") || grepl("[/\\\\]", run_id))
    stop("export_metadata.csv contains an invalid run_id.", call. = FALSE)
  read_estimates <- function(path) read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
    colClasses = c(biomarker = "character", treatment = "character", method = "character"))
  matrices <- heatmap_comparison_matrices(read_estimates(sources[1L]), read_estimates(sources[2L]))
  axes <- list(
    heatmap_comparison_axis(t(matrices$IPW), t(matrices$`G-comp`), "biomarkers", r_threshold),
    heatmap_comparison_axis(matrices$IPW, matrices$`G-comp`, "interventions", r_threshold))
  clusters <- do.call(rbind, lapply(axes, `[[`, "clusters"))
  pairs <- do.call(rbind, lapply(axes, `[[`, "pairs"))
  diagnostics <- do.call(rbind, lapply(axes, `[[`, "diagnostics"))
  maximal <- clusters[clusters$is_maximal, , drop = FALSE]

  if (!requireNamespace("writexl", quietly = TRUE))
    stop("Excel export requires the 'writexl' package. Install it before running this script.", call. = FALSE)
  threshold_label <- format(r_threshold, scientific = FALSE, trim = TRUE, digits = 15)
  if (is.null(output_dir)) output_dir <- file.path(dirname(current_dir), "cluster_comparison", run_id,
    paste0("pearson_", threshold_label))
  if (!is.character(output_dir) || length(output_dir) != 1L || is.na(output_dir) || !nzchar(output_dir))
    stop("output_dir must be a nonempty directory path.", call. = FALSE)
  # Refuse mixed snapshots if current was replaced while the estimates were read.
  if (!identical(hashes, tools::md5sum(sources)))
    stop("The current bundle changed during comparison. Rerun against a stable published bundle.", call. = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(output_dir)) stop("Cannot create output directory: ", output_dir, call. = FALSE)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = TRUE)
  workbook <- file.path(output_dir, "shared_heatmap_clusters.xlsx")
  report <- file.path(output_dir, "shared_heatmap_clusters.md")
  generated_utc <- format(Sys.time(), tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
  definitions <- c(
    axis = "biomarkers: profiles across interventions/exercise; interventions: profiles across biomarkers.",
    cluster_id = "Identifier within this comparison, shared between the cluster sheets; not a cross-run identifier.",
    members = "Original exported identifiers separated by semicolons; membership matching uses identifiers, not this display string.",
    n_members = "Number of members in the cluster.",
    n_profile_coordinates = "Number of estimates in each member's profile; at least three are required for screening.",
    min_r_ipw = "Minimum Pearson correlation over all distinct member pairs in IPW; must meet the threshold.",
    mean_r_ipw = "Mean Pearson correlation over all distinct member pairs in IPW; descriptive only.",
    min_r_gcomp = "Minimum Pearson correlation over all distinct member pairs in G-comp; must meet the threshold.",
    mean_r_gcomp = "Mean Pearson correlation over all distinct member pairs in G-comp; descriptive only.",
    sign_agreement = "Fraction of aligned cluster cells with identical sign(heatmap_ate) between methods; zero is separate. Not a filter.",
    is_maximal = "TRUE when no larger qualifying shared cluster on this axis contains all these members.",
    member_1 = "First member of a distinct pair, identified by its original exported identifier.",
    member_2 = "Second member of the pair; membership in a shared branch is not required for the pair table.",
    r_ipw = "Pearson correlation of the pair's profiles in IPW.",
    r_gcomp = "Pearson correlation of the pair's profiles in G-comp.",
    min_r_both = "The smaller of r_ipw and r_gcomp; must meet the threshold.",
    clustering = "Euclidean distance and complete linkage, independently for each method and axis. Shared branches require exact membership.",
    ordering = "Both matrices use first-appearance IPW identifier order, as in the current pipeline. CSV rows are not plotted leaf order; ties can depend on input order.",
    values = "Full exported heatmap_ate, including existing sign adjustments; no additional sign flips, clipping, scaling, or CI-star filtering.",
    eligibility = "All within-cluster pairs must have finite Pearson r >= r_threshold in both methods. Negative correlations do not qualify by magnitude.",
    undefined = "Zero/nonfinite profile SD or nonfinite pair correlations exclude affected pairs and branches, but profiles remain in dendrograms.",
    interpretation = "Descriptive similarity of estimated effect profiles, not participant-level biomarker correlation or statistical significance. Similarity does not guarantee matching magnitudes or signs.",
    empty_results = "Sheets retain their column headers when no clusters or pairs qualify. See per-axis diagnostics for skipped axes and exclusions."
  )
  field_rows <- function(section, values) data.frame(section = section, field = names(values),
    value = as.character(values), stringsAsFactors = FALSE)
  provenance <- c(run_id = run_id, current_dir = current_dir, generated_utc = generated_utc,
    r_threshold = threshold_label, correlation_method = "pearson", output_dir = output_dir)
  definitions <- rbind(field_rows("definition", definitions), field_rows("comparison", provenance),
    field_rows("source_metadata", unlist(metadata[1L, , drop = FALSE], use.names = TRUE)),
    field_rows("input_md5", setNames(unname(hashes), required)))
  for (i in seq_len(nrow(diagnostics))) definitions <- rbind(definitions,
    field_rows(paste0("diagnostics_", diagnostics$axis[i]), unlist(diagnostics[i, , drop = FALSE], use.names = TRUE)))
  lines <- heatmap_comparison_report(clusters, pairs, diagnostics, run_id, current_dir,
    r_threshold, generated_utc, workbook, metadata)
  writexl::write_xlsx(list(Maximal_clusters = maximal, All_shared_branches = clusters,
    Strong_pairs = pairs, Definitions = definitions), workbook)
  writeLines(enc2utf8(lines), report, useBytes = TRUE)
  message(sprintf("Shared qualifying branches: %d (%d maximal); strong pairs: %d.",
    nrow(clusters), nrow(maximal), nrow(pairs)))
  message("Workbook: ", workbook, "\nReport: ", report)
  invisible(list(maximal_clusters = maximal, all_shared_branches = clusters, strong_pairs = pairs,
    diagnostics = diagnostics, definitions = definitions, workbook = workbook, report = report))
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 1L)
    stop("Usage: Rscript --vanilla Biomarker_analysis/compare_heatmap_clusters.R [r_threshold]", call. = FALSE)
  threshold <- if (length(args)) suppressWarnings(as.numeric(args[1L])) else 0.80
  compare_heatmap_clusters(r_threshold = threshold)
}
