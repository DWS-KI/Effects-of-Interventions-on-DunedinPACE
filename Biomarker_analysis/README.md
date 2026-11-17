# Multiple biomarkers through the existing pipeline

Run from the repository root (`biological_calendar_aging/`):

```bash
Rscript --vanilla Biomarker_analysis/run.R
```

In RStudio, you can instead run:

```r
source("Biomarker_analysis/run.R")
main()
```

Settings are in `Biomarker_analysis/config.R`. No existing pipeline files
or input data are overwritten. No commits are made. The wrapper does not generate mock data.

## Automatic and standalone comparison of shared heatmap clusters

`compare_heatmap_clusters.R` compares the latest completed heatmap exports in
`Output/biomarker_analysis/current/`. `run.R` calls it automatically after heatmap
publication, using the existing default threshold, then copies both reports into
the published `current/` directory with overwrite enabled. The comparison reads
only aggregate exports and does not rerun models or alter the published heatmaps.
It requires the two heatmap CSVs, `export_metadata.csv`, `SUCCESS.txt`, and the
`writexl` package for Excel export.

To rerun the comparison separately, run from the repository root:

```bash
Rscript --vanilla Biomarker_analysis/compare_heatmap_clusters.R
Rscript --vanilla Biomarker_analysis/compare_heatmap_clusters.R 0.90
```

The default threshold is **Pearson r >= 0.80**. Alternatively, source the file
without starting a comparison, then call the function:

```r
source("Biomarker_analysis/compare_heatmap_clusters.R")
comparison <- compare_heatmap_clusters(r_threshold = 0.80)

# Optional input and output directories (paths relative to the repository root):
comparison <- compare_heatmap_clusters(
  r_threshold = 0.90,
  current_dir = "Output/biomarker_analysis/current",
  output_dir = "Output/biomarker_analysis/custom_cluster_comparison"
)
```

Both axes are compared separately: biomarker profiles across intervention/exercise
contrasts, and intervention/exercise profiles across biomarkers. The script uses
the full exported `heatmap_ate` values, preserving their sign adjustments, without
clipping or additional scaling. It retains exactly the exercise contrasts present
in the exports; it does not read current analysis settings from `config.R`.

The script reconstructs the existing **Euclidean-distance, complete-linkage**
dendrograms. Both matrices use the IPW CSV's first-appearance identifier order,
matching the current pipeline's shared input order; ties can depend on that order.
A shared cluster has exactly the same branch membership in both methods,
regardless of leaf order. **Every member pair** must also have Pearson r at least
the chosen threshold in **both** methods. A high mean alone is insufficient, and
negative correlations do not qualify by magnitude. No confidence-interval or
cell-star filter is applied.

At least two members and three profile coordinates are needed for screening an
axis. Constant profiles remain in the dendrograms, but pairs or clusters with
undefined correlations are excluded and documented. Missing, duplicate, nonfinite,
or mismatched exported cells cause an error instead of imputation or silent dropping.

The default output directory is:

```text
Output/biomarker_analysis/cluster_comparison/<run_id>/pearson_<threshold>/
    shared_heatmap_clusters.xlsx
    shared_heatmap_clusters.md
```

For the default threshold, the final directory is `pearson_0.8`. The source run ID
comes from the published metadata. Repeating the same comparison replaces its
derived reports; different runs and thresholds have separate default directories.

In addition to these archived reports, each successful `run.R` invocation saves
the latest copies, replacing any existing files:

```text
Output/biomarker_analysis/current/shared_heatmap_clusters.md
Output/biomarker_analysis/current/shared_heatmap_clusters.xlsx
```

A custom `output_dir` gets the same `cluster_comparison/` and `current/` layout
under that directory. Both copies must succeed before `run.R` prints `Complete:`.
Comparison or copy failures stop the runner; already published heatmaps remain
available. Standalone comparison commands retain their existing behavior and do
not automatically refresh the copies in `current/`.

The Excel workbook contains four sheets:

- `Maximal_clusters`: qualifying shared clusters not contained in a larger
  qualifying shared cluster on that axis.
- `All_shared_branches`: all qualifying shared branches, including nested ones.
- `Strong_pairs`: every pair meeting the threshold in both methods, whether or
  not it forms a shared dendrogram branch.
- `Definitions`: column meanings, source metadata and input hashes, comparison
  settings, and counts/reasons for exclusions or skipped axes.

Cluster tables include membership, minimum and mean pairwise correlations per
method, and sign agreement. Sign agreement is the fraction of aligned cluster
cells whose displayed ATE signs agree between IPW and G-comp; zero is a separate
sign. It is descriptive and does not affect eligibility. The Markdown report
summarizes maximal clusters on each axis and the exclusions. Empty results are
valid: tables retain their headers and the report explicitly says none qualified.
Workbook numbers are not deliberately rounded; the report rounds for readability.
The function invisibly returns the tables, diagnostics, definitions, and output paths.

These are **descriptive correlations between estimated effect profiles**, not
participant-level biomarker correlations or tests of statistical significance.
High correlation does not guarantee matching effect magnitudes or signs.

## Main analyses only for heatmaps

Preparation steps 1–3 run once. Each biomarker then runs only
**steps 2–4, 7, 8, and 10**: selection and data processing, 18 interventions with IPW
and G-computation, and four main exercise comparisons with both methods.
This gives **44 estimates per biomarker instead of 340**, approximately 87 % fewer.
The time savings have not been measured; the bootstrap count and selected biomarkers are retained.

## Parallel biomarkers and core count

After the shared preparation, biomarkers run in separate R processes through
a bounded `callr::r_bg()` queue. The default `n_workers = NULL` uses
`parallelly::availableCores(omit = 1L)`: **available cores minus one**, with at least
one worker and at most the number of selected biomarkers. This adapts on each run
to the computer and any container/scheduler limits. Two available
cores give one worker; eight give at most seven. A positive integer, such as
`n_workers = 1L`, further limits the count but cannot exceed the limit.
Older configurations without `n_workers` receive the same automatic default.
The actual count is saved in the run's `config.rds` and shown in the startup log.

Analysis steps and bootstrapping within each biomarker remain sequential.
The same seed and the bootstrap functions' own seed resets are retained.
Numerical libraries are limited to one thread per worker to avoid nested
CPU load. Each concurrent process needs its own memory; a lower
`n_workers` can be used on computers with limited RAM.
The main process validates and writes combined results in the configured
biomarker order, regardless of completion order. Heatmaps are then created in the same
way as before, with unchanged models, selection, scaling, clustering, and CIs.

Subgroup and sensitivity analyses, descriptive tables, UpSet/forest plots,
concurrent-treatment histograms, manuscript metadata, and separate IPW diagnostics
are not run. The flowchart and Love plots are not drawn. Exclusion rules and models
are retained, as is the balance summary already included in the IPW results.

## Original data on your computer

1. Place your original file at the usual path:
   `Data/EPICv1 and v2 data/All_metrics_crossectional_samples.csv`.
2. Check that `data_kind = "real"` in `config.R` (the current setting).
3. Change `n_bootstrap = 2L` to `n_bootstrap = 1000L`.
4. Run the command above. All data steps, residuals, standardizations, analyses, and
   figures are recalculated from **your CSV file**. Previous mock results are not reused.

`input_file` can also be changed to another local path. Results stay local;
the wrapper does not call any cloud service or transfer data. Result folders contain
personal data, just like the base pipeline, and should be treated as sensitive in real runs.
Input data must follow the base pipeline's column names and categories. The legacy
name `DunedinPoAm` is handled as `DunedinPACE`, just as in step 1. Missing selected
biomarkers cause a clear error: adjust `biomarkers` to the available measures.
The mock test verifies program flow, but cannot guarantee model convergence,
positivity, or sufficient treatment prevalence in the original data's comparison groups.

An alternative configuration file can be used without changing the default file:

```bash
Rscript --vanilla Biomarker_analysis/run.R Biomarker_analysis/min_config.R
```

The file must define the entire `biomarker_config` list, or read `config.R` and
modify the relevant list components. The active lines in
`biomarkers` in `config.R` specify the current selection, one measure per line. Delete/comment out the lines you
do not want to run. Example with two measures:

```r
biomarkers = c(
  "Horvath.PC" = "age_years",
  "DunedinPACE" = "positive_raw"
)
```

The value on each line determines the transformation: `"age_years"` means
an age residual followed by z-standardization (raw value > 0), `"raw"` means direct
z-standardization without residualization, and `"positive_raw"` means the same
with the check raw value > 0. New clocks in your private file can be added directly
here with the exact R column name and the correct transformation; no other code needs to change.
Example: `"MyAdditionalAgeClock" = "age_years"`. If the column is absent from your CSV,
the run stops before analysis with its name in the error message.

The 31 existing acceleration, residual, and percentile measures are also listed,
but as commented-out options to preserve the earlier decision to run
original measures. Uncomment such a line, including its leading comma,
to include it. These options are standardized directly and are not residualized again.

### Where do the column names come from?

The biomarker names in `config.R` and the catalog in `residuals.R` specify which measures
the code supports. They must match the R column names in your local input file.
The repository does not contain a complete column inventory or example data.
Other measures can be added to the configuration with the correct transformation rule if
they are present in your file.

Check the actual column list locally without reading any participant rows:

```r
source("Biomarker_analysis/config.R")
names(read.csv(biomarker_config$input_file, nrows = 0))
```

The number of bootstrap iterations applies to each analysis,
not the number of biomarkers or the number of master runs.

## Residuals and z-standardization

After steps 1–3 of the base pipeline, the potential main analysis population is identified
using the same selection criteria as step 4, but without the old DunedinPACE check.
Validity checks are then applied per biomarker. Regression and SD are calculated
on the biomarker's main analysis population, pooling treated and untreated participants.
The same transformation is used in all subsequent main analyses; SD is
not recalculated per treatment or exercise comparison. Sensitivity and subgroup analyses are skipped.

**Fallback catalog's age clocks in years (23 measures):** Extrinsic.Age, Intrinsic.Age, Hannum.PC,
Horvath.PC, GrimAge.PC, PhenoAge.PC, OMICmAge, OMICmAge_cor, DNAmFitAge,
OMICmAge_cor_v2, DNAmFitAge_cor_v2, SystemsAge and its 11 organ measures.

- `X.AgeResidual = X - predicted X from regression on chronological age`.
- `X.AgeResidualZ = (residual - mean residual) / SD(residual)`.

**Fallback catalog's other original measures (11):** DunedinPACE, Telomere.Values,
Telomere.Values.PC, Full_CumulativeStemCellDivisions,
Avg_LifeTime_IntrinsicStemCellDivisionRate,
Median_Lifetime_IntrinsicStemCellDivisionRate, MitoticScore_OldEpitoc,
HypoClock_Score_Old, DNAmGait_noAge, DNAmGrip_noAge and DNAmVO2max.

- These are **not** residualized, according to the user's method choice.
- `X.Z = (X - mean) / SD(X)` from the original measure.
- Chronological age is still included as a covariate in the base pipeline's IPW/G-comp.

The fallback catalog includes these 34 original measures; the explicit
`biomarkers` list selects which ones actually run. The existing selection has not changed.
`biomarker_set` is used only if you set `biomarkers = NULL`.
Existing residuals, acceleration columns, and percentiles are not used as
separate outcomes. They are preserved in the data. `"clocks"` selects 23 age clocks plus
DunedinPACE. `"all"` is an explicit option that also includes the existing
derived measures; these are z-standardized directly, without further residualization.
The active classification by unit appears next to each name in `config.R`.
The fallback catalog for older configurations is in `residuals.R`.

Valid age is 18–110 years. Nonfinite biomarker values become NA in new columns.
Age clocks and raw DunedinPACE must be > 0; negative residuals and negative
z-scores are valid and are not excluded. Other measures require finite raw values;
select `"positive_raw"` in the configuration if a new measure must also be positive.
Original columns are unchanged in `data_with_residuals.*`. Rows outside survey 1
are preserved there, with NA in new columns. The reference population can differ between
biomarkers if raw values are missing/invalid. This is reported in the metadata and
the result table's `n_reference`, `n_treated`, and `n_control`.

Options for `residual_method`:

| Choice | Model | Usage |
| --- | --- | --- |
| `"linear"` (default) | biomarker ~ age | Matches the article's regression form; easiest to compare and interpret. |
| `"quadratic"` | biomarker ~ age + age² | Allows a simple curvature across the age range. |
| `"spline"` | biomarker ~ natural spline of age | More flexible relationship; `spline_df` controls complexity. |

The difference of biological age minus chronological age is another possible
age gap, but is not a regression residual and is therefore not implemented here.
Model choice should be based on the relationship between age and biomarker, not on which choice
produces the largest intervention effect. Regression and standardization are performed once
before the base pipeline's bootstrap; the intervals are conditional on this transformation.
They do not separately include uncertainty in the residual model's coefficients or SD.

### Relationship to the article

[Sehgal et al., Methods, steps 4–6](https://www.nature.com/articles/s41591-026-04562-9)
use linear age residualization and external residual SDs from the Health and
Retirement Study. Here, the analysis cohort's own SD is used, according to the user's choice.
The 11 measures that do not measure age in years are not residualized, also according to the user's
choice. This is therefore a methodological adaptation, not an exact reproduction of the article's
scaling or its paired before/after analyses. Effect measures come from this
repository's existing IPW and G-computation.

`reference_sd_file` can later specify a CSV with the columns `biomarker,residual_sd`.
For age clocks, SD must refer to age residuals; for other measures, it must refer to
the **original scale**, not residuals. All selected measures must have positive, finite
SDs. The name `residual_sd` is the file format's shared field name. With external SDs,
outcomes are scaled to the reference and do not necessarily have SD 1 in the local cohort.

## How the master pipeline is reused

Each run gets a new folder under `Output/biomarker_analysis/`. First, an
isolated preparation of steps 1–3 is performed. Each biomarker then gets its own project
with byte-identical copies of `Master_parallel.R` and the R files in `Program/`.
The master file runs in a separate R process per biomarker, with at most `n_workers`
concurrent processes. Within each process, the analysis steps are sequential. `here::here()`
and relative `Output/` paths point to the isolated project.

`runtime.R` adapts expressions in memory when they are read with `source()`:

- The current standardized column is used under the internal name
  `DunedinPACE`. The original PACE value is preserved as `Original.DunedinPACE`.
- The master receives prepared survey-1 data, the correct bootstrap count, and a sequential future plan.
  The wrapper replaces the master's call to step 1 with a log message in memory,
  because preparation has already completed the step, **even when data_kind = "real"**.
  When `Master_parallel.R` is run directly, step 1 is always performed. The master has no
  options for mock data and does not generate data.
- Outcome validity in steps 4 and 6 allows finite negative/positive z-scores.
  The raw-value check has already been performed per biomarker.
- Table 1 splits at z = 0, and labels are changed to the current biomarker.
- Forest plots and effect tables show SD units; the old multiplication
  of ATE/intervals by 100 and the percentage labels are removed in these functions.
  Regular percentages in descriptive tables are retained.
- Unguarded package reinstallation in step 1 is skipped after the dependency check.

Only steps 2–4, 7, 8, and 10 run for each biomarker, with auxiliary graphics disabled.
`Table1_afflictions_step4b.R` is explicitly skipped before the file is read or
run, even if auxiliary outputs are enabled. The table describes the main analysis population
with original DunedinPACE values and is still created when
`Master_parallel.R` is run separately. In the biomarker wrapper, the internal
`DunedinPACE` column instead contains z-values, which may be negative or zero.
This also applies when DunedinPACE itself is selected as the standardized biomarker.
Step 4b is therefore not added to `logs/steps.csv`; the default run still has
six recorded steps per biomarker.

The adaptations for Table 1 and forest plots remain in the adapter but are
not used by the simplified run. After all
consumers have finished, outcome names in exported data/results are changed to the correct
biomarker. Internal object/file names such as `Standardization_results_df` are retained so that
the result structure remains recognizable. MD5 hashes of the source files and original input are checked
before and after the run. Isolated source copies and logs retain the original
code/messages for traceability; the master may therefore write the word PARALLEL in the log
even though the steps within that biomarker run sequentially.

## Results

After a successful run, the fixed directory
`Output/biomarker_analysis/current/`, which `Manuscript.qmd` reads directly, is also updated.
It contains both `main_heatmap_IPW` and `main_heatmap_G-comp` in PNG/PDF/RDS/CSV,
as well as `Supplementary_Data_S1_IPW.xlsx` and `Supplementary_Data_S2_G-comp.xlsx`.
The Excel files have an `Estimates` sheet with the same rows as the respective figure:
original standardized ATEs, unchanged 95 % CIs, sign reversal,
display values, CI markers, group sizes, actual bootstrap counts, and
transformation metadata. The `Definitions` sheet explains the scale and columns.
Numerical values are not deliberately rounded. Raw individual data are not exported here.

`export_metadata.csv` identifies the source run and its settings;
`biomarker_selection.csv`, `residual_metadata.csv`, and `SUCCESS.txt` are included.
All files are prepared before `current/` is replaced. If export or publication
fails, the previous `current/` and `LATEST.txt` are restored; timestamped runs
are retained. An alternative `output_dir` gets its own `current/` directory, keeping
test runs separate when they use a separate output directory.

To prepare for the manuscript, run **both** `Master_parallel.R` (main analysis and
manuscript metadata) and `Biomarker_analysis/run.R` (heatplots and Excel files)
separately from the project root. The manuscript can then be rendered locally in an
environment with the necessary dependencies. Rendering does not start any analysis and produces a
clear error if the fixed heatplot directory is incomplete. The default
`n_bootstrap = 2L` is retained for technical tests; choose, for example, `1000L` for
final scientific intervals. The Excel files always report the actual count.

`Output/biomarker_analysis/LATEST.txt` points to the latest fully validated run.
That directory contains:

- `data_with_residuals.rds` and `.csv`: all original rows/columns plus new columns.
- `residual_metadata.csv`: model type, reference N, mean, SD, and column names.
- `residual_models.rds`: estimated regression models; NULL for directly z-scaled measures.
- `main_results.rds` and `.csv`: data frame with the main results for all biomarkers.
  It covers 18 interventions and four exercise contrasts, with both methods:
  **44 rows per biomarker**, i.e., the number of selected measures × 44 rows. The `analysis` column
  distinguishes `Main interventions` from `Main exercise`.
- `<biomarker>/Output/`: preparation data, exclusion counts, and main results for that measure.
- `<biomarker>/main_results.*`, `master.log`, `logs/steps.csv`, `SUCCESS.txt`.
- `heatmaps/main_heatmap_IPW.{png,pdf,rds,csv}` and the corresponding files for `G-comp`.
- `heatmaps/Supplementary_Data_S1_IPW.xlsx` and `heatmaps/Supplementary_Data_S2_G-comp.xlsx`.
- `status.csv`, `config.rds`, `source_hashes.csv`, and `SUCCESS.txt` for the entire run.
- `biomarker_selection.csv`: selected columns and transformation rules (new runs).

Heatmaps use a red–white–green scale: a positive display value is red,
zero is white, and a negative value is green. For exactly `Telomere.Values`, `Telomere.Values.PC`,
and `DNAmVO2max`, ATE is multiplied by **−1 before both coloring and clustering**.
These columns are marked with `†`: original positive ATEs become green and negative ones
become red. This indicates an assumed favorable direction, not demonstrated clinical benefit.
Other biomarkers retain their signs. Both methods use the same color limits
(default ±1 SD); more extreme values receive the endpoint color. The heatmap CSV retains the original
`ate_estimate`, row order, `sign_multiplier`, and `heatmap_ate`. In addition,
`ci_lower`, `ci_upper`, and `ci_excludes_zero` are exported so that each star can be checked.
Main results and confidence intervals are unchanged. Dendrograms above and to the left
show independent clustering for each method using **only Euclidean distance and complete
linkage**, not Pearson correlation. The RDS contains a composite `patchwork` figure;
the package is also needed when only recreating heatmaps. Figures show point estimates and a centered black `*` where a valid, finite
95 % bootstrap CI excludes zero (`ci_lower > 0` or `ci_upper < 0`). Intervals
that reach or cross zero, have missing/nonfinite bounds, or reversed bounds
are not marked. Stars use the CI;
no adjustment for multiple testing is made. Both CI columns must be present.
Sign reversal does not affect whether a CI excludes zero.
`include_exercise_in_heatmap = FALSE` limits figures to steps 7–8;
step 10 still runs and its results are saved.

To add stars to a previous run's heatmaps without rerunning analyses:

```r
source("Biomarker_analysis/results.R")
run_dir <- readLines("Output/biomarker_analysis/LATEST.txt", n = 1)
plot_biomarker_heatmaps(readRDS(file.path(run_dir, "main_results.rds")),
  file.path(run_dir, "heatmaps"), readRDS(file.path(run_dir, "config.rds")))
```

This overwrites the heatmap and Excel exports for the selected run; saved
main results are unchanged, and `current/` is not updated by this standalone
plotting call. It also requires `writexl`. Older results without metadata
receive empty metadata fields in Excel, not estimated bootstrap counts.

Two bootstrap iterations are a technical test run and are insufficient for inference.
On an error, new jobs are stopped and the run's remaining worker processes are terminated.
The error and interrupted jobs that had started are written to `status.csv`; the same cleanup occurs
on Ctrl-C. An incomplete run receives no overall SUCCESS marker
and does not update `current/` or `LATEST.txt`. Fully validated results and logs are preserved
for troubleshooting. The next start recalculates everything in a new directory.

## Dependencies and validation

The same R packages as the base pipeline are required, plus `callr`, `parallelly`, `readxl`, and `writexl`.
If the repository's local library `Output/mockdata/library` exists, it is used automatically.
On a new computer, install missing packages once in R:

```r
install.packages(c("callr", "parallelly", "pacman", "here", "tidyverse", "lubridate",
  "DiagrammeR", "DiagrammeRsvg", "rsvg", "UpSetR", "patchwork", "png",
  "future", "future.apply", "tableone", "writexl", "readxl", "remotes"))
remotes::install_github("stopsack/khsmisc", upgrade = "never")
```

The wrapper does not install packages during analysis. Quarto is not needed because
the manuscript is not rendered.

### Checks during normal runs

The normal run validates exactly the six steps per measure, negative and
positive standardized outcomes, correct individual IDs, exactly 44 unique main results,
finite estimates, ordered CIs, and exactly the requested number of successful bootstrap draws.
The 18 interventions and four exercise comparisons must be present for both
methods; extra subgroup files are rejected. These checks are in `steps.R`
and `results.R` and run automatically with real data as well.

On an error, the run stops and previously completed manuscript exports are retained.
These checks do not replace independent recalculation of models or bootstrap intervals.
Standalone test suites, mock data, and separate post-run reviews are not included.
Historical main results can still be used to recreate heatmaps
with the plotting call above.
