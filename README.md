# Biological calendar aging: cross-sectional analyses

This repository contains the source files needed to regenerate the DunedinPACE
analyses, analyses of additional biomarkers, figures, Excel tables, and Word
manuscript from the original cross-sectional dataset. Treatment effects are
estimated using inverse probability weighting (IPW) and G-computation.

The original data and generated results are supplied or created locally. No
existing output files are needed to start. The instructions below use the
supplied code and settings without editing them.

**Both analysis workflows currently use two bootstrap iterations.** This is a
technical test setting and is insufficient for final confidence intervals.
Copying the code unchanged preserves this setting; it does not reproduce an
earlier analysis that used a different bootstrap count.

## Repository structure

Keep the existing filenames and relative paths:

| Location | Contents and purpose |
| --- | --- |
| [Master_parallel.R](Master_parallel.R) | Runs the main DunedinPACE workflow, including subgroup and sensitivity analyses, tables, figures, and diagnostics. |
| [biological_calendar_aging.Rproj](biological_calendar_aging.Rproj) | RStudio project and project-root marker. The biomarker runner explicitly requires this filename. |
| [Manuscript.qmd](Manuscript.qmd) | Manuscript source, using the saved analysis results. |
| [_quarto.yml](_quarto.yml) | Project rendering settings and supplementary-table export hooks. |
| [nature.csl](nature.csl), [DAG.png](DAG.png) | Citation style and the manuscript's causal diagram. |
| `Program/` | Analysis steps 1–26, including step 4b, plus `Table1_descriptive_tests.R`, `manuscript_review_summaries.R`, and `export_supplementary_tables.R` (30 R files). |
| `Program/Functions/` | The 20 shared R helper files used by the analyses and manuscript. Keep helpers even when their filenames contain parentheses. |
| `Program/References/` | `Article 2.bib`, `manuscript-reference.docx`, and `supplementary_table_captions.md`. |
| `Program/Filters/` | `compact-crossrefs.lua`, used during manuscript rendering. |
| `Biomarker_analysis/` | `run.R`, `config.R`, `runtime.R`, `parallel.R`, `residuals.R`, `steps.R`, `results.R`, and `compare_heatmap_clusters.R`. |
| [.gitignore](.gitignore) | Excludes local data, analysis outputs, and selected session/cache files from Git. |

The Word reference document and `DAG.png` are required manuscript inputs. Keep
them in the repository even though generated documents and figures are excluded.

## Requirements

Install [R](https://cran.r-project.org/) and the
[Quarto CLI](https://quarto.org/docs/get-started/). RStudio is optional. Make sure
`Rscript` and `quarto` are available in your terminal, and that Quarto uses the R
installation where you install the packages below. The R package `quarto` does
not replace the Quarto CLI.

Run this setup once in R on your local computer:

```r
options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages(c(
  "pacman", "here", "tidyverse", "lubridate",
  "DiagrammeR", "DiagrammeRsvg", "rsvg", "UpSetR",
  "patchwork", "png", "ragg", "gtable", "scales",
  "future", "future.apply", "callr", "parallelly",
  "tableone", "writexl", "readxl", "flextable",
  "quarto", "knitr", "rmarkdown", "openxlsx", "xml2", "remotes"
))
remotes::install_github("stopsack/khsmisc", upgrade = "never")
```

`tidyverse` installs the data-manipulation packages used by the scripts, including
`dplyr`, `tidyr`, `readr`, `stringr`, and `ggplot2`. Base R packages such as `grid`
and `parallel` are already included with R.

Package installation requires internet access and a writable R library. The
unchanged main pipeline also attempts package installation during execution;
step 1 calls `install.packages("pacman")` unconditionally. The biomarker runner
checks its dependencies before starting.

On systems that build R packages from source, additional operating-system
libraries may be needed for XML, SVG, Cairo graphics, fonts, and image formats.
Install the missing development libraries identified by package installation
errors before retrying. Use a local environment with these dependencies for
manuscript rendering.

## Prepare the local data and folders

Work from the repository root: the folder containing `Master_parallel.R` and
`biological_calendar_aging.Rproj`. In RStudio, open that project file first.

Place your original CSV at this exact path relative to the repository root:

```text
Data/
└── EPICv1 and v2 data/
    └── All_metrics_crossectional_samples.csv
```

Keep the spelling `crossectional` in the filename. The data are not included in
the repository. Both workflows read this CSV and select survey version 1 during
preparation.

The CSV must contain the columns and category labels expected by the preparation
scripts, including the selected biomarkers in
[Biomarker_analysis/config.R](Biomarker_analysis/config.R). The biomarker runner
requires unique, nonmissing `Patient.ID` values. The preparation code handles the
legacy outcome name `DunedinPoAm` by renaming it to `DunedinPACE`.

Before running the main pipeline, create an empty `Output/` folder. From an R
session whose working directory is the repository root:

```r
dir.create("Output", showWarnings = FALSE)
```

Step 1 writes directly into this folder without creating it. Git does not retain
empty directories, so recreate it after cloning. The workflows create their
remaining output subfolders as needed.

## Run the workflows

Run the following terminal commands separately, in this order, from the
repository root. Wait for each stage to finish successfully before continuing.

1. Run the main DunedinPACE analyses:

   ```bash
   Rscript --vanilla Master_parallel.R
   ```

   This creates the main results, subgroup and sensitivity analyses, flowchart,
   descriptive tables, diagnostic plots, combined figures, and manuscript
   metadata. Check that the parallel summary reports `Failed steps: 0` and that
   the script reaches its final completion message without errors. A completion
   message alone does not establish that all analysis steps succeeded.

2. Run the additional biomarker analyses:

   ```bash
   Rscript --vanilla Biomarker_analysis/run.R
   ```

   This uses the same original CSV and the supplied `config.R`. It creates
   biomarker transformations, main IPW and G-computation estimates, heatmaps,
   supplementary data workbooks, and shared-cluster comparison reports. Wait for
   the final `Complete:` message. This workflow runs main analyses for the
   selected biomarkers; the full subgroup and sensitivity results come from
   the first command.

3. Render the manuscript and export its supplementary tables:

   ```bash
   quarto render
   ```

   The project renders `Manuscript.qmd` and runs the pre-render and post-render
   hooks in `_quarto.yml`. Keep those hooks and code execution enabled. Rendering
   uses the saved results and computes the manuscript's descriptive comparisons
   from the saved cohort; it does not launch either analysis pipeline. Keep the
   default output name `Manuscript.docx`, which the export hook expects.

## Generated files

| Location | Generated content |
| --- | --- |
| `Output/` | Intermediate datasets, main result objects, descriptive Excel tables, summaries, and manuscript metadata. |
| `Output/Forest_plots/`, `Output/love_plots/`, `Output/Combined_figures/`, `Output/IPW_diagnostics_plots/` | Main-analysis figures and diagnostics. Other figures, including the flowchart, are written directly into `Output/`. |
| `Output/biomarker_analysis/<run_id>/` | Each biomarker run's data, estimates, transformation metadata, logs, and heatmap exports. |
| `Output/biomarker_analysis/current/` | Published heatmaps, `Supplementary_Data_S1_IPW.xlsx`, `Supplementary_Data_S2_G-comp.xlsx`, export metadata, `SUCCESS.txt`, and shared-cluster reports from a completed workflow. |
| `Output/biomarker_analysis/cluster_comparison/` | Archived `shared_heatmap_clusters.xlsx` and `.md` reports. The runner also copies the latest reports into `current/`. |
| `Output/supplementary_tables/` | One `Supplementary_Tables_*.xlsx` workbook exported during project rendering, plus export bookkeeping files. The filename follows the resolved manuscript table numbers. |
| `Manuscript.docx` | Rendered manuscript in the repository root. |

The main pipeline writes to fixed output paths. Biomarker runs use new run
directories and refresh the published `current/` bundle. Regenerate both sets of
results from the same input data before rendering a new manuscript.

The existing `.gitignore` excludes `Data/` and `Output/`. Keep the generated
root-level `Manuscript.docx` uncommitted manually; it is not excluded by the
supplied ignore rules. Intermediate outputs include participant-level data and
should remain local along with the original CSV.

## Settings and reproducibility

The bootstrap settings are `n_bootstrap <- 2` in `Master_parallel.R` and
`n_bootstrap = 2L` in `Biomarker_analysis/config.R`. The latter also specifies the
selected biomarkers, their transformations, the seed, and worker settings. These
instructions preserve all supplied settings.

Reproducing earlier numerical results requires the same input data, code,
analysis settings, random-number settings, and R/package versions. Package
versions are not locked in this repository; the installation commands retrieve
the versions currently available. Copying source files alone does not preserve
the original software environment.

## Troubleshooting

| Problem | What to check |
| --- | --- |
| Input CSV cannot be opened | Start from the repository root and check the exact `Data/EPICv1 and v2 data/All_metrics_crossectional_samples.csv` path. |
| Step 1 cannot write `Output/data_after_step1.csv` | Create `Output/` and check that it is writable. |
| Runner says to start from the repository root | Check the working directory and keep the filename `biological_calendar_aging.Rproj` unchanged. |
| Missing package or package installation failure | Install the required packages in the R environment used by `Rscript` and Quarto. Resolve any missing operating-system libraries reported during installation. |
| Missing biomarker columns or invalid `Patient.ID` values | Confirm that the original cross-sectional CSV matches the supplied configuration and input requirements. |
| Main analysis step fails | Read the reported step error and `Output/parallel_execution_log.csv` when present. Resolve the cause and rerun the main workflow before rendering. |
| Biomarker workflow fails | Read `status.csv` in the reported run directory and the individual biomarker `master.log` files. A previous `current/` bundle may still be present; its existence does not prove the new run succeeded. |
| Manuscript reports missing or inconsistent results | Complete both workflows using the same CSV. Check the main metadata, saved cohort, and published biomarker bundle; avoid mixing results from different runs. |
| Supplementary workbook is missing or export preparation is stale | Run the normal project command `quarto render` with `_quarto.yml`, its hooks, and code execution enabled. Confirm that `openxlsx` and `xml2` are installed and the output remains named `Manuscript.docx`. |
