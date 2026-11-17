# Combined Love and forest figures

Only `training_combined` and `metformin_combined` use the new style. Standalone
Love plots, standalone forest plots, supplementary figures and biomarker figures
retain their existing appearance. The combined figures are exported at 18 cm wide
with 9–10 pt axis/legend text, compact labels, bottom legends and native plot
elements. PNG exports are 300 dpi; PDF exports retain vector text and graphics.

Run all commands from `biological_calendar_aging/`.

## Regenerate only the two combined figures

After a run of the updated pipeline:

```bash
Rscript --vanilla Program/Replot_combined_figures.R Output
```

An alternative completed run directory can replace `Output`. Its saved inputs
must all come from the same run. The command reads these inputs and writes only
`training_combined.png`, `training_combined.pdf`, `metformin_combined.png` and
`metformin_combined.pdf` in that directory's `Combined_figures/` subdirectory.
It does not fit models, bootstrap, recompute estimates, modify result tables or
render the manuscript.

The required inputs are:

- `love_plots/exercise_effects/love_plot_Exercise_3to4_per_week_vs_Never.rds`
- `love_plots/main/love_plot_Metformin.rds`
- `training_analysis_results.RData`, containing `training_results_table`
- `metformin_summary`, containing `metformin_summary`

The new Love-plot RDS files contain only `covariate`, `status` and `smd`: aggregate
plotting values, with no patient records or fitted models. They are written by the
existing `create_love_plot()` function alongside its unchanged standalone PNG.

## One-time regeneration for an older run

Older runs saved Love plots only as PNGs. The original balance values cannot be
recovered from those images. Either rerun the full master pipeline, or run the
following existing scripts in an R session. This requires the completed run's
`Output/data_after_step4` and the other saved inputs used by step 18.

```r
# Use the bootstrap count intended for your analysis; this example uses 1000.
options(n_bootstrap = 1000L)
source("Program/IPW_step7.R")
source("Program/exercise_effects_step10.R")
source("Program/Metformin_table_and_figures_step18.R")
```

Steps 7 and 10 **repeat their analyses and bootstrapping**, replacing their saved
results. Step 7 creates the metformin balance input; step 10 creates the exercise
balance input. Step 18 rebuilds summary/forest outputs and both combined figures
from saved results, without refitting models. Once these inputs exist, use the
plot-only command for subsequent styling changes. It reports missing inputs and
the responsible step rather than launching any analysis automatically.

## Where to change appearance

- `Functions/combined_plot_style.R`: local labels, fonts, points, intervals and
  legends; selected only with `style = "combined"`.
- `Functions/combined_plot_functions.R`: native panel composition, dimensions,
  input loading and PNG/PDF export; shared by step 18 and the plot-only command.
- `Functions/love_plot_functions.R` and `Functions/forest_plot_functions.R`:
  drawing constructors whose default style remains `"standalone"`.

The balance calculation remains in `Functions/IPW_functions.R`, and forest
conversion/parsing remains in the existing forest constructors. No statistical
calculation has a second implementation in the plotting entry point.

`Manuscript.qmd` includes the two combined PNGs at full available text width.
Its captions and references are unchanged. The plotting command requires the
pipeline's existing `here`, `ggplot2`, `dplyr`, `tidyr` and `stringr` packages,
plus `gtable` (a ggplot2 dependency) and R's Cairo graphics support.
