# IPW Analysis with Bootstrap Confidence Intervals

## Overview

This implementation provides a modular, reusable framework for conducting Inverse Probability Weighting (IPW) analysis with bootstrap-based confidence intervals. The code is split into two files:

1. **`IPW_functions.R`** - Modular functions for IPW analysis
2. **`IPW_step6.R`** - Main script that uses the functions

## Files

### IPW_functions.R

Contains all the core functions for IPW analysis:

- **`create_treatment_variable()`** - Creates binary treatment variable
- **`prepare_complete_cases()`** - Removes missing values
- **`build_ps_formula()`** - Builds propensity score formula
- **`fit_propensity_model()`** - Fits logistic regression model
- **`calculate_ipw_weights()`** - Calculates inverse probability weights
- **`estimate_ate_weighted()`** - Estimates ATE using IP weights
- **`bootstrap_ate_single()`** - Single bootstrap iteration
- **`bootstrap_ate_confidence_interval()`** - Full bootstrap procedure
- **`estimate_ate_for_treatment()`** - Complete ATE estimation for one treatment
- **`IPW()`** - Main user-facing function for multiple treatments

### IPW_step6.R

Main analysis script that:
1. Loads data from Step 4
2. Defines analysis parameters (treatments, outcomes, covariates)
3. Calls the `IPW()` function
4. Saves results to CSV
5. Provides summary statistics

## Bootstrap Methodology

The bootstrap procedure accounts for uncertainty in both propensity score estimation and treatment effect estimation:

1. **Draw M bootstrap samples** (resampling with replacement)
2. **Re-fit propensity score model** within each bootstrap sample
3. **Compute IPW weights** based on bootstrap propensity scores
4. **Fit weighted model** using those IP weights
5. **Save ATE estimate** for this iteration
6. **Calculate 95% CI** from empirical distribution using quantiles (2.5%, 97.5%)

## Usage

### Basic Usage

```r
# Source the functions
source(here::here("Program", "Functions", "IPW_functions.R"))

# Load your data
load(here::here("Output", "data_after_step4"))

# Define covariates for propensity score model
covariates <- c(
  "Decimal.Chronological.Age",
  "Biological.Sex",
  "Level.of.Education",
  "Tobacco.Use.Binary",
  "Alcohol.per.week",
  "Exercise.per.week",
  "BMI",
  "Stress.Level",
  "Hours.of.sleep.per.night",
  "Ethnicity.Recoded"
)

# Run IPW analysis with bootstrap
results <- IPW(
  data = data_after_step4,
  treatment_list = c("Metformin"),
  outcome_list = c("DunedinPACE"),
  covariates = covariates,
  n_bootstrap = 1000
)
```

### Multiple Treatments

```r
# Analyze multiple treatments at once
results <- IPW(
  data = data_after_step4,
  treatment_list = c("Metformin", "Aspirin", "Statin"),
  outcome_list = c("DunedinPACE", "PhenoAge"),
  covariates = covariates,
  n_bootstrap = 1000
)
```

### Running the Full Script

```r
# Simply run the script
source(here::here("Program", "IPW_step6.R"))
```

Or from the command line (if Rscript is in PATH):
```bash
Rscript Program/IPW_step6.R
```

## Output

The function returns a data frame with the following columns:

- **treatment** - Name of treatment variable
- **outcome** - Name of outcome variable
- **ate_estimate** - Point estimate of Average Treatment Effect
- **ci_lower** - Lower bound of 95% confidence interval
- **ci_upper** - Upper bound of 95% confidence interval
- **std_error** - Bootstrap standard error
- **n_treated** - Number of treated observations
- **n_control** - Number of control observations
- **n_bootstrap** - Number of successful bootstrap iterations
- **SMD** - Covariate balance indicator ("<0.1" or list of unbalanced covariates)

Results are automatically saved to:
- `Output/IPW_results_df.RData` - R data file with results dataframe
- `Output/love_plots/love_plot_[treatment_name].png` - Love plot for each treatment showing covariate balance before and after weighting

## Bootstrap Iterations

**Recommended values:**
- **Testing:** 100-200 iterations (fast, for debugging)
- **Final analysis:** 1000+ iterations (stable confidence intervals)

**Note:** Each iteration refits the propensity score model, so computation time scales linearly with the number of bootstraps.

## Validation

The implementation has been validated against the original `IPW_method.R` script:

```
Original Method:
  ATE: -0.003084971
  95% CI: [-0.01220012, 0.006184386]
  SE: 0.00468883

New Bootstrap Method:
  ATE: -0.003084971
  95% CI: [-0.01220012, 0.006184386]
  SE: 0.00468883
```

Results are identical, confirming statistical accuracy.

## Advantages of This Implementation

1. **Modular design** - Functions can be reused in other scripts
2. **Self-documenting** - Function names clearly indicate purpose
3. **Multiple treatments** - Analyze many treatments in one call
4. **Progress tracking** - Prints status messages during execution
5. **Error handling** - Handles bootstrap failures gracefully
6. **Reproducible** - Uses set.seed() for consistent results
7. **Automated output** - Saves results automatically with timestamps
8. **Visual diagnostics** - Automatically generates Love plots for each treatment
9. **Balance assessment** - SMD column shows which covariates are balanced

## Dependencies

Required R packages:
- `here` - Path management
- `dplyr` - Data manipulation (optional, for future extensions)
- `ggplot2` - Creating Love plots
- `tidyr` - Data reshaping for plots

## Customization

To analyze different treatments or outcomes, edit `IPW_step6.R`:

```r
# Change these variables
treatment_list <- c("YourTreatment1", "YourTreatment2")
outcome_list <- c("YourOutcome1", "YourOutcome2")
n_bootstrap <- 1000  # Adjust for speed vs. accuracy
```

To add more covariates:

```r
covariates <- c(
  "Decimal.Chronological.Age",
  "Biological.Sex",
  # Add your new covariates here
  "New.Covariate.1",
  "New.Covariate.2"
)
```

## Love Plots

The analysis automatically generates **Love plots** (SMD plots) for each treatment, showing covariate balance before and after propensity score weighting.

### What is a Love Plot?

A Love plot visualizes the standardized mean difference (SMD) for each covariate:
- **X-axis**: SMD values (measure of imbalance)
- **Y-axis**: Covariate names
- **Colors**: Unweighted (before) vs. Weighted (after)
- **Reference lines**: Red dashed lines at ±0.1 (balance threshold)

### Interpretation

**Good balance:**
- Points should move toward 0 after weighting
- All weighted points should fall between -0.1 and 0.1

**Poor balance:**
- Weighted points remain far from 0
- Points outside ±0.1 indicate residual confounding

### Location

Love plots are saved to: `Output/love_plots/love_plot_[treatment_name].png`

Example filenames:
- `love_plot_Metformin.png`
- `love_plot_Antiaging_Stem_Cell.png`
- `love_plot_Rapamycin.png`

### Example Usage

```r
# After running IPW analysis, view the Love plots
library(here)

# List all Love plots
list.files(here::here("Output", "love_plots"))

# View a specific plot (in RStudio)
library(png)
library(grid)
img <- readPNG(here::here("Output", "love_plots", "love_plot_Metformin.png"))
grid.raster(img)
```

## Notes

- Missing values in any propensity score variable will result in case deletion
- Treatment variables should be binary (0/1 or TRUE/FALSE)
- All specified variables must exist in the input data
- Bootstrap iterations that fail to converge are automatically excluded
- The function prints progress every 100 bootstrap iterations
- Love plots are automatically created and saved for quality control
- The `love_plots` subfolder is created automatically if it doesn't exist

## Author

Created: November 2025
Based on: `IPW_method.R`
