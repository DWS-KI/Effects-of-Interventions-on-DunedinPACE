# SMD (Standardized Mean Difference) Column Documentation

## Overview

The IPW analysis now includes an automatic covariate balance assessment using **Standardized Mean Differences (SMD)** after inverse probability weighting. This is reported in the `SMD` column of the results dataframe.

## What is SMD?

Standardized Mean Difference (SMD) is a scale-free measure used to assess the balance of covariates between treatment and control groups after weighting. It measures the difference in means (or proportions) standardized by the pooled standard deviation.

**Interpretation:**
- **SMD < 0.1**: Generally considered good balance (rule of thumb)
- **SMD > 0.1**: May indicate residual imbalance requiring attention

## SMD Column Values

The `SMD` column in `IPW_results_df` shows one of two possible values:

### 1. `"<0.1"` - All Covariates Balanced

When you see `"<0.1"`, it means:
- All covariates have SMD < 0.1 after weighting
- The treatment and control groups are well-balanced
- The propensity score weighting successfully adjusted for confounding
- Results are likely more reliable

**Example:**
```r
   treatment           outcome     ate_estimate  SMD
   Antiaging.Stem.Cell DunedinPACE 0.0154       <0.1
```

### 2. Comma-Separated Covariate Names - Some Covariates Unbalanced

When you see covariate names (e.g., `"BMI, Stress.Level"`), it means:
- These specific covariates have SMD > 0.1 after weighting
- There is residual imbalance for these variables
- The propensity score model may not fully adjust for these confounders
- Consider sensitivity analyses or alternative modeling approaches

**Example:**
```r
   treatment    outcome     ate_estimate  SMD
   Metformin    DunedinPACE -0.0031      BMI, Stress.Level
```

This indicates that BMI and Stress.Level are not well-balanced between Metformin users and non-users even after weighting.

## How SMD is Calculated

### For Continuous Variables
```
SMD = (mean_treated - mean_control) / pooled_SD
```

Where:
- `mean_treated` = weighted mean in treatment group
- `mean_control` = weighted mean in control group  
- `pooled_SD` = sqrt((var_treated + var_control) / 2)

### For Binary/Categorical Variables
```
SMD = (prop_treated - prop_control) / pooled_SD
```

Where proportions are calculated using weighted means.

## What to Do with SMD Results

### If SMD = "<0.1" for all treatments
✅ **Good News!** Your analysis has achieved good covariate balance.
- Report this in your manuscript
- The propensity score weighting is working well
- Your ATE estimates are more credible

### If SMD lists unbalanced covariates
⚠️ **Consider the following:**

1. **Check the unbalanced variables:**
   - Are they clinically important confounders?
   - How large is the imbalance? (You can calculate exact SMD values separately)

2. **Potential actions:**
   - **Add interactions** to the propensity score model
   - **Include polynomial terms** (e.g., age² for non-linear relationships)
   - **Use alternative methods** (e.g., matching, trimming extreme weights)
   - **Conduct sensitivity analyses** to assess robustness

3. **Report transparently:**
   - Always report which covariates remain unbalanced
   - Discuss potential implications for your results
   - Consider sensitivity analyses

## Example Results Interpretation

```r
# Load results
load(here::here("Output", "IPW_results_df"))

# View results
print(IPW_results_df[, c("treatment", "outcome", "ate_estimate", "SMD")])
```

**Example Output:**
```
  treatment              outcome      ate_estimate  SMD
  Antiaging.Stem.Cell    DunedinPACE  0.0154       <0.1
  Antiaging.NAD          DunedinPACE -0.0008       <0.1
  Metformin              DunedinPACE -0.0031       BMI, Exercise.per.week
  Rapamycin              DunedinPACE  0.0089       Decimal.Chronological.Age
```

**Interpretation:**
- **Antiaging.Stem.Cell** and **Antiaging.NAD**: Good balance, results are reliable
- **Metformin**: BMI and exercise not well-balanced, consider sensitivity analysis
- **Rapamycin**: Age not well-balanced, may need age² term in propensity score model

## Technical Details

### Function: `calculate_weighted_smd()`
Calculates SMD for a single covariate using weighted means and variances.

### Function: `assess_covariate_balance()`
- Loops through all covariates
- Calculates SMD for each
- Returns "<0.1" if all balanced, otherwise lists unbalanced variables

### Integration
The balance assessment is automatically performed in `estimate_ate_for_treatment()` after calculating IPW weights and before estimating the ATE.

## References

**Standard threshold:**
- Austin, P.C. (2011). An Introduction to Propensity Score Methods for Reducing the Effects of Confounding in Observational Studies. *Multivariate Behavioral Research*, 46(3), 399-424.

**Recommendation:**
- SMD < 0.1 is widely used but not universal
- Some researchers use SMD < 0.2 or 0.25
- The 0.1 threshold is conservative and recommended for rigorous analyses

## Code Example

```r
# Run IPW analysis
source(here::here("Program", "Functions", "IPW_functions.R"))
load(here::here("Output", "data_after_step4"))

covariates <- c("Decimal.Chronological.Age", "Biological.Sex", 
                "Level.of.Education", "BMI")

results <- IPW(
  data = data_after_step4,
  treatment_list = c("Metformin", "Rapamycin"),
  outcome_list = c("DunedinPACE"),
  covariates = covariates,
  n_bootstrap = 1000
)

# Check balance
print(results[, c("treatment", "SMD")])

# Filter for well-balanced results only
well_balanced <- results[results$SMD == "<0.1", ]
print(well_balanced)
```

## Notes

- SMD is calculated using **absolute values** (always positive)
- The assessment happens **after weighting** (weighted SMD)
- Balance is checked for **all covariates** in the propensity score model
- The column stores a **character string** for easy interpretation
- Missing values in covariates are excluded before calculating SMD

---

**Last Updated:** November 2025
