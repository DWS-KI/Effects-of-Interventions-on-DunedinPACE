# =============================================================================
# IPW STEP 21: DIAGNOSTICS FOR ALL INTERVENTIONS
# =============================================================================
# This script performs additional IPW diagnostics requested by co-authors,
# for all interventions (especially rare interventions like Rapamycin, TA-65,
# Exosome Therapy, Stem Cell Therapy where poor positivity or extreme weights 
# can make ATE estimates unstable).
#
# It reports:
# 1. Propensity-score overlap
# 2. Standard Weights (Hajek Estimator)
# 3. Sensitivity analyses using truncated weights (1st/99th percentiles)
# 4. Bootstrapped Confidence Intervals for both methods
# 5. Comparative Forest Plot

# -----------------------------------------------------------------------------
# Load Required Packages
# -----------------------------------------------------------------------------
cat("Loading required packages...\n")
required_packages <- c("here", "dplyr", "ggplot2", "tidyr")
for (pkg in required_packages) {
  if (!require(pkg, quietly = TRUE, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# -----------------------------------------------------------------------------
# Source IPW Functions
# -----------------------------------------------------------------------------
cat("Loading IPW functions...\n")
source(here::here("Program", "Functions", "IPW_functions.R"))
source(here::here("Program", "Functions", "display_names.R"))
source(here::here("Program", "Functions", "forest_plot_functions.R"))

# -----------------------------------------------------------------------------
# Load Data
# -----------------------------------------------------------------------------
cat("Loading data from Step 4...\n")
load(here::here("Output", "data_after_step4"))
data <- data_after_step4

# -----------------------------------------------------------------------------
# Define Analysis Parameters
# -----------------------------------------------------------------------------
all_treatments <- c(
  "Metformin",
  "Antioxidants",
  "Fish.oil.omega",
  "Workout.supplements",
  "Herbal.supplements",
  "Vitamins",
  "Minerals",
  "TA.65",
  "NR.NAD",
  "Sulphoraphane.broccoli.extract",
  "Rapamycin",
  "DHEA",
  "Resveratrol",
  "Antiaging.Exosome",
  "Antiaging.Hormone.Replacement",
  "Antiaging.Senolytics",
  "Antiaging.Stem.Cell",
  "Antiaging.NAD"
)

outcome_name <- "DunedinPACE"

covariates <- c(
  "Decimal.Chronological.Age",
  "Biological.Sex",
  "Level.of.Education",
  "Tobacco.Use.Binary",
  "Alcohol.per.week",
  "Exercise.per.week",
  "BMI",
  "Stress.Level",
  "Hours.of.sleep.per.night"
)

n_bootstrap <- getOption("n_bootstrap", default = 1000)

# Set up output directory for plots
plot_dir <- here::here("Output", "IPW_diagnostics_plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# -----------------------------------------------------------------------------
# Run Diagnostics
# -----------------------------------------------------------------------------
results_list <- list()

for (treatment_name in all_treatments) {
  cat("\nRunning diagnostics and bootstrap for:", treatment_name, "\n")
  
  if (!treatment_name %in% names(data)) {
    cat("Warning:", treatment_name, "not found in data. Skipping.\n")
    next
  }
  
  # Prepare data
  df <- create_treatment_variable(data, treatment_name)
  df <- prepare_complete_cases(df, covariates, strict_treatment = TRUE)
  
  # Fit Propensity Score Model
  ps_model <- fit_propensity_model(df, covariates)
  ps <- predict(ps_model, type = "response")
  
  # 1. Weights
  w_standard <- ifelse(df$treatment == 1, 1 / ps, 1 / (1 - ps))
  
  lower_bound <- quantile(w_standard, 0.01)
  upper_bound <- quantile(w_standard, 0.99)
  w_truncated <- pmin(pmax(w_standard, lower_bound), upper_bound)
  
  # 2. Sample Sizes
  n_treated <- sum(df$treatment == 1)
  n_control <- sum(df$treatment == 0)
  
  # 3. ATE Point Estimates
  ate_standard <- estimate_ate_weighted(df[[outcome_name]], df$treatment, w_standard)
  ate_truncated <- estimate_ate_weighted(df[[outcome_name]], df$treatment, w_truncated)
  
  # 4. Bootstrap for CIs
  cat("  Running", n_bootstrap, "bootstrap iterations...\n")
  boot_std <- numeric(n_bootstrap)
  boot_trunc <- numeric(n_bootstrap)
  
  set.seed(12435)
  for (i in 1:n_bootstrap) {
    if (i %% 250 == 0) cat("    Iteration", i, "\n")
    boot_df <- df[sample(nrow(df), replace = TRUE), ]
    boot_ps_model <- tryCatch({ fit_propensity_model(boot_df, covariates) }, error = function(e) NULL)
    
    if (!is.null(boot_ps_model)) {
      b_ps <- predict(boot_ps_model, type = "response")
      
      b_w_std <- ifelse(boot_df$treatment == 1, 1/b_ps, 1/(1-b_ps))
      b_lb <- quantile(b_w_std, 0.01, na.rm = TRUE)
      b_ub <- quantile(b_w_std, 0.99, na.rm = TRUE)
      b_w_trunc <- pmin(pmax(b_w_std, b_lb), b_ub)
      
      boot_std[i] <- estimate_ate_weighted(boot_df[[outcome_name]], boot_df$treatment, b_w_std)
      boot_trunc[i] <- estimate_ate_weighted(boot_df[[outcome_name]], boot_df$treatment, b_w_trunc)
    } else {
      boot_std[i] <- NA; boot_trunc[i] <- NA
    }
  }
  
  # Calculate 95% CIs
  ci_std <- quantile(boot_std[!is.na(boot_std)], probs = c(0.025, 0.975))
  ci_trunc <- quantile(boot_trunc[!is.na(boot_trunc)], probs = c(0.025, 0.975))
  
  # Store Metrics
  metrics <- data.frame(
    Treatment = treatment_name,
    N_Treated = n_treated,
    N_Control = n_control,
    ATE_Standard = round(ate_standard, 6),
    CI_Lower_Standard = round(ci_std[1], 6),
    CI_Upper_Standard = round(ci_std[2], 6),
    ATE_Truncated = round(ate_truncated, 6),
    CI_Lower_Truncated = round(ci_trunc[1], 6),
    CI_Upper_Truncated = round(ci_trunc[2], 6)
  )
  results_list[[treatment_name]] <- metrics
  
  # ---------------------------------------------------------------------------
  # Generate Single-Treatment Plots
  # ---------------------------------------------------------------------------
  
  # Plot A: Propensity Score Overlap Density
  plot_data <- data.frame(
    PS = ps, 
    Treatment = factor(ifelse(df$treatment == 1, "Treated", "Control"))
  )
  p1 <- ggplot(plot_data, aes(x = PS, fill = Treatment)) +
    geom_density(alpha = 0.5) +
    labs(
      title = paste("Propensity Score Overlap:", treatment_name), 
      x = "Propensity Score", 
      y = "Density"
    ) +
    theme_minimal()
  ggsave(file.path(plot_dir, paste0("PS_Overlap_", treatment_name, ".png")), p1, width = 8, height = 6)
}

# -----------------------------------------------------------------------------
# Compile and Save Outputs
# -----------------------------------------------------------------------------
if (length(results_list) > 0) {
  diagnostics_table <- bind_rows(results_list)
  
  csv_path <- here::here("Output", "IPW_all_diagnostics_table.csv")
  rdata_path <- here::here("Output", "IPW_all_diagnostics_table.RData")
  
  write.csv(diagnostics_table, csv_path, row.names = FALSE)
  save(diagnostics_table, file = rdata_path)
  
  # ---------------------------------------------------------------------------
  # Create Comparative Forest Plot for All Treatments
  # ---------------------------------------------------------------------------
  cat("\nGenerating comparative forest plot...\n")
  
  # Reshape data for plotting
  plot_df <- data.frame()
  for (i in 1:nrow(diagnostics_table)) {
    row <- diagnostics_table[i, ]
    # Standard
    plot_df <- rbind(plot_df, data.frame(
      Treatment = row$Treatment,
      Method = "Main (Standard IPW)",
      ATE = row$ATE_Standard * 100,
      Lower = row$CI_Lower_Standard * 100,
      Upper = row$CI_Upper_Standard * 100
    ))

    # Truncated
    plot_df <- rbind(plot_df, data.frame(
      Treatment = row$Treatment,
      Method = "Truncated Weights (1st-99th)",
      ATE = row$ATE_Truncated * 100,
      Lower = row$CI_Lower_Truncated * 100,
      Upper = row$CI_Upper_Truncated * 100
    ))
  }
  
  # Apply display names
  plot_df$Treatment <- sapply(as.character(plot_df$Treatment), format_display_name)
  
  # Order treatments by the Main (Standard IPW) effect size
  main_effects <- plot_df[plot_df$Method == "Main (Standard IPW)", ]
  ordered_treatments <- main_effects$Treatment[order(main_effects$ATE)]
  plot_df$Treatment <- factor(
    plot_df$Treatment,
    levels = rev(ordered_treatments)
  )
  
  # Set colors
  colors <- c("Main (Standard IPW)" = "#008fd5", "Truncated Weights (1st-99th)" = "#de6b35")
  plot_df$Label <- plot_df$Treatment
  plot_df$group <- plot_df$Method
  p_forest <- create_forest_panel(plot_df, colors)

  forest_path <- file.path(plot_dir, "comparative_diagnostics_forest_plot.png")
  ggsave(filename = forest_path, plot = p_forest, width = 10, height = forest_plot_height(nlevels(plot_df$Label), plot = p_forest), dpi = 300, bg = "white")
  
  cat("\n========================================\n")
  cat("STEP 21 COMPLETED SUCCESSFULLY\n")
  cat("Diagnostics table saved to:\n")
  cat("  -", csv_path, "\n")
  cat("Comparative Forest Plot saved to:\n")
  cat("  -", forest_path, "\n")
  cat("========================================\n")
} else {
  cat("\nWarning: No results generated. Check treatment names.\n")
}
