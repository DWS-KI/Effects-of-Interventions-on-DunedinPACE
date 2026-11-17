# =============================================================================
# STEP 3: REDUCE DIMENSIONS AND RECODE VARIABLES
# =============================================================================
# This script performs variable recoding and dimension reduction:
#   1. Tobacco.Use: Reduced to 2 levels (non-smokers vs Smokers)
#   2. Ethnicity: Recoded to 4 levels (Euro, Asia, Lati, Other)
#   3. European: Binary indicator (1 = Euro, 0 = Non-Euro)
#   4. Exercise: Binary variables for each frequency category
# NAs are preserved throughout all transformations

# Load required packages
library(pacman)
p_load(dplyr, here)

# Load data from step 2
load(here::here("Output", "data_after_step2"))
# The loaded object is named data_fixed_datatypes from step 2

# Check current Tobacco.Use levels
cat("\n=== Original Tobacco.Use Distribution ===\n")
original_table <- table(data_after_step2$Tobacco.Use, useNA = "ifany")
print(original_table)
cat("\nPercentages:\n")
print(round(prop.table(original_table) * 100, 2))

# Identify the category with the highest count (excluding NA)
tobacco_counts <- table(data_after_step2$Tobacco.Use)
highest_count_category <- names(tobacco_counts)[which.max(tobacco_counts)]

cat("\n=== Category with Highest Count ===\n")
cat("Category:", highest_count_category, "\n")
cat("Count:", max(tobacco_counts), "\n")

# Create new binary tobacco variable
# Keep the most common category as is, combine all others as "Smokers"
data_after_step3 <- data_after_step2 |>
  mutate(
    Tobacco.Use.Binary = case_when(
      is.na(Tobacco.Use) ~ NA_character_,
      Tobacco.Use == highest_count_category ~ as.character(Tobacco.Use),
      TRUE ~ "Smokers"
    )
  )

# Convert to factor with the non-smoker category as reference level
data_after_step3$Tobacco.Use.Binary <- factor(
  data_after_step3$Tobacco.Use.Binary,
  levels = c(highest_count_category, "Smokers")
)

# Verify the new variable
cat("\n=== New Tobacco.Use.Binary Distribution ===\n")
new_table <- table(data_after_step3$Tobacco.Use.Binary, useNA = "ifany")
print(new_table)
cat("\nPercentages:\n")
print(round(prop.table(new_table) * 100, 2))

# Cross-tabulation to verify the recoding
cat("\n=== Cross-tabulation: Original vs Binary ===\n")
print(table(Original = data_after_step3$Tobacco.Use, 
            Binary = data_after_step3$Tobacco.Use.Binary, 
            useNA = "ifany"))

# ===== Ethnicity Recoding =====
# Reduce ethnicity to 3 levels: Euro, Asia, and Other Ethnicity

cat("\n=== Original Ethnicity Distribution ===\n")
ethnicity_original_table <- table(data_after_step3$Ethnicity, useNA = "ifany")
print(ethnicity_original_table)
cat("\nPercentages:\n")
print(round(prop.table(ethnicity_original_table) * 100, 2))

# Create new 3-level ethnicity variable with explicit categories
data_after_step3 <- data_after_step3 |>
  mutate(
    Ethnicity.Recoded = case_when(
      is.na(Ethnicity) ~ NA_character_,
      Ethnicity == "Euro" ~ "Euro",
      Ethnicity == "Asia" ~ "Asia",
      Ethnicity == "Lati" ~ "Latin",
      TRUE ~ "Other Ethnicity"
    )
  )

# Convert to factor with Euro as reference level
data_after_step3$Ethnicity.Recoded <- factor(
  data_after_step3$Ethnicity.Recoded,
  levels = c("Euro", "Asia", "Latin", "Other Ethnicity")
)

# ===== European Binary Variable =====
# Create binary indicator: 1 if Euro, 0 otherwise

data_after_step3 <- data_after_step3 |>
  mutate(
    European = case_when(
      is.na(Ethnicity.Recoded) ~ NA_integer_,
      Ethnicity.Recoded == "Euro" ~ 1L,
      TRUE ~ 0L
    )
  )

# Verify the new variable
cat("\n=== New European Binary Variable Distribution ===\n")
european_table <- table(data_after_step3$European, useNA = "ifany")
print(european_table)
cat("\nPercentages:\n")
print(round(prop.table(european_table) * 100, 2))

# Cross-tabulation to verify the recoding
cat("\n=== Cross-tabulation: Ethnicity.Recoded vs European ===\n")
print(table(Ethnicity = data_after_step3$Ethnicity.Recoded, 
            European = data_after_step3$European, 
            useNA = "ifany"))

# ===== Exercise Binary Categories =====
# Create binary columns for each exercise frequency category

cat("\n=== Original Exercise.per.week Distribution ===\n")
exercise_original_table <- table(data_after_step3$Exercise.per.week, useNA = "ifany")
print(exercise_original_table)
cat("\nPercentages:\n")
print(round(prop.table(exercise_original_table) * 100, 2))

# Create binary columns for each exercise category
data_after_step3 <- data_after_step3 |>
  mutate(
    # Binary: Exercising (1) vs Never (0)
    Exercising = case_when(
      is.na(Exercise.per.week) ~ NA_integer_,
      Exercise.per.week == "Never" ~ 0L,
      TRUE ~ 1L
    ),
    Exercise.1to2.per.week = case_when(
      is.na(Exercise.per.week) ~ NA_integer_,
      Exercise.per.week == "1-2 times per week" ~ 1L,
      TRUE ~ 0L
    ),
    Exercise.3to4.per.week = case_when(
      is.na(Exercise.per.week) ~ NA_integer_,
      Exercise.per.week == "3-4 times per week" ~ 1L,
      TRUE ~ 0L
    ),
    Exercise.5to7.per.week = case_when(
      is.na(Exercise.per.week) ~ NA_integer_,
      Exercise.per.week == "5-7 times per week" ~ 1L,
      TRUE ~ 0L
    ),
    Exercise.8plus.per.week = case_when(
      is.na(Exercise.per.week) ~ NA_integer_,
      Exercise.per.week == "8 or more times per week" ~ 1L,
      TRUE ~ 0L
    ),
    Exercise.Never = case_when(
      is.na(Exercise.per.week) ~ NA_integer_,
      Exercise.per.week == "Never" ~ 1L,
      TRUE ~ 0L
    )
  )

# Verify the new binary variables
cat("\n=== New Exercise Binary Variables Distribution ===\n")
cat("Exercising (binary: 0=Never, 1=Any exercise):\n")
print(table(data_after_step3$Exercising, useNA = "ifany"))
cat("\nExercise.1to2.per.week:\n")
print(table(data_after_step3$Exercise.1to2.per.week, useNA = "ifany"))
cat("\nExercise.3to4.per.week:\n")
print(table(data_after_step3$Exercise.3to4.per.week, useNA = "ifany"))
cat("\nExercise.5to7.per.week:\n")
print(table(data_after_step3$Exercise.5to7.per.week, useNA = "ifany"))
cat("\nExercise.8plus.per.week:\n")
print(table(data_after_step3$Exercise.8plus.per.week, useNA = "ifany"))
cat("\nExercise.Never:\n")
print(table(data_after_step3$Exercise.Never, useNA = "ifany"))

# Cross-tabulation to verify the recoding
cat("\n=== Cross-tabulation: Original Exercise vs Binary Categories ===\n")
cat("Exercise.1to2.per.week:\n")
print(table(Original = data_after_step3$Exercise.per.week, 
            Binary = data_after_step3$Exercise.1to2.per.week, 
            useNA = "ifany"))
cat("\nExercise.3to4.per.week:\n")
print(table(Original = data_after_step3$Exercise.per.week, 
            Binary = data_after_step3$Exercise.3to4.per.week, 
            useNA = "ifany"))
cat("\nExercise.5to7.per.week:\n")
print(table(Original = data_after_step3$Exercise.per.week, 
            Binary = data_after_step3$Exercise.5to7.per.week, 
            useNA = "ifany"))
cat("\nExercise.8plus.per.week:\n")
print(table(Original = data_after_step3$Exercise.per.week, 
            Binary = data_after_step3$Exercise.8plus.per.week, 
            useNA = "ifany"))
cat("\nExercise.Never:\n")
print(table(Original = data_after_step3$Exercise.per.week, 
            Binary = data_after_step3$Exercise.Never, 
            useNA = "ifany"))

# Save the data after step 3
save(data_after_step3, file = here::here("Output", "data_after_step3"))

cat("\n=== Step 3 Complete ===\n")
cat("Data saved to:", here::here("Output", "data_after_step3"), "\n")
cat("\nNew variable 'Tobacco.Use.Binary' created with 2 levels:\n")
cat("- Level 1 (reference):", highest_count_category, "\n")
cat("- Level 2: Smokers (all other categories combined)\n")
cat("- NAs preserved:", sum(is.na(data_after_step3$Tobacco.Use.Binary)), "\n")
cat("\nNew variable 'Ethnicity.Recoded' created with 4 levels:\n")
cat("- Level 1 (reference): Euro\n")
cat("- Level 2: Asia\n")
cat("- Level 3: Lati\n")
cat("- Level 4: Other Ethnicity (all remaining categories combined)\n")
cat("- NAs preserved:", sum(is.na(data_after_step3$Ethnicity.Recoded)), "\n")
cat("\nNew variable 'European' created (binary):\n")
cat("- 1 = Euro:", sum(data_after_step3$European == 1, na.rm = TRUE), "\n")
cat("- 0 = Non-Euro:", sum(data_after_step3$European == 0, na.rm = TRUE), "\n")
cat("- NAs preserved:", sum(is.na(data_after_step3$European)), "\n")
cat("\nNew binary exercise variables created:\n")
cat("- Exercising (0=Never, 1=Any): 1 =", sum(data_after_step3$Exercising == 1, na.rm = TRUE), ", 0 =", sum(data_after_step3$Exercising == 0, na.rm = TRUE), "\n")
cat("- Exercise.1to2.per.week: 1 =", sum(data_after_step3$Exercise.1to2.per.week == 1, na.rm = TRUE), "\n")
cat("- Exercise.3to4.per.week: 1 =", sum(data_after_step3$Exercise.3to4.per.week == 1, na.rm = TRUE), "\n")
cat("- Exercise.5to7.per.week: 1 =", sum(data_after_step3$Exercise.5to7.per.week == 1, na.rm = TRUE), "\n")
cat("- Exercise.8plus.per.week: 1 =", sum(data_after_step3$Exercise.8plus.per.week == 1, na.rm = TRUE), "\n")
cat("- Exercise.Never: 1 =", sum(data_after_step3$Exercise.Never == 1, na.rm = TRUE), "\n")
cat("- NAs preserved in all binary variables:", sum(is.na(data_after_step3$Exercise.1to2.per.week)), "\n")
