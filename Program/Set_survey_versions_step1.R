
# Import packages ----

install.packages("pacman")
library(pacman)

p_load(dplyr)
# Load the necessary packages
p_load(ggplot2)
# Add writexl for Excel export
p_load(writexl)

p_load(here)


# Import all data ----

df_EPICv1_EPICv2_new <- read.csv(
  here::here("Data", "EPICv1 and v2 data", "All_metrics_crossectional_samples.csv"),
  header = TRUE,
  stringsAsFactors = FALSE
)


# Rename collumn DunedinPoAm to DunedinPACE as it was misnamed according to Natalia
names(df_EPICv1_EPICv2_new)[names(df_EPICv1_EPICv2_new) == "DunedinPoAm"] <- "DunedinPACE"

# df_EPICv1_old <- read.csv("C:/Users/danwik/OneDrive - Karolinska Institutet/Documents/Metformin project/Data/EPICv1 data/All_metrics_crossectional_samples.csv", header=TRUE, stringsAsFactors=FALSE)

# Replace all empty strings with NA ----

df_EPICv1_EPICv2_new <- df_EPICv1_EPICv2_new |> 
  mutate(across(where(is.character), ~na_if(., "")))

# Import functions ----

source(here::here("Program", "Functions", "survey_version_functions.R"))
source(here::here("Program", "Functions", "replace_na_with_false().R"))
source(here::here("Program", "Functions", "seperate_survey().R"))

# Add survey version collumn and use only survey 1 ----

cols_survey2_3 <- c("Hours.Sedentary.Remaining.Awake", "Primary.Diet.Past.Year", "Strictly.Followed.", "Red.Meat.times.per.week", "Processed.Food.times.per.week", "Feel.Well.Rested.days.per.week", "Screens.before.bed", "Track.Sleep.Wearable.Device")
# Remove rows with "other" in biological sex collumn

cols_survey3_1 <- c("Height.in", "Weight", "Lab.Fasting", "Anti.Aging.Interventions", "Menopause", "Given.Birth", "Hours.of.sleep.per.night")

df_EPICv1_EPICv2_new <- add_surv_version_col(df_EPICv1_EPICv2_new, cols_survey2_3, cols_survey3_1)

# Adjust survey version based on collection date for survey 1. Entries before this date cannot be survey 2 or 3.
df_EPICv1_EPICv2_new <- adjust_survey_version_by_date(df_EPICv1_EPICv2_new, "5/01/2023 8:00:00 PM", new_version = 1)

# Remove all surveys that are not version 1
df_EPICv1_EPICv2_new <- df_EPICv1_EPICv2_new |>
  filter(survey_version == "1")


### Save survey 1 
write.csv(df_EPICv1_EPICv2_new, file = here::here("Output", "data_after_step1.csv"), row.names = FALSE)

