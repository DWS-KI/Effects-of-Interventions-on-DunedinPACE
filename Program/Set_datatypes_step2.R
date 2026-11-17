### Import data and functions

library(dplyr)
library(here)

source(here::here("Program", "Functions", "remove_other().R"))
source(here::here("Program", "Functions", "logical_to_integer().R"))
source(here::here("Program", "Functions", "convert_yes_no_to_logical().R"))
source(here::here("Program", "Functions", "convert_to_logical().R"))
source(here::here("Program", "Functions", "char_cols_to_factors().R"))


data_after_step2 <- read.csv(here::here("Output", "data_after_step1.csv"), header=TRUE, stringsAsFactors=FALSE)

# Convert to Date format
data_after_step2$Collection.Date <- as.Date(data_after_step2$Collection.Date, format = "%m/%d/%Y")


data_after_step2<-convert_to_logical(data_after_step2, column_name="Diagnosed.with.low.or.high.calcium", false_values="No")

data_after_step2<-convert_to_logical(data_after_step2, column_name="Menopause", false_values="No")

data_after_step2<-convert_to_logical(data_after_step2, column_name="Given.Birth", false_values="No")

data_after_step2<-convert_to_logical(data_after_step2, column_name="Are.you.adopted.", false_values="No")

data_after_step2<-convert_to_logical(data_after_step2, column_name="Do.you.have.any.parents.or.grandparents.or.great.grandparents.that.live.to.or.beyond.100.years.old.", false_values="No")

data_after_step2<-convert_to_logical(data_after_step2, column_name="Track.Sleep.Wearable.Device", false_values="No")


data_after_step2<-convert_to_logical(data_after_step2, column_name="Lab.Fasting", false_values="No")



### True/false values need to be converted to 0/1 for stdReg package
data_after_step2 <- logical_to_integer(data_after_step2)

# Set all character columns to factors, except for the ID and date columns
data_after_step2 <- char_cols_to_factors(
  data_after_step2,
  max_categories = 50,
  cols_to_skip = c(
    "Patient.ID", "PID", "Collection.Date", "Array", "survey_version",
    "Kit.ID", "Date.of.Birth", "LIMS.Date", "Zip.Code", "Birth.Country"
    # Add other high-cardinality columns as needed
  )
)

### Save data as rdata files to keep formatting
# write.csv(data_after_step2, file = here::here("Output", "data_after_step2.csv"), row.names = FALSE)
save(data_after_step2, file = here::here("Output", "data_after_step2"))
