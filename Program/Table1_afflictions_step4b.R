# Supplementary Table 1 extension: self-reported afflictions after all exclusions.
# Run independently from the repository root, or through Master_parallel.R:
# Rscript --vanilla Program/Table1_afflictions_step4b.R
# Requires Output/data_after_step4; does not change the analysis population.

summarize_self_reported_afflictions <- function(data, print_coding_counts = FALSE) {
  labels <- c(
    Cancer.Diagnosis = "Cancer diagnosis",
    Cardiovascular = "Cardiovascular",
    Respiratory.Disease = "Respiratory",
    Endocrine.Disease = "Endocrine",
    Skin.Hair = "Skin and hair",
    Gastrointestinal = "Gastrointestinal",
    Genito.Urinary = "Genitourinary",
    Musculoskeletal = "Musculoskeletal",
    Neuropsychological = "Neuropsychological",
    Reproductive = "Reproductive",
    Immune = "Immune"
  )
  missing_columns <- setdiff(c("DunedinPACE", names(labels)), names(data))
  if (length(missing_columns)) {
    stop("Missing columns for the self-reported afflictions table: ",
         paste(missing_columns, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data$DunedinPACE) ||
      any(!is.finite(data$DunedinPACE) | data$DunedinPACE <= 0)) {
    stop("Expected valid DunedinPACE values after the step 4 exclusions.", call. = FALSE)
  }

  # Use the original questionnaire responses, not pre-existing .any indicators.
  # Missing/unreported responses stay in the denominator and contribute zero.
  indicators <- lapply(data[names(labels)], function(x) {
    response <- tolower(trimws(as.character(x)))
    as.integer(!is.na(response) & !response %in% c("", "none", "no", "na"))
  })
  if (print_coding_counts) {
    coding_counts <- t(vapply(names(labels), function(column) {
      response <- trimws(as.character(data[[column]]))
      c(No = sum(response %in% "No"), no = sum(response %in% "no"),
        None = sum(response %in% "None"), none = sum(response %in% "none"),
        "NA / missing" = sum(is.na(response) | tolower(response) %in% "na"),
        Blank = sum(response %in% ""),
        "Other no/none spellings" = sum(tolower(response) %in% c("no", "none") &
                                        !response %in% c("No", "no", "None", "none")),
        "Total coded 0" = sum(indicators[[column]] == 0L))
    }, numeric(8)))
    cat("\nNo reported affliction: response-coding counts (N = ", nrow(data), ")\n", sep = "")
    print(data.frame(Variable = unname(labels), coding_counts, check.names = FALSE), row.names = FALSE)
    cat("NA / missing includes literal NA and missing values; earlier imports merged some NA/blank responses.\n")
  }
  groups <- list(
    Overall = rep(TRUE, nrow(data)),
    "DunedinPACE < 1" = data$DunedinPACE < 1,
    "DunedinPACE \u2265 1" = data$DunedinPACE >= 1
  )
  format_count <- function(count, denominator) {
    percentage <- if (denominator > 0) {
      paste0(formatC(100 * count / denominator, format = "f", digits = 1), "%")
    } else {
      "\u2014"
    }
    paste0(format(count, big.mark = ",", scientific = FALSE, trim = TRUE),
           " (", percentage, ")")
  }

  result <- data.frame(Variable = c("N", unname(labels)), stringsAsFactors = FALSE)
  for (group in names(groups)) {
    selected <- groups[[group]]
    denominator <- sum(selected)
    counts <- vapply(indicators, function(x) sum(x[selected]), numeric(1))
    result[[group]] <- c(
      # Like Table 1, the N-row percentage gives the share of the overall cohort.
      format_count(denominator, nrow(data)),
      vapply(counts, format_count, character(1), denominator = denominator)
    )
  }
  result
}

local({
  library(pacman)
  p_load(here, writexl)

  input_path <- here::here("Output", "data_after_step4")
  if (!file.exists(input_path)) {
    stop("Missing Output/data_after_step4. Run steps 1-4 before this script.", call. = FALSE)
  }
  input_env <- new.env(parent = emptyenv())
  loaded <- load(input_path, envir = input_env)
  if (!"data_after_step4" %in% loaded) {
    stop("Expected object data_after_step4 in Output/data_after_step4.", call. = FALSE)
  }
  table1_afflictions_df <- summarize_self_reported_afflictions(input_env$data_after_step4, print_coding_counts = TRUE)

  # Persist only the aggregate display table; manuscript rendering needs no raw data.
  saveRDS(table1_afflictions_df, here::here("Output", "table1_afflictions_df.rds"))
  writexl::write_xlsx(
    list(Self_reported_afflictions = table1_afflictions_df),
    path = here::here("Output", "Table1_SelfReportedAfflictions.xlsx")
  )
  print(table1_afflictions_df, row.names = FALSE)
  message("Saved Output/table1_afflictions_df.rds and Output/Table1_SelfReportedAfflictions.xlsx")
})
