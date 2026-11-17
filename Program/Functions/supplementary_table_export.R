# Presentation-only export of the existing supplementary display tables.
# No analysis scripts are sourced and no estimates or diagnostic values are changed.

supplementary_export_ids <- function() {
  paste0("supptbl-", c(
    "excluded-characteristics", "training-results", "exercise_by_metformin_summary",
    "metformin-summary", "comparison-results", "extreme-weights", "sensitivity-diet",
    "sensitivity-exercise-type", "sensitivity-vitamins", "sensitivity-mutual-adjustment",
    "sensitivity-combinations", "subgroup-men",
    "subgroup-women", "subgroup-younger", "subgroup-older", "subgroup-european",
    "subgroup-noneuropean"
  ))
}

supplementary_export_paths <- function(root) {
  output <- file.path(root, "Output", "supplementary_tables")
  list(output = output,
       state = file.path(output, "render_state.rds"),
       bundle = file.path(output, "table_bundle.rds"),
       manifest = file.path(output, "export_manifest.rds"),
       captions = file.path(root, "Program", "References", "supplementary_table_captions.md"))
}

supplementary_export_fingerprint <- function(root) {
  paths <- c("Manuscript.qmd", "_quarto.yml", "Program/export_supplementary_tables.R",
             "Program/Functions/supplementary_table_export.R",
             "Program/Functions/manuscript_functions.R", "Program/Functions/display_names.R",
             "Program/References/supplementary_table_captions.md")
  stats::setNames(unname(tools::md5sum(file.path(root, paths))), paths)
}

# Compress only consecutive numbers. Gaps must never imply that a table is exported.
supplementary_number_ranges <- function(numbers) {
  if (!is.numeric(numbers) || !length(numbers) || any(!is.finite(numbers)) ||
      any(numbers < 1 | numbers != floor(numbers)) || anyDuplicated(numbers)) {
    stop("Supplementary table numbers must be distinct positive integers.", call. = FALSE)
  }
  numbers <- sort(numbers)
  starts <- c(1L, which(diff(numbers) != 1) + 1L)
  ends <- c(starts[-1L] - 1L, length(numbers))
  paste(vapply(seq_along(starts), function(i) {
    first <- numbers[starts[i]]
    last <- numbers[ends[i]]
    if (first == last) paste0("S", first) else paste0("S", first, "-", last)
  }, character(1)), collapse = ",")
}

# Keep the original caption text and inline R expressions in a separate source file.
read_supplementary_caption_templates <- function(path) {
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  pattern <- "^## Supplementary @(supptbl-[^:]+): (.+)$"
  starts <- grep(pattern, lines)
  ids <- sub(pattern, "\\1", lines[starts])
  if (anyDuplicated(ids) || !setequal(ids, supplementary_export_ids())) {
    stop("Supplementary caption templates must contain each exported table exactly once.", call. = FALSE)
  }
  ends <- c(starts[-1L] - 1L, length(lines))
  stats::setNames(lapply(seq_along(starts), function(i) {
    body <- lines[seq.int(starts[i] + 1L, ends[i])]
    body <- body[nzchar(trimws(body))]
    if (length(body) != 1L || !startsWith(body, "**")) {
      stop("Expected the original caption paragraph for ", ids[i], ".", call. = FALSE)
    }
    list(title = sub(pattern, "\\2", lines[starts[i]]), caption = body)
  }), ids)
}

# Evaluate only the existing inline expressions, in the manuscript's environment.
# This replaces the same inline evaluation previously performed by knitr.
supplementary_caption_text <- function(template, envir) {
  matches <- gregexpr("`r[[:space:]]+[^`]+`", template, perl = TRUE)
  expressions <- regmatches(template, matches)
  regmatches(template, matches) <- lapply(expressions, function(values) {
    vapply(values, function(value) {
      code <- sub("`$", "", sub("^`r[[:space:]]+", "", value))
      result <- eval(parse(text = code), envir = envir)
      if (length(result) != 1L || is.na(result)) {
        stop("A supplementary caption expression did not produce one value: ", code, call. = FALSE)
      }
      as.character(result)
    }, character(1), USE.NAMES = FALSE)
  })
  # Excel cells contain plain text; remove Markdown emphasis and escaped punctuation.
  template <- gsub("**", "", template, fixed = TRUE)
  gsub("\\\\([[:punct:]])", "\\1", template, perl = TRUE)
}

begin_supplementary_export <- function(root) {
  paths <- supplementary_export_paths(root)
  if (!file.exists(paths$state)) {
    stop("Missing supplementary export preparation. Render Manuscript.qmd with the project's Quarto hooks enabled.",
         call. = FALSE)
  }
  state <- readRDS(paths$state)
  if (!identical(state$fingerprint, supplementary_export_fingerprint(root))) {
    stop("Supplementary export preparation is stale. Restart the manuscript render.", call. = FALSE)
  }
  registry <- new.env(parent = emptyenv())
  registry$root <- root
  registry$state <- state
  registry$templates <- read_supplementary_caption_templates(paths$captions)
  registry$tables <- list()
  registry
}

register_supplementary_table <- function(registry, id, data, envir = parent.frame()) {
  if (!id %in% supplementary_export_ids() || id %in% names(registry$tables)) {
    stop("Unknown or duplicate supplementary export: ", id, call. = FALSE)
  }
  if (!is.data.frame(data) || !nrow(data) || !ncol(data) ||
      anyDuplicated(names(data)) || anyNA(names(data)) || any(!nzchar(names(data)))) {
    stop("Invalid supplementary display table: ", id, call. = FALSE)
  }
  # Only ordinary display columns are supported; do not coerce or reformat them.
  supported <- vapply(data, function(x) {
    is.character(x) || is.integer(x) || is.double(x) || is.logical(x)
  }, logical(1))
  if (!all(supported) || any(vapply(data, is.object, logical(1)))) {
    stop("Unsupported Excel column type in ", id, "; export requires plain display values.", call. = FALSE)
  }
  registry$tables[[id]] <- list(
    data = data,
    title = registry$templates[[id]]$title,
    caption = supplementary_caption_text(registry$templates[[id]]$caption, envir)
  )
  invisible(NULL)
}

finish_supplementary_export <- function(registry) {
  if (!setequal(names(registry$tables), supplementary_export_ids())) {
    stop("Not all ", length(supplementary_export_ids()),
         " supplementary tables were registered; no workbook can be exported.", call. = FALSE)
  }
  if (!identical(registry$state$fingerprint, supplementary_export_fingerprint(registry$root))) {
    stop("Manuscript sources changed during rendering; restart to export matching tables.", call. = FALSE)
  }
  saveRDS(list(state = registry$state, tables = registry$tables),
          supplementary_export_paths(registry$root)$bundle)
  invisible(NULL)
}

# Read resolved Quarto references, never infer numbers from source order.
# Accept an XML document separately so this can be checked with small fixtures.
supplementary_docx_references <- function(document, ids) {
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  body <- xml2::xml_find_first(document, "/w:document/w:body", ns)
  paragraphs <- xml2::xml_find_all(body, "./w:p[w:pPr/w:pStyle]", ns)
  styles <- xml2::xml_find_chr(paragraphs, "string(w:pPr/w:pStyle/@w:val)", ns)
  headings <- paragraphs[grepl("^Heading[1-9]$", styles, ignore.case = TRUE)]
  text <- function(node) {
    value <- paste(xml2::xml_text(xml2::xml_find_all(node, ".//w:t", ns)), collapse = "")
    trimws(gsub("\u00a0", " ", value, fixed = TRUE))
  }
  matches <- lapply(seq_along(headings), function(i) {
    refs <- xml2::xml_find_all(headings[[i]], ".//w:hyperlink[@w:anchor]", ns)
    anchors <- xml2::xml_attr(refs, "w:anchor", ns)
    keep <- which(anchors %in% ids)
    lapply(keep, function(j) {
      label <- text(refs[[j]])
      if (!grepl("^Table S[1-9][0-9]*$", label)) {
        stop("Unresolved supplementary reference for ", anchors[j], ": ", label, call. = FALSE)
      }
      list(id = anchors[j], number = as.integer(sub("^Table S", "", label)),
           sheet = label, position = i)
    })
  })
  references <- unlist(matches, recursive = FALSE)
  found <- vapply(references, function(x) x$id, character(1))
  numbers <- vapply(references, function(x) x$number, integer(1))
  if (anyDuplicated(found) || !setequal(found, ids) || anyNA(numbers) || anyDuplicated(numbers)) {
    stop("The rendered DOCX must contain one uniquely numbered heading per exported table. Missing: ",
         paste(setdiff(ids, found), collapse = ", "), call. = FALSE)
  }
  references
}

read_supplementary_docx <- function(path, ids) {
  connection <- unz(path, "word/document.xml", open = "rb")
  on.exit(close(connection))
  supplementary_docx_references(xml2::read_xml(connection), ids)
}

# Worksheet styling only: pass the original data frame directly to writeData().
build_supplementary_workbook <- function(tables, references) {
  wb <- openxlsx::createWorkbook(creator = "")
  openxlsx::modifyBaseFont(wb, fontSize = 11, fontName = "Arial")
  body_style <- openxlsx::createStyle(valign = "top", wrapText = TRUE)
  heading_style <- openxlsx::createStyle(textDecoration = "bold", valign = "top", wrapText = TRUE)
  separator_style <- openxlsx::createStyle(border = "right", borderColour = "#D9D9D9", borderStyle = "thin")
  for (reference in references) {
    entry <- tables[[reference$id]]
    data <- entry$data
    sheet <- reference$sheet
    columns <- seq_len(ncol(data))
    widths <- rep(18, ncol(data))
    widths[1] <- 32
    widths[names(data) == "Level"] <- 42
    widths[names(data) == "Residual imbalance"] <- 95
    widths[grepl("^CI |^95% CI", names(data))] <- 25
    openxlsx::addWorksheet(wb, sheet, gridLines = FALSE)
    openxlsx::writeData(wb, sheet, paste0("Supplementary ", sheet, ": ", entry$title),
                        startRow = 1, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet, cols = columns, rows = 1)
    openxlsx::addStyle(wb, sheet, heading_style, rows = 1, cols = 1)
    openxlsx::setRowHeights(wb, sheet, rows = 1, heights = 40)
    openxlsx::writeData(wb, sheet, data, startRow = 3, rowNames = FALSE,
                        headerStyle = heading_style, withFilter = TRUE, keepNA = FALSE)
    openxlsx::addStyle(wb, sheet, body_style, rows = 4:(nrow(data) + 3),
                       cols = columns, gridExpand = TRUE)
    openxlsx::addStyle(wb, sheet, separator_style, rows = 3:(nrow(data) + 3),
                       cols = 1, gridExpand = TRUE, stack = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = columns, widths = widths)
    openxlsx::setRowHeights(wb, sheet, rows = 3, heights = 32)
    # Estimate wrapped heights from copies of text, without changing any cell value.
    line_counts <- lapply(columns, function(j) {
      vapply(as.character(data[[j]]), function(value) {
        if (is.na(value) || !nzchar(value)) return(1)
        sum(pmax(1, ceiling(nchar(strsplit(value, "\n", fixed = TRUE)[[1]]) / (widths[j] * 0.8))))
      }, numeric(1), USE.NAMES = FALSE)
    })
    heights <- pmax(22, 16 * do.call(pmax, line_counts) + 6)
    if (any(heights > 409)) {
      # Excel limits row height; the complete text is retained in the cell.
      warning("Long cells in ", sheet, " exceed Excel's row-height limit; inspect their full text in the formula bar.",
              call. = FALSE)
    }
    openxlsx::setRowHeights(wb, sheet, rows = 4:(nrow(data) + 3), heights = pmin(409, heights))
    caption_row <- nrow(data) + 6
    openxlsx::writeData(wb, sheet, entry$caption, startRow = caption_row, colNames = FALSE)
    openxlsx::mergeCells(wb, sheet, cols = columns, rows = caption_row)
    openxlsx::addStyle(wb, sheet, body_style, rows = caption_row, cols = 1)
    caption_height <- max(45, 16 * ceiling(nchar(entry$caption) / (sum(widths) * 0.8)) + 12)
    openxlsx::setRowHeights(wb, sheet, rows = caption_row, heights = min(409, caption_height))
    openxlsx::freezePane(wb, sheet, firstActiveRow = 4)
  }
  wb
}

# Read back only the exported table rectangle; captions and titles are metadata.
verify_supplementary_workbook <- function(path, tables, references) {
  expected_sheets <- vapply(references, function(x) x$sheet, character(1))
  if (!identical(openxlsx::getSheetNames(path), expected_sheets)) {
    stop("Exported worksheet names/order do not match the resolved references.", call. = FALSE)
  }
  for (reference in references) {
    expected <- tables[[reference$id]]$data
    # check.names = FALSE alone still replaces header spaces with sep.names (".").
    # Preserve the displayed headers during readback so exact comparisons are valid.
    actual <- openxlsx::read.xlsx(path, sheet = reference$sheet,
                                 rows = 3:(nrow(expected) + 3), cols = seq_len(ncol(expected)),
                                 colNames = TRUE, check.names = FALSE, sep.names = " ",
                                 na.strings = character(),
                                 skipEmptyRows = FALSE, skipEmptyCols = FALSE)
    if (!identical(dim(actual), dim(expected))) {
      stop("Excel readback dimensions differ in ", reference$sheet, ": expected ",
           nrow(expected), " rows and ", ncol(expected), " columns; read ",
           nrow(actual), " rows and ", ncol(actual), " columns.", call. = FALSE)
    }
    if (!identical(names(actual), names(expected))) {
      stop("Excel readback column names differ in ", reference$sheet, ": expected ",
           paste(dQuote(names(expected)), collapse = ", "), "; read ",
           paste(dQuote(names(actual)), collapse = ", "), ".", call. = FALSE)
    }
    for (j in seq_along(expected)) {
      original <- expected[[j]]
      recovered <- actual[[j]]
      # Excel represents empty text and missing cells alike; numeric storage does not
      # distinguish R integers from doubles. Compare values without changing either table.
      if (is.character(original)) {
        equal <- (is.na(original) | original == "") & (is.na(recovered) | recovered == "")
        nonempty <- !is.na(original) & original != ""
        equal[nonempty] <- !is.na(recovered[nonempty]) & original[nonempty] == as.character(recovered[nonempty])
        valid <- all(equal)
      } else {
        valid <- isTRUE(all.equal(original, recovered, check.attributes = FALSE, tolerance = 0))
      }
      if (!valid) {
        stop("Excel changed cell values in ", reference$sheet, ", column ", names(expected)[j],
             "; the workbook will not be published.", call. = FALSE)
      }
    }
  }
  invisible(TRUE)
}
