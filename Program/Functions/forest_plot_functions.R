# =============================================================================
# FOREST PLOT FUNCTIONS FOR TRAINING ANALYSIS VISUALIZATION
# =============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(here)

# Source display names function for reader-friendly labels
source(here::here("Program", "Functions", "display_names.R"))
source(here::here("Program", "Functions", "combined_plot_style.R"))

# Shared rendering for every forest plot; callers retain their existing units.
forest_ci_bounds <- function(ci_string) {
  parts <- str_split(str_remove_all(ci_string, "[\\[\\]%]"), ",")[[1]]
  values <- suppressWarnings(as.numeric(trimws(parts)))
  list(lower = values[1], upper = values[2])
}

forest_effect_limits <- function(df) {
  values <- c(0, df$ATE, df$Lower, df$Upper)
  limits <- range(values[is.finite(values)])
  if (diff(limits) == 0) limits <- limits + c(-1, 1)
  limits
}

forest_plot_height <- function(n_labels, panels = FALSE, plot = NULL) {
  if (!is.null(plot)) return(attr(plot, "forest_geometry")$height)
  ceiling(max(5, 0.36 * n_labels + if (panels) 2.5 else 1.5) * 2) / 2
}

# Measure decorations on the export device, then add height only to the panel.
# Fixed physical panel heights also preserve spacing when saved objects are printed.
size_forest_plot <- function(p, n_labels, panels = FALSE, panel_range = NULL) {
  if (panels) p <- p & theme(panel.heights = NULL)
  else p <- p + theme(panel.heights = NULL)
  base_height <- forest_plot_height(n_labels, panels)
  ragg::agg_capture(width = if (panels) 13 else 10, height = base_height,
                    units = "in", res = 300)
  on.exit(grDevices::dev.off(), add = TRUE)
  grob <- if (panels) patchwork::patchworkGrob(p) else ggplotGrob(p)
  decoration_height <- grid::convertHeight(sum(grob$heights), "in", valueOnly = TRUE)
  old_panel_height <- base_height - decoration_height
  stopifnot(old_panel_height > 0)
  offset <- min(0.20, max(0.11, 0.022 * n_labels))
  pitch <- 2 * offset + 1.25 * (1 - 2 * offset)
  old_range <- n_labels + 0.2
  new_range <- 1.2 + (n_labels - 1) * pitch
  if (!is.null(panel_range)) new_range <- panel_range
  panel_height <- old_panel_height * new_range / old_range
  sizing <- theme(panel.heights = grid::unit(panel_height, "in"))
  p <- if (panels) p & sizing else p + sizing
  attr(p, "forest_geometry") <- list(
    height = decoration_height + panel_height, panel_height = panel_height,
    old_panel_height = old_panel_height, old_range = old_range, new_range = new_range,
    pitch = pitch, offset = offset)
  p
}

# Label is an ordered factor (bottom to top); colors explicitly name each group.
# Fixed row offsets keep missing estimates from moving the remaining series.
create_forest_panel <- function(df, colors, x_label = " ",
                                effect_limits = forest_effect_limits(df),
                                method = NULL, tag = NULL, show_labels = TRUE,
                                label_aware_spacing = FALSE) {
  label_order <- levels(df$Label)
  df$group <- factor(df$group, levels = names(colors))
  # Keep within-pair offsets fixed and increase the gap to the next pair by 25%.
  separation <- min(0.20, max(0.11, 0.022 * length(label_order)))
  pitch <- 2 * separation + 1.25 * (1 - 2 * separation)
  row_centers <- 1 + (seq_along(label_order) - 1) * pitch
  wrapped_labels <- str_wrap(label_order, width = 34)
  row_limits <- c(0.4, tail(row_centers, 1) + 0.6)
  if (label_aware_spacing) {
    # Reserve space for every wrapped line without changing existing figures.
    row_units <- pmax(1, (str_count(wrapped_labels, "\n") + 1) * 0.55 + 0.35)
    row_centers <- 1 + c(0, cumsum((head(row_units, -1) + tail(row_units, -1)) / 2 * pitch))
    row_limits <- c(0.4 - (row_units[1] - 1) * pitch / 2,
                    tail(row_centers, 1) + 0.6 + (tail(row_units, 1) - 1) * pitch / 2)
  }
  cap_width <- min(0.18, max(0.055, 0.018 * length(label_order)))
  offsets <- setNames(seq(separation, -separation, length.out = length(colors)), names(colors))
  df$.forest_y <- row_centers[as.numeric(df$Label)] + unname(offsets[as.character(df$group)])

  p <- ggplot(df, aes(x = ATE, y = .forest_y, color = group)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.7, color = "black") +
    geom_errorbar(aes(xmin = Lower, xmax = Upper), orientation = "y",
                  width = cap_width, linewidth = 0.75, alpha = 1, na.rm = TRUE) +
    geom_point(size = 3.5, shape = 16, alpha = 1, na.rm = TRUE) +
    scale_color_manual(values = colors, limits = names(colors), drop = FALSE, name = NULL,
                       labels = function(labels) ifelse(labels == "G-computation", "G-Computation", labels)) +
    scale_x_continuous(name = "DunedinPACE (ATE, %)", limits = effect_limits,
                       breaks = scales::breaks_pretty(n = 5),
                       expand = expansion(mult = 0.05)) +
    scale_y_continuous(name = x_label, breaks = row_centers,
                       labels = wrapped_labels,
                       limits = row_limits,
                       minor_breaks = NULL, expand = expansion(mult = 0)) +
    labs(title = if (identical(method, "G-computation")) "G-Computation" else method, tag = tag) +
    theme_classic(base_size = 16) +
    theme(
      axis.title.x = element_text(size = 17, margin = margin(t = 12)),
      axis.text.x = element_text(size = 13, color = "#4A4A4A"),
      axis.text.y = element_text(size = 14, color = "#4A4A4A", margin = margin(r = 8)),
      axis.ticks.y = element_blank(),
      axis.line = element_blank(),
      panel.border = element_rect(color = "#4A4A4A", fill = NA, linewidth = 0.6),
      panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.6),
      panel.grid.minor = element_line(color = "#F2F2F2", linewidth = 0.4),
      legend.position = "bottom",
      legend.text = element_text(size = 14),
      legend.key.width = grid::unit(1, "cm"),
      plot.title = element_text(size = 16, hjust = 0.5, margin = margin(b = 12)),
      plot.tag = element_text(size = 18, face = "bold"),
      plot.tag.position = c(0.01, 0.99),
      plot.margin = margin(t = 15, r = 15, b = 10, l = 10)
    )
  if (!show_labels) {
    p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  }
  if (!is.null(method)) p <- p + guides(color = guide_legend(ncol = 1))
  size_forest_plot(p, length(label_order),
                  panel_range = if (label_aware_spacing) diff(row_limits) else NULL)
}

#' Create grouped forest plot from training results table
#'
#' @param results_table Data frame with columns: Treatment, `ATE (IPW)`, 
#'   `95% CI (IPW)`, `ATE (G-comp)`, `95% CI (G-comp)`
#' @param output_path Optional path to save the plot (default: NULL, no saving)
#' @param style Use combined only for the two combined manuscript figures.
#' @return ggplot object
create_training_forest_plot <- function(results_table, output_path = NULL,
                                        style = c("standalone", "combined")) {
  style <- match.arg(style)
  
  # Parse CI strings to extract lower and upper bounds
  parse_ci <- function(ci_string) {
    # Remove brackets and split by comma
    bounds <- forest_ci_bounds(ci_string)
    list(lower = bounds$lower * 100, upper = bounds$upper * 100)
  }
  
  # Prepare IPW data
  ipw_data <- results_table |>
    rowwise() |>
    mutate(
      ci_parsed = list(parse_ci(`95% CI (IPW)`)),
      Lower = ci_parsed$lower,
      Upper = ci_parsed$upper,
      ATE = `ATE (IPW)` * 100,
      group = "IPW"
    ) |>
    ungroup() |>
    select(Treatment, ATE, Lower, Upper, group)
  
  # Prepare G-comp data
  gcomp_data <- results_table |>
    rowwise() |>
    mutate(
      ci_parsed = list(parse_ci(`95% CI (G-comp)`)),
      Lower = ci_parsed$lower,
      Upper = ci_parsed$upper,
      ATE = `ATE (G-comp)` * 100,
      group = "G-computation"
    ) |>
    ungroup() |>
    select(Treatment, ATE, Lower, Upper, group)
  
  # Combine datasets
  df <- rbind(ipw_data, gcomp_data)
  
  # Apply display names, then remove the contextual prefix used elsewhere so
  # this plot retains its concise exercise-frequency labels.
  treatment_labels <- vapply(
    as.character(df$Treatment),
    format_display_name,
    character(1)
  ) |>
    str_remove("^Exercise\\s+")

  # Define outcome order (bottom to top)
  outcome_order <- rev(c("1-2 Times per Week", "3-4 Times per Week", "5-7 Times per Week", "8+ Times per Week"))

  unexpected_treatments <- unique(treatment_labels[!(treatment_labels %in% outcome_order)])
  if (length(unexpected_treatments) > 0) {
    stop(
      "Unexpected treatment label(s) in training forest plot: ",
      paste(unexpected_treatments, collapse = ", "),
      call. = FALSE
    )
  }

  df$Treatment <- factor(treatment_labels, levels = outcome_order)
  
  # Define colours for dots and bars
  colors <- c("IPW" = "#de6b35", "G-computation" = "#008fd5")

  # Create plot
  df$Label <- df$Treatment
  if (style == "combined") {
    p <- style_combined_forest_plot(df, effect_label = "DunedinPACE (ATE, %)")
  } else {
    p <- create_forest_panel(df, colors)
  }

  # Save if path provided
  if (!is.null(output_path)) {
    ggsave(
      filename = output_path,
      plot = p,
      width = 10,
      height = if (style == "combined") forest_plot_height(nlevels(df$Label)) else
        forest_plot_height(nlevels(df$Label), plot = p),
      dpi = 300,
      bg = "white"
    )
    cat("Forest plot saved to:", output_path, "\n")
  }
  
  return(p)
}


#' Create generalized forest plot for comparing IPW and G-computation results
#'
#' @param results_table Data frame with columns: label_col, `ATE (IPW)`, 
#'   `CI (IPW)`, `ATE (G-comp)`, `CI (G-comp)`
#' @param label_col Character string specifying which column to use for labels
#' @param x_label Label for x-axis (the categorical variable)
#' @param title Plot title retained for backward compatibility but not displayed
#' @param subtitle Plot subtitle retained for backward compatibility but not displayed
#' @param output_path Optional path to save the plot (default: NULL, no saving)
#' @param style Use combined only for the two combined manuscript figures.
#' @return ggplot object
create_comparison_forest_plot <- function(results_table, 
                                          label_col = "Analysis",
                                          x_label = " ",
                                          title = "Treatment Effects on Biological Aging",
                                          subtitle = "Comparing IPW and G-computation methods",
                                          output_path = NULL,
                                          style = c("standalone", "combined"),
                                          label_aware_spacing = FALSE) {
  style <- match.arg(style)
  
 # Parse percentage strings to numeric (handles "X.XX%" format)
  parse_pct <- function(pct_string) {
    as.numeric(gsub("%", "", pct_string))
  }
  
  # Parse CI strings to extract lower and upper bounds
  parse_ci <- function(ci_string) {
    # Remove brackets and percentage signs, split by comma
    forest_ci_bounds(ci_string)
  }
  
  # Prepare IPW data
  ipw_data <- results_table |>
    rowwise() |>
    mutate(
      ci_parsed = list(parse_ci(`CI (IPW)`)),
      Lower = ci_parsed$lower,
      Upper = ci_parsed$upper,
      ATE = parse_pct(`ATE (IPW)`),
      group = "IPW",
      Label = .data[[label_col]]
    ) |>
    ungroup() |>
    select(Label, ATE, Lower, Upper, group)
  
  # Prepare G-comp data
  gcomp_data <- results_table |>
    rowwise() |>
    mutate(
      ci_parsed = list(parse_ci(`CI (G-comp)`)),
      Lower = ci_parsed$lower,
      Upper = ci_parsed$upper,
      ATE = parse_pct(`ATE (G-comp)`),
      group = "G-computation",
      Label = .data[[label_col]]
    ) |>
    ungroup() |>
    select(Label, ATE, Lower, Upper, group)
  
  # Combine datasets
  df <- rbind(ipw_data, gcomp_data)
  
  # Apply display names to Label column
  df$Label <- sapply(df$Label, format_display_name)
  
  # Preserve order from input table (bottom to top)
  # Apply display names to get proper order
  label_order <- rev(sapply(unique(results_table[[label_col]]), format_display_name))
  df$Label <- factor(df$Label, levels = label_order)
  
  # Define colours for dots and bars
  colors <- c("IPW" = "#de6b35", "G-computation" = "#008fd5")

  # Create plot
  if (style == "combined") {
    p <- style_combined_forest_plot(df, effect_label = "DunedinPACE (ATE, %)")
  } else {
    p <- create_forest_panel(df, colors, x_label = x_label,
                             label_aware_spacing = label_aware_spacing)
  }

  # Save if path provided
  if (!is.null(output_path)) {
    ggsave(
      filename = output_path,
      plot = p,
      width = 10,
      height = if (style == "combined") forest_plot_height(nlevels(df$Label)) else
        forest_plot_height(nlevels(df$Label), plot = p),
      dpi = 300,
      bg = "white"
    )
    cat("Forest plot saved to:", output_path, "\n")
  }
  
  return(p)
}


#' Create a two-panel forest plot comparing two datasets within each method
#'
#' @param results_table_1 Data frame for dataset 1
#' @param results_table_2 Data frame for dataset 2
#' @param dataset1_name Name of dataset 1
#' @param dataset2_name Name of dataset 2
#' @param label_col Character string specifying which column to use for labels
#' @param x_label Label for x-axis
#' @param title Plot title
#' @param subtitle Plot subtitle
#' @param output_path Optional path to save the plot
#' @return patchwork composite with A: IPW and B: G-computation
create_overlay_forest_plot <- function(results_table_1, results_table_2,
                                       dataset1_name, dataset2_name,
                                       label_col = "Treatment",
                                       x_label = " ",
                                       title = "Treatment Effects on Biological Aging",
                                       subtitle = "Comparing methods across datasets",
                                       output_path = NULL) {
  
  # Parse percentage strings to numeric
  parse_pct <- function(pct_string) {
    as.numeric(gsub("%", "", pct_string))
  }
  
  # Parse CI strings to extract lower and upper bounds
  parse_ci <- function(ci_string) {
    forest_ci_bounds(ci_string)
  }
  
  process_table <- function(df, dataset_name) {
    if (nrow(df) == 0) return(tibble(
      Label = character(), ATE = numeric(), Lower = numeric(), Upper = numeric(), group = character()
    ))
    
    ipw_data <- df |>
      rowwise() |>
      mutate(
        ci_parsed = list(parse_ci(`CI (IPW)`)),
        Lower = ci_parsed$lower,
        Upper = ci_parsed$upper,
        ATE = parse_pct(`ATE (IPW)`),
        group = paste0("IPW (", dataset_name, ")"),
        Label = .data[[label_col]]
      ) |>
      ungroup() |>
      select(Label, ATE, Lower, Upper, group)
    
    gcomp_data <- df |>
      rowwise() |>
      mutate(
        ci_parsed = list(parse_ci(`CI (G-comp)`)),
        Lower = ci_parsed$lower,
        Upper = ci_parsed$upper,
        ATE = parse_pct(`ATE (G-comp)`),
        group = paste0("G-comp (", dataset_name, ")"),
        Label = .data[[label_col]]
      ) |>
      ungroup() |>
      select(Label, ATE, Lower, Upper, group)
    
    rbind(ipw_data, gcomp_data)
  }
  
  df1 <- process_table(results_table_1, dataset1_name)
  df2 <- process_table(results_table_2, dataset2_name)
  
  df <- rbind(df1, df2)
  
  # Apply display names to Label column
  df$Label <- sapply(df$Label, format_display_name)
  
  # Preserve order from input table 1 (bottom to top)
  all_labels <- unique(c(results_table_1[[label_col]], results_table_2[[label_col]]))
  label_order <- rev(sapply(all_labels, format_display_name))
  df$Label <- factor(df$Label, levels = label_order)
  
  if (!length(label_order)) stop("No treatment rows available for the forest plot.", call. = FALSE)

  # Group ordering: IPW 1, IPW 2, G-comp 1, G-comp 2
  df$group <- factor(df$group, levels = c(
    paste0("IPW (", dataset1_name, ")"),
    paste0("IPW (", dataset2_name, ")"),
    paste0("G-comp (", dataset1_name, ")"),
    paste0("G-comp (", dataset2_name, ")")
  ))
  
  # Ensure all groups are present for each Label, leaving empty spaces for missing data
  df <- df |> tidyr::complete(Label, group)
  
  # Define colours (distinct but grouped)
  # Dataset 1: blue, Dataset 2: orange, identical for IPW and G-computation.
  colors <- setNames(c("#008fd5", "#de6b35"), c(dataset1_name, dataset2_name))
  limits <- forest_effect_limits(df)
  groups <- levels(df$group)
  panels <- lapply(seq_len(2), function(i) {
    selected <- groups[seq_len(2) + (i - 1) * 2]
    panel_data <- df[df$group %in% selected, ]
    panel_data$Method <- c("IPW", "G-computation")[i]
    panel_data$group <- factor(as.character(panel_data$group), levels = selected,
                               labels = c(dataset1_name, dataset2_name))
    create_forest_panel(
      panel_data, colors,
      x_label = x_label, effect_limits = limits,
      method = c("IPW", "G-computation")[i], tag = c("A", "B")[i],
      show_labels = i == 1
    )
  })
  p <- patchwork::wrap_plots(panels, nrow = 1, widths = c(1, 1), guides = "keep")
  p <- size_forest_plot(p, length(label_order), panels = TRUE)

  # Save if path provided
  if (!is.null(output_path)) {
    ggsave(
      filename = output_path,
      plot = p,
      width = 13,
      height = forest_plot_height(length(label_order), panels = TRUE, plot = p),
      dpi = 300,
      bg = "white"
    )
    cat("Forest plot saved to:", output_path, "\n")
  }
  
  return(p)
}
