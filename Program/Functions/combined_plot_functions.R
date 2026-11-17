# Reuses saved balance values and result tables; no analysis or model fitting.
source(here::here("Program", "Functions", "love_plot_functions.R"))
source(here::here("Program", "Functions", "forest_plot_functions.R"))

validate_combined_balance <- function(data, label) {
  required <- c("covariate", "status", "smd")
  if (!is.data.frame(data) || !all(required %in% names(data)) || !nrow(data))
    stop(label, " must contain covariate, status and smd columns.", call. = FALSE)
  if (anyNA(data$covariate) || any(!nzchar(as.character(data$covariate))) ||
      !is.numeric(data$smd) || any(!is.finite(data$smd)) ||
      anyNA(data$status) ||
      !setequal(unique(data$status), c("smd_before", "smd_after")) ||
      anyDuplicated(data[c("covariate", "status")]) ||
      any(table(data$covariate) != 2L))
    stop(label, " requires one finite before/after SMD pair per covariate level.", call. = FALSE)
  invisible(data)
}

compose_love_forest <- function(love, forest, demo = FALSE) {
  # Native vector grobs: no PNG import, raster resizing or nested image margins.
  love_grob <- ggplot2::ggplotGrob(love)
  forest_grob <- ggplot2::ggplotGrob(forest)
  # Centre titles/legends over the whole column, including its row labels.
  # Otherwise long labels push them beyond the right edge of a narrow panel.
  centre_footer <- function(grob) {
    rows <- grob$layout$name %in% c("xlab-b", "guide-box", "guide-box-bottom")
    grob$layout$l[rows] <- 1L
    grob$layout$r[rows] <- length(grob$widths)
    grob
  }
  love_grob <- centre_footer(love_grob)
  forest_grob <- centre_footer(forest_grob)
  if (length(love_grob$heights) == length(forest_grob$heights)) {
    heights <- grid::unit.pmax(love_grob$heights, forest_grob$heights)
    love_grob$heights <- forest_grob$heights <- heights
  }
  heights <- grid::unit.c(grid::unit(6, "mm"), grid::unit(1, "null"))
  if (demo) heights <- grid::unit.c(heights, grid::unit(5, "mm"))
  figure <- gtable::gtable(
    widths = grid::unit.c(grid::unit(0.56, "null"), grid::unit(3, "mm"),
                         grid::unit(0.44, "null")),
    heights = heights
  )
  for (i in seq_along(c("A", "B"))) {
    column <- if (i == 1L) 1L else 3L
    tag <- grid::textGrob(c("A", "B")[i], x = grid::unit(2, "pt"),
                          just = "left", gp = grid::gpar(fontsize = 13, fontface = "bold"))
    figure <- gtable::gtable_add_grob(figure, tag, t = 1, l = column, clip = "off")
    figure <- gtable::gtable_add_grob(figure, if (i == 1L) love_grob else forest_grob,
                                     t = 2, l = column, clip = "off")
  }
  if (demo) {
    figure <- gtable::gtable_add_grob(
      figure, grid::textGrob("SYNTHETIC VALUES — layout preview only",
                             gp = grid::gpar(fontsize = 9, col = "#666666")),
      t = 3, l = 1, r = 3
    )
  }
  figure
}

build_combined_figures <- function(training_balance, metformin_balance,
                                   training_results_table, metformin_summary,
                                   demo = FALSE) {
  # ggplotGrob needs a device for font metrics. Avoid creating Rplots.pdf when
  # called from Rscript, and leave an existing interactive device untouched.
  if (grDevices::dev.cur() == 1L) {
    grDevices::pdf(file = NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
  }
  validate_combined_balance(training_balance, "Exercise balance data")
  validate_combined_balance(metformin_balance, "Metformin balance data")
  training_love <- draw_love_plot(training_balance, "Exercise.3to4.per.week_vs_Never", "combined")
  metformin_love <- draw_love_plot(metformin_balance, "Metformin", "combined")
  training_forest <- create_training_forest_plot(training_results_table, style = "combined")
  metformin_forest <- create_comparison_forest_plot(metformin_summary, style = "combined")
  for (plot in list(training_forest, metformin_forest)) {
    if (!nrow(plot$data) || anyNA(plot$data) ||
        any(!is.finite(as.matrix(plot$data[c("ATE", "Lower", "Upper")]))))
      stop("Saved forest results contain missing or non-finite plotting values.", call. = FALSE)
    if (any(plot$data$Lower > plot$data$Upper))
      stop("Saved forest results contain reversed confidence limits.", call. = FALSE)
  }
  list(
    training_combined = compose_love_forest(training_love, training_forest, demo),
    metformin_combined = compose_love_forest(metformin_love, metformin_forest, demo)
  )
}

save_combined_figures <- function(figures, output_dir, demo = FALSE) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  heights_cm <- c(training_combined = 13, metformin_combined = 15) + if (demo) 0.5 else 0
  paths <- character()
  for (name in names(figures)) {
    for (extension in c("png", "pdf")) {
      path <- file.path(output_dir, paste0(name, ".", extension))
      device <- if (extension == "pdf") grDevices::cairo_pdf else "png"
      ggplot2::ggsave(path, plot = figures[[name]], device = device,
                      width = 18, height = heights_cm[[name]], units = "cm",
                      dpi = 300, bg = "white", limitsize = TRUE)
      paths <- c(paths, path)
    }
  }
  message("Saved combined figures:\n", paste(paths, collapse = "\n"))
  invisible(paths)
}

render_combined_figures <- function(input_dir = "Output",
                                    output_dir = file.path(input_dir, "Combined_figures")) {
  files <- c(
    training_balance = "love_plots/exercise_effects/love_plot_Exercise_3to4_per_week_vs_Never.rds",
    metformin_balance = "love_plots/main/love_plot_Metformin.rds",
    training_results_table = "training_analysis_results.RData",
    metformin_summary = "metformin_summary"
  )
  steps <- c("Program/exercise_effects_step10.R", "Program/IPW_step7.R",
             "Program/exercise_effects_step10.R", "Program/Metformin_table_and_figures_step18.R")
  paths <- file.path(input_dir, files)
  missing <- !file.exists(paths)
  if (any(missing)) {
    stop("Missing saved plotting inputs:\n",
         paste(paste0("  ", paths[missing], "\n    Created by: ", steps[missing]), collapse = "\n"),
         "\nOlder runs saved Love plots only as PNGs. Rerun the indicated existing steps",
         " (steps 7 and 10 repeat analyses), or rerun Master_parallel.R, then retry.",
         "\nNo analyses were launched and no figures were written.", call. = FALSE)
  }
  load_table <- function(path, name) {
    env <- new.env(parent = emptyenv())
    loaded <- load(path, envir = env)
    if (!name %in% loaded || !is.data.frame(env[[name]]))
      stop(path, " must contain the saved table ", name, ".", call. = FALSE)
    env[[name]]
  }
  inputs <- list(
    training_balance = readRDS(paths[1]),
    metformin_balance = readRDS(paths[2]),
    training_results_table = load_table(paths[3], "training_results_table"),
    metformin_summary = load_table(paths[4], "metformin_summary")
  )
  figures <- do.call(build_combined_figures, inputs)
  save_combined_figures(figures, output_dir)
}
