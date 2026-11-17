# Opt-in styling for training_combined and metformin_combined only.
# No global theme or display-name settings are changed.

combined_figure_theme <- function() {
  ggplot2::theme_minimal(base_size = 10, base_family = "sans") +
    ggplot2::theme(
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      axis.title = ggplot2::element_text(size = 10),
      axis.text = ggplot2::element_text(size = 9, colour = "#333333"),
      axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 3)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 5)),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E7E7E7", linewidth = 0.3),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 9, margin = ggplot2::margin(r = 5)),
      legend.key.width = grid::unit(3.5, "mm"),
      legend.key.height = grid::unit(3.5, "mm"),
      legend.spacing.x = grid::unit(2, "pt"),
      legend.margin = ggplot2::margin(t = 3),
      plot.margin = ggplot2::margin(2, 2, 2, 2)
    )
}

compact_combined_labels <- function(labels) {
  # Display-only replacements: the source labels, rows and order are retained.
  replacements <- c(
    "Hours of Sleep per Night: " = "Sleep: ",
    "Education Level: " = "Education: ",
    "Biological Sex: " = "Sex: ",
    "Tobacco Use: " = "Tobacco: ",
    "Alcohol per Week: " = "Alcohol: ",
    "Exercise per Week: " = "Exercise: ",
    "Technical or occupational certificate" = "Tech./Occup. Cert.",
    "Some college coursework completed" = "Some College",
    "Did not complete high school" = "Below High School",
    "High school or equivalent" = "High School/Equiv.",
    "Professional (MD, DO, DDS, JD)" = "Professional Degree",
    "Doctorate (PhD)" = "PhD",
    "Associate degree" = "Associate",
    "Bachelor’s degree" = "Bachelor’s",
    "Master’s degree" = "Master’s",
    "Bachelor's degree" = "Bachelor's",
    "Master's degree" = "Master's",
    "More than 8 hours" = ">8 h/night",
    "6 to 8 hours" = "6–8 h/night",
    "6 hours or less" = "≤6 h/night",
    "On special occasions" = "Special Occasions",
    "Once per week" = "1/week",
    "8 or more times per week" = "8+/week",
    " Times per Week" = "/week",
    " times per week" = "/week",
    " Per Week" = "/week",
    " per Week" = "/week",
    "Exercises " = "Exercise: ",
    " Years" = " y"
  )
  for (key in names(replacements)) {
    labels <- gsub(key, replacements[[key]], labels, fixed = TRUE)
  }
  labels
}

style_combined_love_plot <- function(plot) {
  # Preserve the original data, signed SMDs, reference lines and palette.
  plot$layers[[1]] <- ggplot2::geom_point(size = 2.1)
  plot +
    ggplot2::scale_y_discrete(labels = compact_combined_labels) +
    ggplot2::labs(x = "Standardized Mean Difference") +
    combined_figure_theme()
}

style_combined_forest_plot <- function(data, effect_label) {
  # Data and percentage conversion come from the existing forest constructors.
  # Draw thin intervals and solid points at their final physical size.
  # Use the shared result columns directly: standalone panels now have their
  # own horizontal mapping and fixed physical heights, which do not apply here.
  ggplot2::ggplot(data, ggplot2::aes(x = Label, y = ATE, ymin = Lower, ymax = Upper,
                                   colour = group, fill = group)) +
    ggplot2::geom_errorbar(width = 0.12, linewidth = 0.55,
                          position = ggplot2::position_dodge(width = 0.42)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.45) +
    ggplot2::geom_point(size = 2.3,
                       position = ggplot2::position_dodge(width = 0.42)) +
    ggplot2::scale_colour_manual(
      values = c("IPW" = "#de6b35", "G-computation" = "#008fd5"),
      breaks = c("IPW", "G-computation"),
      labels = c("IPW" = "IPW", "G-computation" = "G-Computation")
    ) +
    ggplot2::scale_x_discrete(labels = compact_combined_labels) +
    ggplot2::scale_y_continuous(name = effect_label,
                              expand = ggplot2::expansion(mult = c(0.06, 0.06))) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, colour = NULL) +
    ggplot2::guides(fill = "none") +
    combined_figure_theme()
}
