# Drawing only: accepts saved aggregate balance values, never fits a model.
library(ggplot2)
source(here::here("Program", "Functions", "display_names.R"))
source(here::here("Program", "Functions", "combined_plot_style.R"))

#' Draw a Love plot from covariate, status and smd columns.
#' The default preserves the original standalone appearance.
draw_love_plot <- function(smd_long, treatment_name,
                           style = c("standalone", "combined")) {
  style <- match.arg(style)
  # Format treatment name for display
  treatment_display_name <- format_display_name(treatment_name)
  
  # Retain title metadata for backward compatibility; the theme suppresses its display
  title_text <- paste0("Covariate Balance: ", treatment_display_name)
  title_text <- paste(strwrap(title_text, width = 50), collapse = "\n")
  
  subtitle_text <- "Standardized mean differences before and after propensity score weighting"
  
  # Create plot
  p <- ggplot(smd_long, aes(x = smd, y = covariate, color = status)) +
    geom_point(size = 3) +
    geom_vline(xintercept = 0.1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = -0.1, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "solid", color = "gray50", alpha = 0.5) +
    scale_color_discrete(
      labels = c("smd_after" = "Weighted", "smd_before" = "Unweighted")
    ) +
    labs(
      title = title_text,
      subtitle = subtitle_text,
      y = "",
      x = "Standardized Mean Difference",
      color = ""
    ) +
    theme_minimal() +
    theme(
      plot.title.position = "plot",
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      legend.text = element_text(size = 12)
    )
  
  if (style == "combined") p <- style_combined_love_plot(p)
  else p <- p + scale_y_discrete(labels = format_figure_category_labels)
  return(p)
}
