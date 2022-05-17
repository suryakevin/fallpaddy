#' Plot regression diagnostics
#'
#' @description This function plots the distribution of the
#'   phylogenetically-normalized residuals to check if it meets the assumption
#'   of normality. Another panel in the plot is the residuals vs. fitted
#'   values. It is typically helpful for diagnosing model adequacy and the
#'   equal variance assumption. There should be little to no trend and no
#'   "funneling" pattern in the residuals vs. fitted plot.
#'
#' @param output_reg A list outputted from the `fit_punc_model` function
#'
#' @return This function returns an object of the `ggplot` class.
#'
#' @author Kevin Surya
#'
#' @import ggExtra ggplot2 ggthemes
#'
#' @export
#'
#' @references \href{https://doi.org/10.1111/j.1420-9101.2009.01757.x}{Freckleton (2009)};
#'   \href{https://doi.org/10.1007/978-3-662-43550-2_6}{Mundry (2014)}
#'
plot_diagnostics <- function(output_reg) {
  fitted <- output_reg$model$fitted
  phyres <- output_reg$phyres
  data <- data.frame(fitted = fitted, phyres = phyres)
  plot <-
    ggplot(data = data, aes(x = fitted, y = phyres)) +
      geom_point(color = "gray", size = 0.5) +
      geom_smooth(
        method = "loess",
        formula = "y ~ x",
        color = "red",
        se = FALSE
      ) +
      scale_y_reverse() +
      theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
      labs(x = "\nFitted values", y = "Phylogenetic residuals\n")
  plot <- ggMarginal(p = plot, type = "density", margins = "y", size = 1.75)
  return(plot)
}
