#' Plot the node-density test
#'
#' @description This function plots the node-density test, testing for a
#'   curvilinear relationship between the node count and path length.
#'
#' @param data A data frame with path length in the 1st column and node count
#'   in the 2nd
#' @param output_nd A list outputted from the `est_node_density` function
#' @param unit Branch length unit (e.g., subs/site and mya)
#'
#' @return This function returns an object of the `ggplot` class.
#'
#' @author Kevin Surya
#'
#' @import ggExtra ggplot2 ggthemes
#'
#' @importFrom rlang .data
#'
#' @export
#'
#' @references \href{https://doi.org/10.1126/science.1083202}{Webster et al. (2003)};
#'   \href{https://doi.org/10.1080/10635150600865567}{Venditti et al. (2006)}
#'
plot_node_density <- function(data, output_nd, unit) {
  colnames(data) <- c("path", "node")
  beta <- output_nd$beta
  delta <- output_nd$delta
  plot <-
    ggplot(data = data, aes(x = .data$path, y = .data$node)) +
      geom_point(color = "gray", size = 0.5) +
      stat_function(
        color = "red",
        size = 0.5,
        fun = function(path) {beta * path^delta}
      ) +
      theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
      labs(
        x = paste0("\nPhylogenetic path length (", unit, ")"),
        y = "Node count\n"
      )
  return(plot)
}
