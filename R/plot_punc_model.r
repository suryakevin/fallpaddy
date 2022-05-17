#' Plot data with or without a fit line
#'
#' @description This function plots data with or without a fit line derived
#'   from the `fit_punc_model` function.
#'
#' @param data A data frame with path length in the 1st column, node count in
#'   the 2nd, and time in the 3rd
#' @param output_reg A list outputted from the `fit_punc_model` function
#' @param size Size of data point
#' @param alpha Transparency of data point (0: transparent; 1: opaque)
#' @param fit If `TRUE`, the plot shows the phylogenetically-normalized fitted
#'   line but not the fitted plane derived from a multiple regression model
#'   (`path ~ time + node`)
#' @param type List of options:
#'   \itemize{
#'     \item "extant": all taxa in the dataset are sampled at the "same" time
#'       (e.g., present-day mammal species)
#'     \item "serial": taxa are serially-sampled, and the timescale is in days
#'       to years (e.g., SARS-CoV-2 genomes)
#'     \item "fossil": taxa are serially-sampled, and the timescale is in
#'       hundreds to millions of years (e.g., extinct dinosaur species)
#'   }
#'
#' @return This function returns an object of the `ggplot` class.
#'
#' @author Kevin Surya
#'
#' @import ggplot2 ggthemes
#'
#' @importFrom rlang .data
#' @importFrom stats IQR
#'
#' @export
#'
#' @references \href{https://doi.org/10.1007/BF01025868}{Freedman and Diaconis (1981)};
#'   \href{https://doi.org/10.1126/science.1083202}{Webster et al. (2003)};
#'   \href{https://doi.org/10.1126/science.1129647}{Pagel et al. (2006)};
#'   \href{https://doi.org/10.1093/ve/vew007}{Rambaut et al. (2016)}
#'
plot_punc_model <- function(
  data,
  output_reg,
  size = 1,
  alpha = 1,
  fit = TRUE,
  type = c("extant", "serial", "fossil")
) {
  colnames(data)[1] <- "path"
  colnames(data)[2] <- "node"
  colnames(data)[3] <- "time"
  model <- output_reg$model
  bw <- 2 * (IQR(x = data$path) / length(data$path)^(1/3))
  # Freedman-Diaconis rule
  if (fit == TRUE) {  # with a fit line
    if (model$dims$p == 1) {  # path ~ 1
      if (type == "extant" || type == "serial") {
        plot <-
          ggplot(data = data, aes(x = .data$path)) +
            geom_histogram(color = "white", fill = "gray", binwidth = bw) +
            geom_vline(
              xintercept = as.numeric(model$coefficients[1]),
              color = "gray",
              lty = "dashed",
              lwd = 0.5
            ) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nPhylogenetic path length (subs/site)",
              y = "Frequency\n"
            )
      } else {
         plot <-
          ggplot(data = data, aes(x = .data$path)) +
            geom_histogram(color = "white", fill = "gray", binwidth = bw) +
            geom_vline(
              xintercept = as.numeric(model$coefficients[1]),
              color = "gray",
              lty = "dashed",
              lwd = 0.5
            ) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(x = "\nPhylogenetic path length", y = "Frequency\n")
      }
    } else if (model$dims$p == 2) {  # path ~ node OR path ~ time
      if (type == "extant") {
        plot <-
          ggplot(data = data, aes(x = .data$node, y = .data$path)) +
            geom_point(size = size, alpha = alpha) +
            stat_function(
              color = "dark gray",
              size = 0.5,
              fun = function(node) {
                as.numeric(model$coefficients[1]) +
                as.numeric(model$coefficients[2]) * node
              }
            ) +
            #> scale_x_continuous(breaks = c(0, ..., n) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nNode count",
              y = "Phylogenetic path length (subs/site)\n"
            )
      } else if (type == "serial") {
        plot <-
          ggplot(data = data, aes(x = .data$time, y = .data$path)) +
            geom_point(size = size, alpha = alpha) +
            stat_function(
              color = "dark gray",
              size = 0.5,
              fun = function(time) {
                as.numeric(model$coefficients[1]) +
                as.numeric(model$coefficients[2]) * time
              }
            ) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nSampling time (decimal years)",
              y = "Phylogenetic path length (subs/site)\n"
            )
      } else {
        plot <-
          ggplot(data = data, aes(x = .data$time, y = .data$path)) +
            geom_point(size = size, alpha = alpha) +
            stat_function(
              color = "dark gray",
              size = 0.5,
              fun = function(time) {
                as.numeric(model$coefficients[1]) +
                as.numeric(model$coefficients[2]) * time
              }
            ) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nMillion years ago",
              y = "Phylogenetic path length (subs/site)\n"
            )
      }
    }
  } else {  # without a fit line
    if (model$dims$p == 1) {  # path ~ 1
      if (type == "extant" || type == "serial") {
        plot <-
          ggplot(data = data, aes(x = .data$path)) +
            geom_histogram(color = "white", fill = "gray", binwidth = bw) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nPhylogenetic path length (subs/site)",
              y = "Frequency\n"
            )
      } else {
         plot <-
          ggplot(data = data, aes(x = .data$path)) +
            geom_histogram(color = "white", fill = "gray", binwidth = bw) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(x = "\nPhylogenetic path length", y = "Frequency\n")
      }
    } else if (model$dims$p == 2) {  # path ~ node OR path ~ time
      if (type == "extant") {
        plot <-
          ggplot(data = data, aes(x = .data$node, y = .data$path)) +
            geom_point(size = size, alpha = alpha) +
            #> scale_x_continuous(breaks = c(0, ..., n) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nNode count",
              y = "Phylogenetic path length (subs/site)\n"
            )
      } else if (type == "serial") {
        plot <-
          ggplot(data = data, aes(x = .data$time, y = .data$path)) +
            geom_point(size = size, alpha = alpha) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nSampling time (decimal years)",
              y = "Phylogenetic path length (subs/site)\n"
            )
      } else {
        plot <-
          ggplot(data = data, aes(x = .data$time, y = .data$path)) +
            geom_point(size = size, alpha = alpha) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nMillion years ago",
              y = "Phylogenetic path length (subs/site)\n"
            )
      }
    } else {  # path ~ time + node
      if (type == "serial") {
        plot <-
          ggplot(
            data = data,
            aes(x = .data$time, y = .data$path, color = .data$node)
          ) +
            geom_point(size = size) +
            scale_colour_gradient(low = "gray75", high = "gray0") +
            guides(colour = guide_colourbar(barwidth = 0.25, ticks = FALSE)) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nSampling time (decimal years)",
              y = "Phylogenetic path length (subs/site)\n",
              color = "Node\ncount"
            )
      } else if (type == "fossil") {
        plot <-
          ggplot(
            data = data,
            aes(x = .data$time, y = .data$path, color = .data$node)
          ) +
            geom_point(size = size) +
            scale_colour_gradient(low = "gray75", high = "gray0") +
            guides(colour = guide_colourbar(barwidth = 0.25, ticks = FALSE)) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nMillion years ago",
              y = "Phylogenetic path length\n",  # scaled to trait evolution
              color = "Node\ncount"
            )
      }
    }
  }
  return(plot)
}
