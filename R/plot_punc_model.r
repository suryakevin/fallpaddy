#' Plot data with or without a fit line
#'
#' @description This function plots data with or without a fit line derived
#'   from the `fit_punc_model` function.
#'
#' @param data A data frame with path length in the 1st column, node count in
#'   the 2nd, time in the 3rd (optional), and group assignment in the 4th
#'   (optional)
#' @param output_reg A list outputted from the `fit_punc_model` function
#' @param size Size of data point
#' @param alpha Transparency of data point (0: transparent; 1: opaque)
#' @param fit If `TRUE`, the plot shows phylogenetically-normalized fitted
#'   lines for `path ~ 1`, `path ~ node`, and `path ~ time` models; for more
#'   complex models, such as those that estimate a fit plane, it is easier to
#'   write a custom script
#' @param type List of options:
#'   \itemize{
#'     \item "extant_molecular": all taxa in the dataset are sampled at the
#'       "same" time (e.g., present-day mammal species), and the phylogenetic
#'       tree branch lengths reflect molecular evolution
#'     \item "extant_morphological": all taxa in the dataset are sampled at the
#'       "same" time (e.g., present-day mammal species), and the phylogenetic
#'       tree branch lengths reflect morphological evolution
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
#' @import ggplot2 ggthemes lubridate
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
  type = c("extant_molecular", "extant_morphological", "serial", "fossil")
) {
  if (ncol(data) == 2) {
    colnames(data)[1] <- "path"
    colnames(data)[2] <- "node"
  } else if (ncol(data) == 3) {
    colnames(data)[1] <- "path"
    colnames(data)[2] <- "node"
    colnames(data)[3] <- "time"
  } else {
    colnames(data)[1] <- "path"
    colnames(data)[2] <- "node"
    colnames(data)[3] <- "time"
    colnames(data)[4] <- "group"
    data$group <- as.factor(data$group)
  }
  model <- output_reg$model
  bw <- 2 * (IQR(x = data$path) / length(data$path)^(1/3))
    # Freedman-Diaconis rule
  if (fit == TRUE) {  # with a fit line
    if (model$dims$p == 1) {  # path ~ 1
      if (type == "extant_molecular" || type == "serial") {
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
              # scaled to trait evolution
      }
    } else if (model$dims$p == 2) {  # path ~ node OR path ~ time
      if (type == "extant_molecular") {
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
      } else if (type == "extant_morphological") {
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
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(x = "\nNode count", y = "Phylogenetic path length\n")
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
      if (type == "extant_molecular" || type == "serial") {
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
              # scaled to trait evolution
      }
    } else if (model$dims$p == 2) {  # path ~ node OR path ~ time
      if (type == "extant_molecular") {
        plot <-
          ggplot(data = data, aes(x = .data$node, y = .data$path)) +
            geom_point(size = size, alpha = alpha) +
            #> scale_x_continuous(breaks = c(0, ..., n) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nNode count",
              y = "Phylogenetic path length (subs/site)\n"
            )
      } else if (type == "extant_morphological") {
        plot <-
          ggplot(data = data, aes(x = .data$node, y = .data$path)) +
            geom_point(size = size, alpha = alpha) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(x = "\nNode count", y = "Phylogenetic path length\n")
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
    } else if (model$dims$p == 3 && names(model$coefficients[3] == "node")) {
        # path ~ time + node
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
              y = "Phylogenetic path length\n",
              color = "Node\ncount"
            )
      }
    } else if (model$dims$p == 4 && names(model)$coefficients[3] == "node") {
        # path ~ time * node
      if (type == "serial") {
        date_bounds <- seq(
          from = min(data$time),
          to = max(data$time),
          length.out = 7
        )  # six time intervals (oldest to youngest)
        for (i in 1:length(rownames(data))) {
          if (data$time[i] <= date_bounds[2]) {
            data$time_interval[i] <- "Interval 1"
          } else if (
              data$time[i] > date_bounds[2] && data$time[i] <= date_bounds[3]
            ) {
            data$time_interval[i] <- "Interval 2"
          } else if (
              data$time[i] > date_bounds[3] && data$time[i] <= date_bounds[4]
            ) {
            data$time_interval[i] <- "Interval 3"
          } else if (
              data$time[i] > date_bounds[4] && data$time[i] <= date_bounds[5]
            ) {
            data$time_interval[i] <- "Interval 4"
          } else if (
              data$time[i] > date_bounds[5] && data$time[i] <= date_bounds[6]
            ) {
            data$time_interval[i] <- "Interval 5"
          } else if (
              data$time[i] > date_bounds[6] && data$time[i] <= date_bounds[7]
            ) {
            data$time_interval[i] <- "Interval 6"
          }
        }
        data$time_interval <- as.factor(data$time_interval)
        plot <-
          ggplot(data = data, aes(x = .data$node, y = .data$path)) +
            facet_wrap(vars(.data$time_interval)) +
            geom_point(size = size, alpha = alpha) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nNode count",
              y = "Phylogenetic path length (subs/site)\n"
            )
      } else if (type == "fossil") {
        date_bounds <- seq(
          from = max(data$time),
          to = min(data$time),
          length.out = 7
        )  # six time intervals (oldest to youngest)
        for (i in 1:length(rownames(data))) {
          if (data$time[i] >= date_bounds[2]) {
            data$time_interval[i] <- "Interval 1"
          } else if (
              data$time[i] < date_bounds[2] && data$time[i] >= date_bounds[3]
            ) {
            data$time_interval[i] <- "Interval 2"
          } else if (
              data$time[i] < date_bounds[3] && data$time[i] >= date_bounds[4]
            ) {
            data$time_interval[i] <- "Interval 3"
          } else if (
              data$time[i] < date_bounds[4] && data$time[i] >= date_bounds[5]
            ) {
            data$time_interval[i] <- "Interval 4"
          } else if (
              data$time[i] < date_bounds[5] && data$time[i] >= date_bounds[6]
            ) {
            data$time_interval[i] <- "Interval 5"
          } else if (
              data$time[i] < date_bounds[6] && data$time[i] >= date_bounds[7]
            ) {
            data$time_interval[i] <- "Interval 6"
          }
        }
        data$time_interval <- as.factor(data$time_interval)
        plot <-
         ggplot(data = data, aes(x = .data$node, y = .data$path)) +
           facet_wrap(vars(.data$time_interval)) +
           geom_point(size = size, alpha = alpha) +
           theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
           labs(
             x = "\nNode count",
             y = "Phylogenetic path length\n"
           )
      }
    } else if (model$dims$p >= 3 && names(model$coefficients[2] == "node")) {
        # path ~ node + group OR path ~ node * group
      if (type == "extant_molecular") {
        plot <-
         ggplot(
           data = data,
           aes(x = .data$node, y = .data$path, color = .data$group)
         ) +
           facet_wrap(vars(.data$group)) +
           geom_point(size = size, alpha = alpha) +
           theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
           theme(legend.position = "none") +
           labs(
             x = "\nNode count",
             y = "Phylogenetic path length (subs/site)\n"
           )
      } else if (type == "extant_morphological") {
        plot <-
         ggplot(
           data = data,
           aes(x = .data$node, y = .data$path, color = .data$group)
         ) +
           facet_wrap(vars(.data$group)) +
           geom_point(size = size, alpha = alpha) +
           theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
           theme(legend.position = "none") +
           labs(
             x = "\nNode count",
             y = "Phylogenetic path length\n"
           )
      }
    } else {
        # path ~ time + node + group OR path ~ time + node * group
      if (type == "serial") {
        plot <-
          ggplot(
            data = data,
            aes(x = .data$time, y = .data$path, color = .data$node)
          ) +
            facet_wrap(vars(.data$group)) +
            geom_point(size = size) +
            scale_colour_gradient(low = "gray75", high = "gray0") +
            guides(colour = guide_colourbar(barwidth = 0.25, ticks = FALSE)) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nSampling time (decimal years)",
              y = "Phylogenetic path length (subs/site)\n",
              color = "Node\ncount"
            )
      } else {
        plot <-
          ggplot(
            data = data,
            aes(x = .data$time, y = .data$path, color = .data$node)
          ) +
            facet_wrap(vars(.data$group)) +
            geom_point(size = size) +
            scale_colour_gradient(low = "gray75", high = "gray0") +
            guides(colour = guide_colourbar(barwidth = 0.25, ticks = FALSE)) +
            theme_tufte(base_size = 10, base_family = "Arial", ticks = FALSE) +
            labs(
              x = "\nSampling time (decimal years)",
              y = "Phylogenetic path length\n",
              color = "Node\ncount"
            )
      }
    }
  }
  return(plot)
}
