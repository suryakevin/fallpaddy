#' Create a 2D interactive plot
#'
#' @description This function creates an interactive plot of the data.
#'
#' @param data A data frame path length in the 1st column, node count in the
#'   2nd, group assignment in the 3rd (optional), and taxon names as the row
#'   names
#' @param unit Branch length unit (e.g., subs/site and mya)
#' @param group If `TRUE`, the colors of the data points represent the group
#'   assignment
#' @param pt_size Size of data point
#' @param alpha Transparency of data point (0: transparent; 1: opaque)
#' @param plot_width Width of the plot in pixels
#' @param plot_height Height of the plot in pixels
#' @param text_size Size of axis/color label
#' @param tick_size Size of axis ticks
#'
#' @return This function returns an object of the class `plotly`.
#'
#' @author Kevin Surya
#'
#' @importFrom magrittr %>%
#' @importFrom plotly layout plot_ly
#'
#' @export
#'
plot_data_2d <- function(
  data,
  unit,
  group = FALSE,
  pt_size = 1.5,
  alpha = 1,
  plot_width = 912,
  plot_height = 564.48,
  text_size = 20,
  tick_size = 14
) {
  if (group == FALSE) {
    colnames(data) <- c("path", "node")
    data$taxon <- rownames(data)
    plot <- plot_ly(
      data = data,
      x = ~node,
      y = ~path,
      text = ~taxon,
      type = "scatter",
      mode = "markers",
      size = pt_size,
      alpha = alpha,
      width = plot_width,
      height = plot_height
    ) %>%
    layout(
      xaxis = list(
        title = "Node count",
        titlefont = list(size = text_size),
        tickfont = list(size = tick_size)
      ),
      yaxis = list(
        title = paste0("Phylogenetic path length (", unit, ")"),
        titlefont = list(size = text_size),
        tickfont = list(size = tick_size)
      )
    )
  } else {
    colnames(data) <- c("path", "node", "group")
    data$taxon <- rownames(data)
    data$group <- as.factor(data$group)
    plot <- plot_ly(
      data = data,
      x = ~node,
      y = ~path,
      color = ~group,
      text = ~taxon,
      type = "scatter",
      mode = "markers",
      size = pt_size,
      alpha = alpha,
      width = plot_width,
      height = plot_height
    ) %>%
    layout(
      xaxis = list(
        title = "Node count",
        titlefont = list(size = text_size),
        tickfont = list(size = tick_size)
      ),
      yaxis = list(
        title = paste0("Phylogenetic path length (", unit, ")"),
        titlefont = list(size = text_size),
        tickfont = list(size = tick_size)
      ),
      legend = list(
        title = list(text = "<b>Group</b>"),
        titlefont = list(size = text_size),
        tickfont = list(size = tick_size)
      )
    )
  }
  return(plot)
}

#' Create a 3D interactive plot
#'
#' @description This function creates a 3D interactive plot of the data.
#'
#' @param data A data frame with path length in the 1st column, node count in
#'   the 2nd, time in the 3rd, group assignment in the 4th (optional), and
#'   taxon names as the row names
#' @param unit Branch length unit (e.g., subs/site and mya)
#' @param group If `TRUE`, the colors of the data points represent the group
#'   assignment instead of the node count
#' @param pt_size Size of data point
#' @param plot_width Width of the plot in pixels
#' @param plot_height Height of the plot in pixels
#'
#' @return This function returns an object of the class `plotly` class.
#'
#' @author Kevin Surya
#'
#' @importFrom magrittr %>%
#' @importFrom plotly add_markers layout plot_ly
#'
#' @export
#'
plot_data_3d <- function(
  data,
  unit,
  group = FALSE,
  pt_size = 1.25,
  plot_width = 912,
  plot_height = 564.48
) {
  if (group == FALSE) {
    colnames(data) <- c("path", "node", "time")
    data$taxon <- rownames(data)
    plot <- plot_ly(
      data = data,
      x = ~time,
      y = ~node,
      z = ~path,
      text = ~taxon,
      color = ~node,
      size = pt_size,
      width = plot_width,
      height = plot_height
    )
  } else {
    colnames(data) <- c("path", "node", "time", "group")
    data$taxon <- rownames(data)
    plot <- plot_ly(
      data = data,
      x = ~time,
      y = ~node,
      z = ~path,
      text = ~taxon,
      color = ~group,
      size = pt_size,
      width = plot_width,
      height = plot_height
    )
  }
  plot <- plot %>%
    add_markers()
  plot <- plot %>%
    layout(
      scene = list(
        xaxis = list(title = "Time"),
        yaxis = list(title = "Node count"),
        zaxis = list(title = paste0("Phylogenetic path length (", unit, ")"))
      )
    )
  return(plot)
}
