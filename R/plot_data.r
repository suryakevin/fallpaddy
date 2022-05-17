#' Create a 2D interactive plot
#'
#' @description This function creates an interactive plot of the data.
#'
#' @param data A data frame with taxon name in the row name, path length in the
#'   1st column, node count in the 2nd, and group assignment in the 3rd
#'   (optional)
#' @param group If `TRUE`, the colors of the data points represent the group
#'   assignment
#' @param size Size of data point
#' @param alpha Transparency of data point (0: transparent; 1: opaque)
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
plot_data_2d <- function(data, group = FALSE, size = 1.5, alpha = 1) {
  colnames(data) <- c("path", "node", "group")
  data$taxon <- rownames(data)
  if (group == FALSE) {
    plot <- plot_ly(
      data = data,
      x = ~node,
      y = ~path,
      text = ~taxon,
      type = "scatter",
      mode = "markers",
      size = size,
      alpha = alpha,
      width = 912,  # pixels
      height = 564.48  # pixels
    ) %>%
    layout(
      xaxis = list(
        title = "Node count",
        titlefont = list(size = 20),
        tickfont = list(size = 14)
      ),
      yaxis = list(
        title = "Phylogenetic path length (subs/site)",
        titlefont = list(size = 20),
        tickfont = list(size = 14)
      )
    )
  } else {
    data$group <- as.factor(data$group)
    plot <- plot_ly(
      data = data,
      x = ~node,
      y = ~path,
      color = ~group,
      text = ~taxon,
      type = "scatter",
      mode = "markers",
      size = size,
      alpha = alpha,
      width = 912,  # pixels
      height = 564.48  # pixels
    ) %>%
    layout(
      xaxis = list(
        title = "Node count",
        titlefont = list(size = 20),
        tickfont = list(size = 14)
      ),
      yaxis = list(
        title = "Phylogenetic path length (subs/site)",
        titlefont = list(size = 20),
        tickfont = list(size = 14)
      ),
      legend = list(
        title = list(text = "<b>Group</b>"),
        titlefont = list(size = 20),
        tickfont = list(size = 14)
      )
    )
  }
  return(plot)
}

#' Create a 3D interactive plot
#'
#' @description This function creates a 3D interactive plot of the data.
#'
#' @param data A data frame with taxon name in the row name, path length in the
#'   1st column, node count in the 2nd, time in the 3rd, and group assignment
#'   in the 4th (optional)
#' @param group If `TRUE`, the colors of the data points represent the group
#'   assignment instead of the node count
#' @param size Size of data point
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
plot_data_3d <- function(data, group = FALSE, size = 1.25) {
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
      size = size
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
      size = size
    )
  }
  plot <- plot %>%
    add_markers()
  plot <- plot %>%
    layout(
      scene = list(
        xaxis = list(title = "Time"),
        yaxis = list(title = "Node count"),
        zaxis = list(title = "Phylogenetic path length (subs/site)")
      )
    )
  return(plot)
}
