#' Export plot to an HTML file
#'
#' @description This function exports an interactive plot (2D or 3D) to an HTML
#'   file.
#'
#' @param plot An object of the `plotly` class
#' @param file Output file directory (save as an .html file)
#' @param plot_3d Exports a 3D plot if `TRUE`
#'
#' @return This function outputs an object of the `plotly` class to an HTML
#'   file.
#'
#' @author Kevin Surya
#'
#' @import htmltools htmlwidgets
#'
#' @importFrom plotly as_widget
#'
#' @export
#'
export_plot_html <- function(plot, file, plot_3d = FALSE) {
  if (plot_3d == FALSE) {
    plot <- div(plot, align = "center")
    save_html(html = plot, file = file)
  } else {
    saveWidget(widget = as_widget(plot), file = file)
  }
}

#' Export plot to a PDF file
#'
#' @description This function exports a plot to a PDF file. The default plot
#'   dimension (width/height) follows the golden ratio, 1.618.
#'
#' @param plot An object of the `ggplot` class
#' @param file Output file directory
#' @param width Plot width (inches)
#' @param height Plot height (inches)
#'
#' @return This function outputs an object of the `ggplot` class to a PDF file.
#'
#' @author Kevin Surya
#'
#' @import Cairo
#'
#' @importFrom grDevices graphics.off
#'
#' @export
#'
#' @references \href{https://cran.r-project.org/web/packages/Cairo/index.html}{Urbanek and Horner (2022)}
#'
export_plot_pdf <- function(plot, file, width = 4.75, height = 2.94) {
  CairoPDF(width = width, height = height, file = file)
  print(plot)
  graphics.off()
}

#' Export plot to a SVG file
#'
#' @description This function exports a plot to a SVG file. The default plot
#'   dimension (width/height) follows the golden ratio, 1.618.
#'
#' @param plot An object of the `ggplot` class
#' @param file Output file directory
#' @param width Plot width (inches)
#' @param height Plot height (inches)
#'
#' @return This function outputs an object of the `ggplot` class to a SVG file.
#'
#' @author Kevin Surya
#'
#' @import Cairo svglite
#'
#' @importFrom grDevices graphics.off
#'
#' @export
#'
#' @references \href{https://cran.r-project.org/web/packages/Cairo/index.html}{Urbanek and Horner (2022)}
#'
export_plot_svg <- function(plot, file, width = 4.75, height = 2.94) {
  CairoSVG(width = width, height = height, file = file)
  print(plot)
  graphics.off()
}
