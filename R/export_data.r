#' Export dataset to a text file
#'
#' @description This function exports a data frame to a tab-delimited text file.
#'
#' @param data A data frame
#' @param file Output file directory
#'
#' @return This function outputs a tab-delimited text file containing the
#'   dataset.
#'
#' @author Kevin Surya
#'
#' @importFrom utils write.table
#'
#' @export
#'
export_data <- function(data, file) {
  write.table(
    x = data,
    file = file,
    quote = FALSE,
    sep = "\t",
    row.names = TRUE,
    col.names = TRUE
  )
}

#' Export dataset to a text file (BayesTraits style)
#'
#' @description This function exports a data frame to a tab-delimited text
#'   file (BayesTraits style).
#'
#' @param data A data frame
#' @param file Output file directory
#'
#' @return This function outputs a tab-delimited text file containing the
#'   dataset.
#'
#' @author Kevin Surya
#'
#' @importFrom utils write.table
#'
#' @export
#'
export_data_bt <- function(data, file) {
  write.table(
    x = data,
    file = file,
    quote = FALSE,
    sep = "\t",
    row.names = TRUE,
    col.names = FALSE
  )
}
