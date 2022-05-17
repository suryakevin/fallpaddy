#' Import dataset
#'
#' @description This function imports a tab-delimited data frame with headers
#'   and row names.
#'
#' @param file Input file directory
#'
#' @return This function returns an object of class `data.frame`.
#'
#' @author Kevin Surya
#'
#' @importFrom utils read.table
#'
#' @export
#'
import_data <- function(file) {
  data <- read.table(file = file, header = TRUE, sep = "\t", row.names = 1)
  return(data)
}

#' Import dataset (BayesTraits style)
#'
#' @description This function imports a tab-delimited data frame without
#'   headers (BayesTraits style).
#'
#' @param file Input file directory
#'
#' @return This function returns an object of class `data.frame`.
#'
#' @author Kevin Surya
#'
#' @importFrom utils read.table
#'
#' @export
#'
import_data_bt <- function(file) {
  data <- read.table(file = file, header = FALSE, sep = "\t", row.names = 1)
  return(data)
}

#' Import a .csv file
#'
#' @description This function is an alias of `read.csv`.
#'
#' @param file Input file directory
#'
#' @return This function returns an object of class `data.frame`.
#'
#' @author Kevin Surya
#'
#' @importFrom utils read.csv
#'
#' @export
#'
import_data_csv <- function(file) {
  data <- read.csv(file = file)
  return(data)
}
