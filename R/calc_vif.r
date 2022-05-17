#' Calculate the variance inflation factors (\eqn{VIFs})
#'
#' @description This function prints the variance inflation factors
#'   (\eqn{VIFs}) calculated using the `vif` function in the `R` package `car`
#'   to a text file. A \eqn{VIF > 10} indicates significant multicollinearity.
#'   It means that the standard error for the beta or coefficient of the
#'   predictor in question is more than ten times higher than if the predictor
#'   is independent of the other predictors.
#'
#' @param output_reg A list outputted from the `fit_punc_model` function
#' @param file Output file directory
#'
#' @return This function outputs a text file containing the VIF for each
#'   predictor in the model.
#'
#' @author Kevin Surya
#'
#' @import car
#'
#' @export
#'
#' @references \href{https://link-springer-com/book/10.1007/978-1-4614-1353-0}{Vittinghoff et al. (2011)}
#'   \href{https://socialsciences.mcmaster.ca/jfox/Books/Companion/}{Fox and Weisberg (2019)}
#'
calc_vif <- function(output_reg, file) {
  sink(file)
  cat("==========================\n")
  cat("Variance Inflation Factors\n")
  cat("==========================\n\n")
  print(vif(mod = output_reg$model))
  sink()
}
