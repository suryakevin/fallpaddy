#' Calculate \ifelse{html}{\out{<i>R<sup>2</sup></i>}}{\eqn{R^2}}
#'
#' @description This function calculates the
#'   \ifelse{html}{\out{<i>R<sup>2</sup></i>}}{\eqn{R^2}} of a phylogenetic
#'   regression model. This value represents how much the predictors in the
#'   model explain (or predict) the variation in the response variable.
#'
#' @param output_null A list outputted from the `fit_punc_model` function (the
#'   null model, typically a phylogenetic mean-only model)
#' @param output_alt A list outputted from the `fit_punc_model` function (the
#'   alternative model)
#'
#' @return This function returns the
#'   \ifelse{html}{\out{<i>R<sup>2</sup></i>}}{\eqn{R^2}} value.
#'
#' @author Kevin Surya
#'
#' @export
#'
#' @references
#'   \href{https://handle.nal.usda.gov/10113/IND43966364}{Wright (1921)}
#'
calc_r2 <- function(output_null, output_alt) {
  r2 <- 1 - (output_alt$sse / output_null$sse)
  return(r2)
}
