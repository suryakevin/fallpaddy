#' Calculate partial \ifelse{html}{\out{<i>R<sup>2</sup></i>}}{\eqn{R^2}}
#'
#' @description This function calculates the partial
#'   \ifelse{html}{\out{<i>R<sup>2</sup></i>}}{\eqn{R^2}} of a phylogenetic
#'   regression model. In the context of models for detecting punctuated
#'   evolution, this value represents the proportion of the deviation from the
#'   strict molecular (or morphological) clock that is attributable to net
#'   speciation or gene duplication events. Specifically, the partial
#'   \ifelse{html}{\out{<i>R<sup>2</sup></i>}}{\eqn{R^2}} roughly represents
#'   how much the sum of squared errors (SSE) decreases after including node
#'   count in the model. So, the more complex model has node count as the only
#'   additional parameter (e.g., `path ~ node` or `path ~ time + node`)
#'   compared to the simpler model (e.g., `path ~ 1` or `path ~ time`).
#'
#' @param output_simple A list outputted from the `fit_punc_model` function
#'   (the simpler model)
#' @param output_complex A list outputted from the `fit_punc_model` function
#'   (the more complex model)
#'
#' @return This function returns the partial
#'   \ifelse{html}{\out{<i>R<sup>2</sup></i>}}{\eqn{R^2}} value.
#'
#' @author Kevin Surya
#'
#' @export
#'
#' @references \href{https://doi.org/10.1080/00031305.1994.10476036}{Anderson-Sprecher (1994)}
#'
calc_partialr2 <- function(output_simple, output_complex) {
  partial_r2 <- (output_simple$sse - output_complex$sse) / output_simple$sse
  return(partial_r2)
}
