#' Compare model fits with \eqn{BIC}
#'
#' @description This function compares model fits with the Bayesian Information
#'   Criterion (\eqn{BIC}). The model with the lowest \eqn{BIC} has the highest
#'   maximum likelihood (or roughly the fit of the model to the data),
#'   accounting for model complexity. A \eqn{BIC} difference between two models
#'   (\eqn{\Delta BIC}) is equal to the Bayes Factor. A \eqn{\Delta BIC > 2}
#'   represents at least a 100 times difference in the likelihoods.
#'
#' @param models A list of models to compare (e.g.,
#'   `models <- list(model1, model2, model3)`) where each model is an output
#'   from the `fit_punc_model` function; it is preferable to order the models
#'   in the list from the simplest to the most complex one
#' @param file Output file directory
#'
#' @return This function outputs a text file containing a \eqn{BIC} table.
#'
#' @author Kevin Surya
#'
#' @export
#'
#' @references \href{https://doi.org/10.1214/aos/1176344136}{Schwarz (1978)};
#'   \href{https://doi.org/10.1080/01621459.1995.10476572}{Kass and Raftery (1995)}
#'
compare_models <- function(models, file) {
  formula <- BIC <- delta_BIC <- NULL
  for (i in 1:length(models)) {
    formula[i] <- deparse(models[[i]]$model$call[[2]])
    BIC[i] <- summary(models[[i]]$model)$BIC
  }
  delta_BIC <- round(BIC - min(BIC), 2)
  bic_table <- data.frame(model = formula, BIC = BIC, delta_BIC = delta_BIC)
  sink(file)
  cat("=========\n")
  cat("BIC Table\n")
  cat("=========\n\n")
  print(bic_table)
  sink()
}
