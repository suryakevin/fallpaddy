#' Detect punctuated evolution
#'
#' @description This function fits the regression model(s) for detecting the
#'   punctuational effect at branching events.
#'
#' @details The goal is to estimate the expected difference in net evolution,
#'   morphological or molecular, between two taxa where one of them has
#'   undergone one additional branching event. A taxon's net divergence is
#'   proxied by its phylogenetic path length or root-to-tip distance. And the
#'   cumulative number of branching events along the path is the node count.
#'
#' This expected difference should account for the strict clock, morphological
#'   or molecular. When all taxa in the tree are co-occurring (e.g., all
#'   present-day mammal species), the expected path length under the strict
#'   clock is the mean (`path ~ 1`). Note that the average should be
#'   phylogenetically-normalized. This mean-only model is the simplest.
#'
#' Punctuated evolution means that more change accumulates during branching
#'   events. As a result, the evolutionary rate is not clock-like. So, we
#'   hypothesize that node count predicts deviation from the clock
#'   (`path ~ node`).
#'
#' However, the strict clock has a different expectation when the taxa in the
#'   tree are not co-occurring. For example, the taxa are sampled serially like
#'   SARS-CoV-2 genomes during the COVID-19 pandemic or sporadically like the
#'   deposition of dinosaur fossils in ancient sediments. In these cases, the
#'   strict clock expects the amount of divergence to scale proportionally with
#'   time (`path ~ sampling time`).
#'
#' In such cases, the detection of punctuated evolution becomes the regression
#'   of path length on sampling time and node count (`path ~ time + node`).
#'
#' We can fit more complex models where we allow the degree of punctuational
#'   effect to vary with time (e.g., `path ~ time + node + time * node`) or
#'   space (e.g., `path ~ time + node * continent`). However, it is best to
#'   create your custom function or write a custom script at this point.
#'
#' @param data A data frame with path length in the 1st column, node count in
#'   the 2nd, time in the 3rd (optional), and taxon names as the row names;
#'   make sure that the row order matches the tree tip labels (use the function
#'   `reorder_data`)
#' @param vcv_parts A list outputted from the `decomp_vcv` function
#' @param D A normalizing matrix D outputted from the `create_dmat` function
#' @param model Model options:
#' \itemize{
#'   \item "p": path ~ 1
#'   \item "pn": path ~ node
#'   \item "pt": path ~ time
#'   \item "ptn": path ~ time + node
#' }
#'
#' @return This function returns a list containing the fitted model (an object
#'   of class `gls`), estimated variance, phylogenetically-normalized
#'   residuals, and sum of squared errors (SSE).
#'
#' @author Kevin Surya
#'
#' @import ape nlme
#'
#' @export
#'
#' @references \href{https://doi.org/10.1126/science.1083202}{Webster et al. (2003)};
#'   \href{https://doi.org/10.1126/science.1129647}{Pagel et al. (2006)}
#'
fit_punc_model <- function(
  data,
  vcv_parts,
  D,
  model = c("p", "pn", "pt", "ptn")
) {
  # prepares data
  colnames(data)[1] <- "path"
  colnames(data)[2] <- "node"
  colnames(data)[3] <- "time"
  data$taxon <- rownames(data)
  corr <- vcv_parts$corr
  data$w <- vcv_parts$w
  # fits model
  if (model == "p") {  # path ~ 1
    model <- gls(
      model = path ~ 1,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  } else if (model == "pn") {  # path ~ node
    model <- gls(
      model = path ~ node,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  } else if (model == "pt") {  # path ~ time
    model <- gls(
      model = path ~ time,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  } else {  # path ~ time + node
    model <- gls(
      model = path ~ time + node,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  }
  # organizes outputs
  model_summ <- summary(model)
  sig2 <- model_summ$sigma^2
  phyres <- D %*% model$residuals
  sse <- sum(phyres^2)
  output <- list(model, sig2, phyres, sse)
  names(output) <- c("model", "sig2", "phyres", "sse")
  return(output)
}

#' Print the output of a regression model
#'
#' @description This function saves the summary of a regression model to a text
#'   file.
#'
#' @param output_reg A list outputted from the `fit_punc_model` function
#' @param file Output file directory
#'
#' @return This function outputs a text file containing the summary of the
#'   regression model.
#'
#' @author Kevin Surya
#'
#' @export
#'
print_punc_model <- function(output_reg, file) {
  model <- output_reg$model
  model_summ <- summary(model)
  sig2 <- output_reg$sig2
  sse <- output_reg$sse
  sink(file)
  cat("==========\n")
  cat("Regression\n")
  cat("==========\n\n")
  print(model_summ)
  cat("\n")
  print(model_summ$tTable)
  cat("\n")
  cat(paste("Variance = ", sig2, "\n", sep = ""))
  cat(paste("SSE = ", sse, sep = ""))
  cat("\n")
  sink()
}
