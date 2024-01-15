#' Detect punctuated evolution
#'
#' @description This function fits the regression model(s) for detecting the
#'   punctuational effect at branching events.
#'
#' @details The goal is to estimate the expected difference in net evolution,
#'   morphological or molecular, between two taxa where one of them has
#'   undergone one additional branching event. A taxon's net divergence is
#'   proxied by its phylogenetic path length or root-to-tip distance. And the
#'   cumulative number of branching events along a taxon's root-to-tip path is
#'   its node count.
#'
#' This expected difference should account for the strict clock (morphological
#'   or molecular). When all taxa in the tree are co-occurring (e.g., all
#'   present-day mammal species), the expected path length under the strict
#'   clock is just the mean (`path ~ 1`). Note that this mean should be
#'   phylogenetically-normalized. This mean-only model is the simplest possible
#'   model.
#'
#' Punctuated evolution means that more change accumulates during branching
#'   events. As a result, the evolutionary rate is not clock-like. So, we
#'   hypothesize that deviations from a clock-like rate depend on node count
#'   (`path ~ node`).
#'
#' However, the strict clock has a different expectation when the taxa in the
#'   tree are not co-occurring. Examples are when taxa are serially sampled
#'   like SARS-CoV-2 genomes during the COVID-19 pandemic or sporadically
#'   preserved as in the deposition of dinosaur fossils in ancient sediments.
#'   In these cases, the strict clock expects the amount of divergence to scale
#'   proportionally with time (`path ~ sampling time`).
#'
#' In the cases above, the detection of punctuated evolution turns into
#'   estimating the node count "effect" accounting for sampling time
#'   (`path ~ time + node`).
#'
#' We can fit more complex models where we allow the degree of punctuational
#'   effect to vary with time (e.g., `path ~ time + node + time * node`) or
#'   across levels of a grouping variable such as continents or lineages. For
#'   the latter, we can fit a separate-intercepts (e.g.,
#'   `path ~ time + node + continent`) or separate-slopes model (e.g.,
#'   `path ~ time + node + continent + continent * node`).
#'
#' @param data A data frame with path length in the 1st column, node count in
#'   the 2nd, time in the 3rd (optional), group assignment in the 4th
#'   (optional), and taxon names as the row names; make sure that the row order
#'   matches the tree tip labels (use the function `reorder_data`)
#' @param vcv_parts A list outputted from the `decomp_vcv` function
#' @param D A normalizing matrix D outputted from the `create_dmat` function
#' @param model Model options:
#' \itemize{
#'   \item "p": `path ~ 1`
#'   \item "pn": `path ~ node`
#'   \item "pt": `path ~ time`
#'   \item "ptn": `path ~ time + node`
#'   \item "ptni": `path ~ time * node`
#'   \item "png": `path ~ node + group`
#'   \item "pngi": `path ~ node * group`
#'   \item "ptng": `path ~ time + node + group`
#'   \item "ptngi": `path ~ time + node * group`
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
#'   \href{https://doi.org/10.3389/fviro.2023.1066147}{Surya et al. (2023)}
#'
fit_punc_model <- function(
  data,
  vcv_parts,
  D,
  model = c("p", "pn", "pt", "ptn", "ptni", "png", "pngi", "ptng", "ptngi")
) {
  # prepares data
  if (ncol(data) == 2) {
    colnames(data)[1] <- "path"
    colnames(data)[2] <- "node"
  } else if (ncol(data) == 3) {
    colnames(data)[1] <- "path"
    colnames(data)[2] <- "node"
    colnames(data)[3] <- "time"
  } else {
    colnames(data)[1] <- "path"
    colnames(data)[2] <- "node"
    colnames(data)[3] <- "time"
    colnames(data)[4] <- "group"
  }
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
  } else if (model == "ptn") {  # path ~ time + node
    model <- gls(
      model = path ~ time + node,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  } else if (model == "ptni") {  # path ~ time * node
    model <- gls(
      model = path ~ time * node,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  } else if (model == "png") {  # path ~ node + group
    model <- gls(
      model = path ~ node + group,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  } else if (model == "pngi") {  # path ~ node * group
    model <- gls(
      model = path ~ node * group,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  } else if (model == "ptng") {  # path ~ time + node + group
    model <- gls(
      model = path ~ time + node + group,
      data = data,
      correlation = corr,
      weights = varFixed(~w),
      method = "ML"
    )
  } else {  # path ~ time + node * group
    model <- gls(
      model = path ~ time + node * group,
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
