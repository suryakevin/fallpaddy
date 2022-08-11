#' Test for the node-density artifact
#'
#' @description This function checks for the node-density artifact, where
#'   branches in parts of a phylogenetic tree with fewer tips are shorter than
#'   their actual lengths. This artifact can bias the regression model towards
#'   detecting punctuated evolution even if this is not the case. A symptom of
#'   this artifact would be a positive curvilinear relationship between the
#'   node count and phylogenetic path length (\eqn{\delta > 1}).
#'
#' @param data A data frame with path length in the 1st column, node count
#'   in the 2nd, and taxon names as the row names; make sure that the row order
#'   matches the tree tip labels (use the function `reorder_data`)
#' @param vcv_parts A list outputted from the `decomp_vcv` function
#'
#' @return This function returns a list of the fitted model and node-density
#'   parameter estimates.
#'
#' @author Kevin Surya
#'
#' @import ape nlme
#'
#' @export
#'
#' @references \href{https://doi.org/10.1126/science.1083202}{Webster et al. (2003)};
#'   \href{https://doi.org/10.1080/10635150600865567}{Venditti et al. (2006)}
#'
est_node_density <- function(data, vcv_parts) {
  # prepares data
  colnames(data)[1] <- "path"
  colnames(data)[2] <- "node"
  if (min(data$node == 0)) {
    data$node == data$node + 1
  }  # prevents logging zero but introduces bias
  data$taxon <- rownames(data)
  corr <- vcv_parts$corr
  data$w <- vcv_parts$w
  # fits PGLS model
  model <- gls(
    model = log(node) ~ log(path),
    data = data,
    correlation = corr,
    weights = varFixed(~w),
    method = "ML"
  )
  # organizes outputs
  beta <- exp(as.numeric(model$coefficients[1]))
  delta <- as.numeric(model$coefficients[2])
  output_nd <- list(model, beta, delta)
  names(output_nd) <- c("model", "beta", "delta")
  return(output_nd)
}

#' Print the node-density test output
#'
#' @description This function saves the summary of the node-density test to a
#'   text file.
#'
#' @param output_nd A list outputted from the `est_node_density` function
#' @param file Output file directory
#'
#' @return This function outputs a text file containing the summary of the
#'   node-density test.
#'
#' @author Kevin Surya
#'
#' @export
#'
print_node_density <- function(output_nd, file) {
  model <- output_nd$model
  beta <- output_nd$beta
  delta <- output_nd$delta
  sink(file)
  cat("=================\n")
  cat("Node-Density Test\n")
  cat("=================\n\n")
  print(summary(model))
  cat("\n")
  cat(paste("Beta = ", beta, "\n", sep = ""))
  cat(paste("Delta = ", delta, sep = ""))
  cat("\n")
  sink()
}
