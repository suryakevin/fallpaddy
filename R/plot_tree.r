#' Plot phylogenetic tree
#'
#' @description This function plots a phylogenetic tree.
#'
#' @param tree An object of class `phylo`
#' @param unit Branch length unit (e.g., subs/site and mya)
#' @param size Branch thickness
#'
#' @return This function returns an object of the `ggtree` class.
#'
#' @author Kevin Surya
#'
#' @import ggimage
#'
#' @importFrom ggtree ggtree theme_tree2
#'
#' @export
#'
plot_tree <- function(tree, unit, size = 0.3) {
  plot <-
    ggtree(tr = tree, size = size) +
      theme_tree2() +
      labs(caption = unit)
  return(plot)
}

#' Plot phylogenetic tree with colored branches
#'
#' @description This function plots a phylogenetic tree with colors
#'   representing groups
#'
#' @param tree An object of class `phylo`
#' @param data A data frame with taxon name in the row name and the group
#'   assignment in the 1st column
#' @param unit Branch length unit (e.g., subs/site and mya)
#' @param size Branch thickness
#'
#' @return This function returns an object of the `ggtree` class.
#'
#' @author Kevin Surya
#'
#' @import ggimage
#'
#' @importFrom ggtree ggtree groupOTU theme_tree2
#' @importFrom rlang .data
#' @importFrom stats reorder
#'
#' @export
#'
plot_tree_color <- function(tree, data, unit, size = 0.3) {
  # prepares data
  data <- cbind(rownames(data), data)
  colnames(data)[1] <- "taxon"
  colnames(data)[2] <- "group"
  data$group <- as.factor(data$group)
  group_lvl <- levels(
    rev(
      reorder(x = data$group, X = data$group, FUN = length)
    )
  )
  len_lvl <- nlevels(data$group)
  group_list <- vector(mode = "list", length = len_lvl)
  for (i in 1:len_lvl) {
    group_list[[i]] <- as.character(data$taxon[data$group == group_lvl[i]])
  }
  names(group_list) <- group_lvl
  tr_object <- groupOTU(tree, group_list)
  # plots tree
  plot <-
    ggtree(tr = tr_object, aes(color = .data$group), size = size) +
      theme_tree2(legend = "right", legend.title = element_blank()) +
      labs(caption = unit)
  return(plot)
}
