#' Detecting punctuated evolution in taxa ranging from dinosaurs to viruses
#'
#' @description This package contains the tools to simulate, detect, and
#'   visualize punctuated evolution in any clades, from co-occurring species
#'   (e.g., present-day mammals) to serially-sampled viruses (e.g., SARS-CoV-2
#'   genomes) and long-extinct taxa (e.g., Mesozoic dinosaurs).
#'
#' @section Function directory:
#' \itemize{
#'   \item Main functions:
#'     \itemize{
#'       \item Extracting data: `get_node_count`, `get_path_length`, and
#'         `get_sampling_time`
#'       \item Preparing dataset: `reorder_data`
#'       \item Editing the phylogenetic variance-covariance matrix:
#'         `decomp_vcv` and `create_dmat`
#'       \item Detecting punctuated evolution: `fit_punc_model`
#'       \item Calculating test statistics: `calc_r2`, `calc_partialr2`,
#'         `est_branching_rate` and `est_punc_contrib`
#'       \item Comparing models: `compare_models`
#'       \item Checking biases and diagnostics: `calc_vif`, `est_node_density`,
#'         `plot_diagnostics`, and `plot_node_density`
#'       \item Visualizing data and results: `export_plot_html`,
#'         `export_plot_pdf`, `export_plot_svg`, `plot_data_2d`,
#'         `plot_data_3d`, `plot_punc_model`, `plot_tree`, and `plot_tree_color`
#'       \item Simulating punctuated evolution: `sim_punc`
#'     }
#'   \item Auxillary functions:
#'     \itemize{
#'       \item Importing dataset: `import_data`, `import_data_bt`,
#'         `import_data_csv`, and `import_data_excel`
#'       \item Importing phylogenetic tree: `import_tree_nex` and
#'         `import_tree_nwk`
#'       \item Exporting dataset: `export_data` and `export_data_bt`
#'       \item Exporting tree: `export_tree_nex` and `export_tree_nwk`
#'       \item Printing outputs: `print_node_density` and `print_punc_model`
#'     }
#' }
#'
#' @author Kevin Surya
#'
#' @references Surya, K., Gardner, J.D. & Organ, C. Detecting punctuated
#'   evolution in SARS-CoV-2 over the first year of the pandemic. In review.
#'
#' @docType package
#'
#' @name fallpaddy
#'
NULL
