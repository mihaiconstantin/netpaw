#' \code{netpaw}: A package for cross-sectional and time series network simulations.
#'
#' The \code{netpaw} package provides three categories of functions...
#' TODO: Add documentation.
#' 
#' @section \code{netpaw} functions:
#' The \code{netpaw} functions...
#'
#' @import Matrix
#' @import glmnet
#' @import igraph
#' @import qgraph
#' @import IsingFit
#' @import IsingSampler
#' @import bootnet
#' @import crayon
#' @import audio
#' @import R6
#'
#' @docType package
#' @name netpaw
"_PACKAGE"



# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Notes for my future self.
# # # # # # # # # # # # # # # # # # # # # # # # # # #

# The order in which you want to open the files in the `R/` is the following:
#     1. graph.R -> generates unweighted undirected graphs
#     2. model.R -> adds parameters to a graph
#     3. data.R -> generates data based on a parameterized model
#     4. estimation.R -> fits a model to a give data object
#     5. outcome.R -> compares a true model and an estimated model and computes several outcome indicators
#     6. simulation.R -> a wrapper around the files from 1 to 5 that allows to replicate and store results
#     7. post.R -> allows to post the result of file 5 to whatever API endpoint
#     8. utils.R -> contains useless functions that I once thought were useful
#     9. externals.R -> contains code mostly copied---and simplified a bit---from other packages



# To run before the package is loaded.
.onAttach <- function(libname, pkgname) {
    # Print the logo.
    cat(LOGO)
}
