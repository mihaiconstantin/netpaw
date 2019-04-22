#' \code{netpaw}: A package for corss-sectional network simulations.
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
#'
#' @docType package
#' @name netpaw
"_PACKAGE"



# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Notes for my future self and my future self alone.
# # # # # # # # # # # # # # # # # # # # # # # # # # #

# Preamble:
#     - let's get this straight: building R packages is not the nicest thing, but it can be rewarding in a sense
#     - to avoid endless moments of frustration just grab a pen and paper and write down the tree diagram representation of the package
#     - I know you know how things are organized, but for God's sake just grab the damn pen and paper and don't be a moron!

# In it's current form (i.e., April 22nd, 2019) this package deals only with GGM and Ising models.

# The order in which you want to open the files in the `R/` is the following:
#     1. graph.R -> generates unweighted undirected graphs
#     2. model.R -> adds parameters to a graph
#     3. data.R -> generates data based on a parameterized model
#     4. estimation.R -> fits a model to a give data object
#     5. outcome.R -> compares a true model and an estimated model and computes several outcome indicators
#     6. simulation.R -> a wrapper around the files from 1 to 5 that allows to replicate and store results
#     7. utils.R -> contains useless functions that I once thought were useful
#     8. externals.R -> contains code mostly copied---and simplified a bit---from other packages
#     9. post.R -> allows to post the result of file 5 to whatever API endpoint
