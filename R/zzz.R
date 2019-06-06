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
#' @import progress
#'
#' @docType package
#' @name netpaw
"_PACKAGE"



# # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Notes for my future self.
# # # # # # # # # # # # # # # # # # # # # # # # # # #

# How to read the files in this package?
#    1. start with the diagram in the `assets/plan/` folder
#    2. all files that begin with `_` in the `R/` folder are files that contain regular `R` functions
#    3. all other files (i.e., except this one) are `R6` classes, according to the UML class diagram within `assets/plan/` 



# To run before the package is loaded.
.onAttach <- function(libname, pkgname) {
    # Print the logo.
    cat(LOGO)
}
