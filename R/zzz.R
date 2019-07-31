# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                              _                                          #
#                             | |                                         #
#                 _ __    ___ | |_  _ __    __ _ __      __               #
#                | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /               #
#                | | | ||  __/| |_ | |_) || (_| | \ V  V /                #
#                |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                 #
#                                  | |                                    #
#                                  |_|                                    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                         #
# File contributors:                                                      #
#   - M.A. Constantin                                                     #
#                                                                         #
# File description:                                                       #
#   - contains package documentation and imports                          #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Notes for my future self ------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# How to read the files in this package?
#    1. start with the diagram in the `assets/plan/` folder
#    2. all lowercase files in the `R/` folder are files that contain regular `R` functions
#    3. all other files (i.e., except this one) are `R6` classes, according to the UML class diagram within `assets/plan/` 



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

# @include logo.R constants.R utils.R externals.R ARandomGraph.R


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# On package attach or load -----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

.onAttach <- function(libname, pkgname) {
    # Print the logo.
    cat(LOGO)
}



# End of file.
