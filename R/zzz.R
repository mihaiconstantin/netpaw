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
#' @import R6
#' @import progress
#' @import foreach
#' @import doParallel
#' @import digest
#'
#' @docType package
#' @name netpaw
"_PACKAGE"



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Mechanism to change package specific options ----------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
..set.netpaw.option <- function(key, value) {
    # Get the current package options.
    package.options <- getOption("netpaw")

    # Update the specified key.
    package.options[[key]] <- value

    # Store the updated options
    options("netpaw" = package.options)
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# On package attach or load -----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

.onAttach <- function(libname, pkgname) {
    # Add package specific default options.
    options("netpaw" = list(
        # Option about using a colorful logo.
        logo.colors = if(is.null(getOption("netpaw")$logo.colors)) TRUE else getOption("netpaw")$logo.colors
    ))

    # Print the logo with our without colors.
    if(getOption("netpaw")$logo.colors) cat(LOGO.COLOR, "\n") else cat(LOGO.NO.COLOR, "\n")
}



# End of file.
