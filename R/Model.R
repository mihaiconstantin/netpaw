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
#   - contains a class that defines a model                               #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Model class -------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Model <- R6::R6Class("Model",

    public = list(
        weights = NULL,
        details = NULL,


        # Constructor.
        initialize = function(weights, details, list = NULL) {
            if(missing(weights) && missing(details)) {
                # Set fields from list.
                if(length(list) > 0) self$weights <- list[[1]]
                if(length(list) > 1) self$details <- list[2: length(list)]
            } else {
                # Set the fields from values.
                self$weights <- weights
                self$details <- details
            }
        },


        # Experimental! Convert from partial correlations to correlations.
        to.correlation = function() {
            # Make sure we have a positive definite matrix. 
            assert(self$is.positive.definite, "Weights matrix is not positive definite.")

            # Get the covariance (correlation perhaps) matrix.
            corr.matrix <- cov2cor(solve(diag(ncol(self$weights)) - self$weights))

            return(corr.matrix)
        },


        # Print.
        print = function(api = TRUE) {
            # General details.
            cat(crayon::bold("Model:"))
            cat("\n")
            cat("  - positive definite:", crayon::yellow(self$is.positive.definite))
            cat("\n")

            # API details.
            if(api) print.class.api(Model)
        }
    ),


    active = list(
        # Check if the model weights matrix is positive definite.
        is.positive.definite = function() {
            return(!any(eigen(diag(ncol(self$weights)) - self$weights)$values < 0))
        }
    )
)



# End of file.
