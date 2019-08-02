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
        }
    ),


    active = list(
        # Check if the model weights matrix is positive definite.
        is.positive.definite = function() {
            return(!any(eigen(diag(ncol(self$weights)) - self$weights)$values < 0))
        },


        # Experimental! Convert from partial correlations to correlations.
        to.correlation = function() {
            # Make sure we have a positive definite matrix. 
            assert(self$is.positive.definite, "Weights matrix is not positive definite.")

            # Get the covariance (correlation perhaps) matrix.
            corr.matrix <- cov2cor(solve(diag(ncol(self$weights)) - self$weights))

            return(corr.matrix)
        }
    )
)



# End of file.
