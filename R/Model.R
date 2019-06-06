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


        initialize = function(weights, details, list = NULL) {
            if(missing(weights) && missing(details)) {
                # Set fields from list.
                if(length(list) > 0) self$weights <- list[[1]]
                if(length(list) > 1) self$details <- list[[2]]
            } else {
                # Set the fields from values.
                self$weights <- weights
                self$details <- details
            }
        }
    )
)



# End of file.
