Model <- R6::R6Class("Model",

    public = list(
        weights = NULL,
        details = NULL,


        initialize = function(weights, details, list = NULL) {
            if(missing(weights) && missing(details)) {
                # Set fields from list.
                self$weights <- list[[1]]
                self$details <- list[[2]]
            } else {
                # Set the fields from values.
                self$weights <- weights
                self$details <- details
            }
        }
    )
)
