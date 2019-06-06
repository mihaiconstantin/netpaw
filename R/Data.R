Data <- R6::R6Class("Data",

    public = list(
        dataset = NULL,


        initialize = function(dataset = NULL) {
            # Set fields.
            self$dataset <- dataset
        }
    )
)
