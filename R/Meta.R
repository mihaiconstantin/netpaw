Meta <- R6::R6Class("Meta",

    public = list(
        timestamp = NULL,
        type = NULL,


        initialize = function(type = NULL) {
            # Record timestamp.
            self$timestamp <- Sys.time()

            # Set fields.
            self$type <- type
        }
    )
)
