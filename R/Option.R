Option <- R6::R6Class("Option",

    public = list(
        values = NULL,
        meta = NULL,


        initialize = function(values = NULL, meta = NULL) {
            # Check types.
            if(!is.null(meta)) assert("Meta" %in% class(meta), ..ERRORS..$incorrect.object.type)
            
            # Set fields.
            self$values <- values
            self$meta <- meta
        }
    )
)
