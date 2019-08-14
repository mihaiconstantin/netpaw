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
#   - contains a class that defines a cell configuration                  #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Config class ------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Config <- R6::R6Class("Config",

    private = list(
        ..values = NULL,
        ..timestamps = NULL,


        # Set the values.
        ..set.values = function(..., supplementary, use.options) {
            # Decide how to set the values.
            if(use.options) {
                # Set the values from a number of options and supplementary information.
                private$..from.options(...)
            } else {
                # Set the values from a list already provided.
                private$..from.list(..1)
            }

            # Store the supplementary information.
            if(!is.null(supplementary)) {
                private$..values$supplementary = supplementary
            }
        },


        # Use a list of options to create the config.
        ..from.options = function(..., supplementary) {
            # Capture the `...` arguments.
            options <- list(...)

            # Ensure that all object passed are of correct type.
            assert(private$..are.options(options), ..ERRORS..$incorrect.object.type)

            # Store the values for each option.
            for(option in options) { 
                # Get the ancestor name and lowercase it.
                ancestor <- tolower(option$meta$ancestor)

                # Set the timestamps.
                private$..timestamps[[ancestor]] <- option$meta$timestamp

                # Set the values.
                private$..values[[ancestor]] <- option$values

                # Set the aliases.
                if(ancestor == "graph") {
                    private$..values[[ancestor]][["alias"]] <- option$meta$alias
                } else {
                    private$..values[["alias"]] <- option$meta$alias
                }
            }
        },


        # Use a user-supplied list as values.
        ..from.list = function(config) {
            # Set the values.
            private$..values <- config

            # Set the timestamp.
            private$..timestamps <- Sys.time()
        },


        # Check if all elements of a list are instances of `Option` class.
        ..are.options = function(options) {
            # Check that all object passed are of type `Option`.
            result = all(sapply(options, function(option) { ifelse("Option" %in% class(option), TRUE, FALSE) }))

            return(result)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(..., supplementary = NULL, use.options = TRUE) {
            # Set the values.
            private$..set.values(..., supplementary = supplementary, use.options = use.options)
        }
    ),


    active = list(
        values = function() {
            return(private$..values)
        },


        timestamps = function() {
            return(private$..timestamps)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrapper for creating config objects ----------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
config <- function(...) {
    # Start the factory and get the first instance.
    result <- Factory$new(Config, ...)$first

    return(result)
}



# End of file.
