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


        ..are.options = function(options) {
            # Check that all object passed are of type `Option`.
            result = all(sapply(options, function(option) { ifelse("Option" %in% class(option), TRUE, FALSE) }))

            return(result)
        },


        ..attach.values = function(..., supplementary) {
            # Capture the `...` arguments.
            options <- list(...)

            # Ensure that all object passed are of correct type.
            assert(private$..are.options(options), ..ERRORS..$incorrect.object.type)

            # Store the values for each option.
            for(option in options) { 
                private$..values = c(private$..values, option$to.list()) 
            }

            # Store the supplementary information.
            if(!is.null(supplementary)) {
                private$..values$supplementary = supplementary
            }
        }
    ),


    public = list(
        # Constructor.
        initialize = function(..., supplementary = NULL) {
            # Set the values from a number of options and supplementary information.
            private$..attach.values(..., supplementary = supplementary)
        }
    ),


    active = list(
        values = function() {
            return(private$..values)
        }
    )
)



# End of file.
