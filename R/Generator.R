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
#   - contains an abstract R6 class that defines a generator and its      #
#     generation and a wrapper that starts a factory                      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @inlcude Option.R Meta.R Model.R Factory.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Parent generator class --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Generator <- R6::R6Class("Generator",

    private = list(
        ..options = NULL,
        ..model = NULL,


        # Hooks.
        ..before = function() { invisible() },
        ..after = function() { invisible() },


        # Boilerplate.
        ..boot = function() {
            # Prepare the Option object and set the meta field.
            private$..options <- Option$new(meta = Meta$new(type = class(self)[1]))

            # Set the values field on the options at runtime.
            patch.function.within.environment("..generator", private, "private$..options$values <- combine.arguments(private$..generator, as.list(match.call())[-1])")
        },


        # Model generation.
        ..generate = function(...) {
            # Run before the generator.
            private$..before()

            # Generate the model.
            private$..model <- Model$new(list = private$..generator(...))
            
            # Run after the generator.
            private$..after()
        },


        # Model generator.
        ..generator = function(...) {
            stop(..ERRORS..$non.instantiable.class)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(...) {
            # Boot.
            private$..boot()

            # Generate.
            private$..generate(...)
        }
    ),


    active = list(
        options = function() { 
            return(private$..options) 
        },


        model = function() {
            return(private$..model)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Storage for keeping track of supported generators -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Generator$..ALIASES.. <- list()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrapper for generating models ----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
generate.model <- function(model.type, ...) {
    # Make sure the model type requests is known.
    if(!model.type %in% names(Generator$..ALIASES..)) {
        stop(..ERRORS..$unsupported.type)
    }
    
    # Match the pretty names to the blueprints.
    blueprint <- Generator$..ALIASES..[[model.type]]$class

    # Start the factory.
    model.factory <- Factory$new(blueprint, ...)

    return(model.factory)
}



# End of file.
