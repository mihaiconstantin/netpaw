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
#   - contains an abstract R6 class that defines a comparator and its     #
#     generation and a wrapper that starts a factory                      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Parent comparator class -------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include Meta.R Option.R Model.R Outcome.R Factory.R



Comparator <- R6::R6Class("Comparator",

    private = list(
        ..true = NULL,
        ..estimated = NULL,
        ..outcome = NULL,
        ..config = NULL,


        # Hooks.
        ..before = function() { invisible() },
        ..after = function() { invisible() },


        # Boilerplate.
        ..boot = function(true, estimated, config) {
            # Type check.
            types.match.expected = ("Model" %in% class(true)) && ("Model" %in% class(estimated)) && ("Config" %in% class(config))
            assert(types.match.expected, ..ERRORS..$incorrect.object.type)

            # Assign the models.
            private$..true <- true
            private$..estimated <- estimated

            # Set the configuration.
            private$..config = config
        },


        # Model comparison.
        ..compare = function(...) {
            # Run before the generator.
            private$..before()

            # Compare the models.
            private$..outcome <- Outcome$new(private$..comparator(...))
            
            # Run after the generator.
            private$..after()
        },


        # Model comparator.
        ..comparator = function(...) {
            stop(..ERRORS..$non.instantiable.class)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(true, estimated, config, ...) {
            # Boot.
            private$..boot(true, estimated, config)

            # Compare.
            private$..compare(...)
        }
    ),


    active = list(
        true = function() { 
            return(private$..true) 
        },


        estimated = function() {
            return(private$..estimated)
        },


        outcome = function() {
            return(private$..outcome)
        },


        config = function() {
            return(private$..config)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Storage for keeping track of supported comparators ----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Comparator$..ALIASES.. <- list()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrapper for comparing models -----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
compare <- function(comparator.type, ...) {
    # Make sure the model type requests is known.
    if(!comparator.type %in% names(Comparator$..ALIASES..)) {
        stop(..ERRORS..$unsupported.type)
    }

    # Match the pretty names to the blueprints.
    blueprint <- Comparator$..ALIASES..[[comparator.type]]$class

    # Start the factory.
    comparator.factory <- Factory$new(blueprint, ...)

    return(comparator.factory)
}



# End of file.
