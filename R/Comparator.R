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

Comparator <- R6::R6Class("Comparator",

    private = list(
        ..true = NULL,
        ..estimated = NULL,
        ..outcome = NULL,


        # Hooks.
        ..before = function() { invisible() },
        ..after = function() { invisible() },


        # Boilerplate.
        ..boot = function() { invisible() },


        # Model comparison.
        ..compare = function(...) {
            # Run before the generator.
            private$..before()

            # Generate the model.
            private$..outcome <- private$..comparator(...)
            
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
        initialize = function() {
            # Boot.
            private$..boot()

            # Compare.
            private$..compare()
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
compare.models <- function(comparator.type, ...) {
    # Make sure the model type requests is known.
    if(!comparator.type %in% names(Comparator$..ALIASES..)) {
        stop(..ERRORS..$unsupported.type)
    }
    
    # Match the pretty names to the blueprints.
    blueprint <- Comparator$..ALIASES..[[comparator.type]]$class

    # Start the factory.
    comparator.factory <- Factory$new(blueprint, ...)

    return(model.factory)
}



# End of file.
