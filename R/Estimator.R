# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                              _                                           #
#                             | |                                          #
#                 _ __    ___ | |_  _ __    __ _ __      __                #
#                | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /                #
#                | | | ||  __/| |_ | |_) || (_| | \ V  V /                 #
#                |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                  #
#                                  | |                                     #
#                                  |_|                                     #
# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                          #
# File contributors:                                                       #
#   - M.A. Constantin                                                      #
#                                                                          #
# File description:                                                        #
#   - contains an abstract R6 class that defines an estimator and its      #
#     generation and a wrapper that starts a factory                       #
#                                                                          #
# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Parent estimator class --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Estimator <- R6::R6Class("Estimator",

    private = list(
        ..options = NULL,
        ..model = NULL,
        ..data = NULL,
        ..thinking = NULL,


        # Hooks.
        ..before = function() { invisible() },
        ..after = function() { invisible() },


        # Boilerplate.
        ..boot = function(data, thinking) {
            # Initialize and store the data.
            private$..data <- Data$new(dataset = data)

            # Set the thinking method.
            private$..thinking = thinking

            # Initialize the Option object and set the meta field.
            private$..options <- Option$new(meta = Meta$new(type = class(self)[1]))

            # Set the values field on the options at runtime.
            patch.function.within.environment("..generator", private, "private$..options$values <- combine.arguments(private$..generator, as.list(match.call())[-1])")
        },


        # Model estimation.
        ..estimate = function(...) {
            # Run before the estimator.
            private$..before()

            # Pick the right estimation type and run the estimator.
            if(private$..thinking == "frequentist") {
                private$..model <- Model$new(list = private$..frequentist(...))
            } else { 
                private$..model <- Model$new(list = private$..bayesian(...))
            }

            # Run after the estimator.
            private$..after()
        },


        # Model estimator, frequentist.
        ..frequentist = function(...) {
            stop(..ERRORS..$non.instantiable.class)
        },


        # Model estimator, bayesian.
        ..bayesian <- function(...) {
            stop(..ERRORS..$non.instantiable.class)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(data, ..., thinking = "frequentist") {
            # Boot.
            private$..boot(data, thinking)

            # Estimate.
            private$..estimate(...)
        }
    ),


    active = list(
        options = function() {
            return(private$..options)
        },


        model = function() {
            return(private$..model)
        },


        data = function() {
            return(private$..data)
        },


        thinking = function() {
            return(private$..thinking)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Storage for keeping track of supported estimators -----------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Estimator$..ALIASES.. <- list()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Exported wrapper for estimating models ----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
estimate.model <- function(estimator.type, ...) {
    # Make sure the requested estimator type is known.
    if(!estimator.type %in% names(Estimator$..ALIASES..)) {
        stop(..ERRORS..$unsupported.type)
    }
    
    # Match the pretty names to the blueprints.
    blueprint <- Estimator$..ALIASES..[[estimator.type]]$class

    # Start the factory.
    estimator.factory <- Factory$new(blueprint, ...)

    return(estimator.factory)
}



# End of file.
