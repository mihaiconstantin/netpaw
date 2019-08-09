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
#   - contains an abstract R6 class that defines an estimator and its     #
#     generation and a wrapper that starts a factory                      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include Meta.R Option.R Model.R Factory.R



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
            # Type check and assertions.
            assert("Data" %in% class(data), ..ERRORS..$incorrect.object.type)

            # Make sure the correct thinking argument is specified.
            assert(thinking == "frequentist" || thinking == "bayesian", ..ERRORS..$unsupported.type)

            # Set the injected data.
            private$..data <- data

            # Set the thinking method.
            private$..thinking = thinking

            # Initialize the Option object and set the meta field.
            private$..options <- Option$new(meta = Meta$new(type = class(self)[1]))

            # Set the estimator.
            set.locked.binding("..estimator", private, ifelse(private$..thinking == "frequentist", private$..frequentist, private$..bayesian))

            # Set the values field on the options at runtime.
            patch.function.within.environment("..estimator", private, "private$..options$set.values(combine.arguments(private$..estimator, as.list(match.call())[-1]))")
        },


        # Model estimation.
        ..estimate = function(...) {
            # Run before the estimator.
            private$..before()

            # Pick the right estimation type and run the estimator.
            private$..model <- Model$new(list = private$..estimator(...))

            # Run after the estimator.
            private$..after()
        },


        # Model estimator as implemented by the user. Set during boot-time based on the `thinking` field.
        ..estimator = function() {},


        # Model estimator, frequentist.
        ..frequentist = function(...) {
            stop(..ERRORS..$non.instantiable.class)
        },


        # Model estimator, bayesian.
        ..bayesian = function(...) {
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
estimate <- function(type, ...) {
    # Make sure the graph type requests is known.
    assert(type %in% names(Estimator$..ALIASES..), ..ERRORS..$unsupported.type)

    # Match the pretty names to the blueprints.
    blueprint <- Estimator$..ALIASES..[[type]]$class

    # Start the factory and get the first instance.
    result <- Factory$new(blueprint, ...)$first

    return(result)
}



# End of file.
