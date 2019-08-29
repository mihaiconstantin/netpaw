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
        ..options = NULL,
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

            # Prepare the Option object and set the meta field.
            private$..options <- Option$new(meta = Meta$new(type = class(self)[1]))

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
        },


        # Print.
        print = function() {
            # General details.
            cat(crayon::bold("Comparator:"))
            cat("\n")
            cat("  - wrapper:", crayon::yellow("compare(type, ...)"))
            cat("\n")

            # Outcome details.
            print(private$..outcome, api = FALSE)

            # Config details.
            print(private$..config, api = FALSE)

            # Option details.
            print(private$..options, api = FALSE)

            # API details.
            print.class.api(eval(as.symbol(private$..options$meta$type)), parent = TRUE)
        }
    ),


    active = list(
        options = function() {
            return(private$..options)
        },


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
compare <- function(type, ...) {
    # Make sure the graph type requests is known.
    assert(type %in% names(Comparator$..ALIASES..), ..ERRORS..$unsupported.type)

    # Match the pretty names to the blueprints.
    blueprint <- Comparator$..ALIASES..[[type]]$class

    # Start the factory and get the first instance.
    result <- Factory$new(blueprint, ...)$first

    return(result)
}



# End of file.
