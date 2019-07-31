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
#   - contains an abstract R6 class that defines a graph and its          #
#     generation and a wrapper that starts a factory                      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @inlcude Option.R Meta.R Factory.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Parent graph class ------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Graph <- R6::R6Class("Graph",

    private = list(
        ..options = NULL,
        ..adjacency = NULL,


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


        # Graph generation.
        ..generate = function(...) {
            # Run before the generator.
            private$..before()

            # Generate the graph.
            private$..adjacency <- private$..generator(...)

            # Run after the generator.
            private$..after()
        },


        # Graph generator.
        ..generator = function(...) {
            stop(..ERRORS..$non.instantiable.class)
        }
    ),


    public = list(
        # Constructor
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


        adjacency = function() {
            return(private$..adjacency)
        },


        number.nodes = function() {
            # Determine the dimensions.
            dimensions <- dim(private$..adjacency)

            # Check the dimensions.
            if(dimensions[1] != dimensions[2]) stop('Wrong type of input: the graph dimensions do not match.')

            # Return the number of nodes.
            return(dimensions[1])
        },


        density = function() {
            # Potential connections.
            potential = (self$number.nodes * (self$number.nodes - 1)) / 2

            # Actual connections.
            actual = sum(private$..adjacency[upper.tri(private$..adjacency)] != 0)

            # Density.
            density = actual / potential

            return(density)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Storage for keeping track of supported graphs ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Graph$..ALIASES.. <- list()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrapper for generating graphs ----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
generate.graph <- function(graph.type, ...) {
    # Make sure the graph type requests is known.
    if(!graph.type %in% names(Graph$..ALIASES..)) {
        stop(..ERRORS..$unsupported.type)
    }

    # Match the pretty names to the blueprints.
    blueprint <- Graph$..ALIASES..[[graph.type]]$class

    # Start the factory.
    graph.factory <- Factory$new(blueprint, ...)

    return(graph.factory)
}



# End of file.
