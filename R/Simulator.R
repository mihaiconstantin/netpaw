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
#   - contains a class that defines a Simulator                           #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include Config.R Simulation.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Simulator class ---------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Simulator <- R6::R6Class("Simulator",

    private = list(
        ..design = NULL,
        ..simulations = NULL,
        ..targets = list(),


        # Boilerplate.
        ..boot = function(design) {
            # Type check.
            assert("Design" %in% class(design), ..ERRORS..$incorrect.object.type)

            # Set the design.
            private$..design <- design
        },


        # Register a design.
        ..create.simulations = function() {
            # Create data frames containing combinations of all design factors.
            combinations <- private$..combine.design.factors()

            # Set the simulation list size.
            private$..simulations <- vector("list", length = length(combinations))

            # Parse the data frames of combinations into configuration lists.
            for (i in 1:length(combinations)) {
               private$..simulations[[i]] <- Simulation$new(Config$new(combinations[[i]], use.options = FALSE), private$..design$replications)
            }
        },


        # Create sensible combinations of all design factors.
        ..combine.design.factors = function() {
            # Prepare the storage for the combinations.
            combinations.with.graph <- list()
            combinations.without.graph <- list()

            for(model in private$..design$structure$model) {
                # Gather and name all model specifications (i.e., simulation steps).
                specification <- list(
                    alias     = model$alias,
                    generator = model$generator,
                    sampler   = model$sampler,
                    estimator = model$estimator
                )

                # Handle graph if it exists.
                if(!is.null(model$graph)) {
                    for(graph in model$graph) {
                    # Create a list with all possible combinations, including the graph.
                    combinations.with.graph[[model$alias]][[graph$alias]] <- list.combine(c(specification, list(graph = graph)))
                    }
                # In case a graph doesn't exist.
                } else {
                    # Create a list with all possible combinations, without a graph.
                    combinations.without.graph[[model$alias]] <- list.combine(specification)
                }
            }

            # Flatten things, just a bit (i.e., each first-level list is a simulation config).
            combinations.with.graph <- unlist(unlist(combinations.with.graph , recursive = FALSE), recursive = FALSE)
            combinations.without.graph <- unlist(combinations.without.graph, recursive = FALSE)

            # Put everything together.
            combinations <- c(combinations.with.graph, combinations.without.graph)

            return(combinations)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(design) {
            # Boot.
            private$..boot(design)

            # Create simulations from design.
            private$..create.simulations()
        },


        # Run a range of simulations (i.e., the range is based on the `..simulations` field).
        run.rage = function(start, end, ...) {
            # Prevent range overflow.
            assert((start > 0) && (end <= length(private$..simulations)), "Invalid simulation range.")

            # Keep track of the things we've ran.
            private$..targets <- c(private$..targets, list(range = start:end))

            # Run simulations in specified range.
            for (i in start:end) {
               private$..simulations[[i]]$perform(...)
            }
        },


        # Run a subset of simulations.
        run.subset = function(subset, ...) {
            # Prevent subset overflow.
            assert((min(subset) > 0) && max(subset) <= length(private$..simulations), "Invalid simulation subset.")

            # Keep track of the things we've ran.
            private$..targets <- c(private$..targets, list(subset = subset))

            # Run the simulations specified in the subset.
            for (i in subset) {
               private$..simulations[[i]]$perform(...)
            }
        }
    ),


    active = list(
        design = function() {
            return(private$..design)
        },


        simulations = function() {
            return(private$..simulations)
        },


        targets = function() {
            return(private$..targets)
        },


        total = function() {
            return(length(private$..simulations))
        },


        completed = function() {
            return(length(unlist(private$..targets)))
        }
    )
)



# End of file.
