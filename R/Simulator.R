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
        ..replications = NULL,
        ..simulations = NULL,


        # Boilerplate.
        ..boot = function(design) {
            # Type check.
            assert("Design" %in% class(design), ..ERRORS..$incorrect.object.type)

            # Set the design.
            private$..design <- design

            # Set the replications for faster access.
            private$..replications <- design$replications
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
        run.rage = function(range) {},


        # Run a custom selection of simulations (i.e., the subset is a list of specific simulations).
        run.subset = function(subset) {}
    ),


    active = list(
        design = function() {
            return(private$..design)
        },


        replications = function() {
            return(private$..replications)
        },


        simulations = function() {
            return(private$..simulations)
        },


        total = function() {
            return(length(private$..simulations))
        }
    )
)



# End of file.
