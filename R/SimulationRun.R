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
#   - contains a class that defines a simulation run                      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# SimulationRun class -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

SimulationRun <- R6::R6Class("SimulationRun",

    private = list(
        # Input.
        ..config = NULL,

        # Output.
        ..generator = NULL,
        ..sampler = NULL,
        ..estimator = NULL,
        ..comparator = NULL,

        # Informative fields.
        ..error = NULL,
        ..start = NULL,
        ..end = NULL,


        # Apply the procedure for the current simulation run.
        ..apply.procedure = function() {
            # Generate.
            private$..set.generator()

            # Sample.
            private$..set.sampler()

            # Estimate.
            private$..set.estimator()

            # Compare.
            private$..set.comparator()
        },


        # Run the procedure.
        ..run = function() {
            # Record the start.
            private$..start <- Sys.time()

            # Try to run the simulation procedure.
            tryCatch(private$..apply.procedure(), error = function(error) {
                # Store the error.
                private$..error <- error
            })

            # Record the end.
            private$..end <- Sys.time()
        },


        # Call the model generator.
        ..set.generator = function(generator) {
            # If a generator is not provided in the constructor, then call one.
            if(is.null(private$..generator)) {
                # Store the arguments.
                args <- c(type = self$alias, private$..config$values$generator)

                # Append the graph if needed.
                if(self$needs.graph) {
                    # Graph arguments.
                    graph.args <- c(type = private$..config$values$graph$alias, private$..config$values$graph[!grepl("alias", names(private$..config$values$graph))])

                    # Call the graph generator.
                    graph <- do.call("generate.graph", graph.args)

                    # Store the graph.
                    args <- c(graph = graph, args)
                }

                # Call the model generator.
                private$..generator <- do.call("generate.model", args)
            }
        },


        # Call the data sampler.
        ..set.sampler = function() {
            # Store the arguments.
            args <- c(type = self$alias, model = private$..generator$model, private$..config$values$sampler)

            # Call the sampler.
            private$..sampler <- do.call("generate.data", args)
        },


        # Call the model estimator.
        ..set.estimator = function() {
            # Store the arguments.
            args <- c(type = self$alias, data = private$..sampler$data, private$..config$values$estimator[!grepl("implementation", names(private$..config$values$estimator))])

            # Set the implementation.
            args <- c(thinking = private$..config$values$estimator$implementation, args)

            # Call the estimator.
            private$..estimator <- do.call("estimate", args)
        },


        # Call the model comparator.
        ..set.comparator = function(config) {
            # Store the arguments.
            args <- c(type = self$alias, true = private$..generator$model, estimated = private$..estimator$model, config = private$..config)

            # Call the comparator.
            private$..comparator <- do.call("compare", args)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(config, generator = NULL) {
            # Set the config for faster access (i.e., the reference).
            private$..config <- config

            # Set the generator if provided, otherwise it will be null and called within `run()`.
            private$..generator <- generator

            # Run.
            private$..run()
        }
    ),


    active = list(
        config = function() {
            return(private$..config)
        },


        generator = function() {
            return(private$..generator)
        },


        sampler = function() {
            return(private$..sampler)
        },


        estimator = function() {
            return(private$..estimator)
        },


        comparator = function() {
            return(private$..comparator)
        },


        error = function() {
            return(private$..error)
        },


        start = function() {
            return(private$..start)
        },


        end = function() {
            return(private$..end)
        },


        duration = function() {
            return(private$..end - private$..start)
        },


        needs.graph = function() {
            return(!is.null(private$..config$values$graph))
        },


        alias = function() {
            return(private$..config$values$alias)
        }
    )
)



# End of file.
