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
        ..simulations = list(),


        # Boilerplate.
        ..boot = function(design) {
            # Type check.
            assert("Design" %in% class(design), ..ERRORS..$incorrect.object.type)

            # Set the design.
            private$..design <- design
        },


        # Create the simulator.


        # Create from design.
        ..create.from.design = function() {
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
        },


        # Backend for the run API.


        # Run all simulations.
        ..run = function(verbose, ...) {
            # Run the simulations.
            for (i in 1:length(private$..simulations)) {
               private$..simulations[[i]]$perform(..., verbose = verbose)
            }
        },


        # Core logic for running a range of simulations (i.e., the range is based on the `..simulations` field).
        ..run.rage = function(start, end, verbose, ...) {
            # Prevent range overflow.
            assert((start > 0) && (end <= length(private$..simulations)), "Invalid simulation range.")

            # Run simulations in specified range.
            for (i in start:end) {
               private$..simulations[[i]]$perform(..., verbose = verbose)
            }
        },


        # Core logic for running a subset of simulations.
        ..run.subset = function(subset, verbose, ...) {
            # Prevent subset overflow.
            assert((min(subset) > 0) && max(subset) <= length(private$..simulations), "Invalid simulation subset.")

            # Run the simulations specified in the subset.
            for (i in subset) {
               private$..simulations[[i]]$perform(..., verbose = verbose)
            }
        },


        # Things to be done before the runners.
        ..before = function(total) {
            # Console feedback.
            cat("Simulator engaged.", "\n")
            cat("Replicating ", total, " simulation(s). Start time: ", as.character(Sys.time()), ".", "\n\n", sep = "")
        },


        # Things to be done after the runners.
        ..after = function() {
            # Console feedback.
            cat("Simulator finished. End time: ", as.character(Sys.time()), ".", sep = "")
        }
    ),


    public = list(
        # Constructor.
        initialize = function(design = NULL) {
            # If the design is provided, then create the simulator from it.
            if(!is.null(design)) {
                # Boot.
                private$..boot(design)

                # Create the simulator.
                private$..create.from.design()
            }
        },


        run = function(verbose = TRUE, ...) {
            # Actions before the run.
            if(verbose) private$..before(length(private$..simulations))

            # Run.
            private$..run(verbose, ...)

            # Actions after the run.
            if(verbose) private$..after()
        },


        # Wrap the range runner with before and after actions.
        run.range = function(start, end, verbose = TRUE, ...) {
            # Actions before the run.
            if(verbose) private$..before(length(start:end))

            # Run.
            private$..run.rage(start, end, verbose, ...)

            # Actions after the run.
            if(verbose) private$..after()
        },


        # Wrap the subset runner with before and after actions.
        run.subset = function(subset, verbose = TRUE, ...) {
            # Actions before the run.
            if(verbose) private$..before(length(subset))

            # Run.
            private$..run.subset(subset, verbose, ...)

            # Actions after the run.
            if(verbose) private$..after()
        },


        # Pop specified simulations.
        pop = function(indices) {
            private$..simulations <- private$..simulations[-indices]
        },


        # Keeps the specified simulations (i.e., by index) and drops all the others.
        keep = function(indices) {
            private$..simulations <- private$..simulations[indices]
        },


        # Merge a simulator instance (i.e., its fields will be merged).
        # TODO: Not now, but later, see how to recreate a `Design` object from a list of simulations.
        merge = function(simulator) {
            # Type check.
            assert("Simulator" %in% class(simulator), ..ERRORS..$incorrect.object.type)

            # Merge the simulations.
            private$..simulations <- c(private$..simulations, simulator$simulations)
        },


        # Print.
        print = function() {
            # General details.
            cat(crayon::bold("Simulator:"))
            cat("\n")
            cat("  - conditions:", crayon::yellow(self$total))
            cat("\n")
            cat("  - completed:", crayon::yellow(self$completed))
            cat("\n")
            cat("  - targets:", paste(crayon::yellow(if(length(self$targets)) unlist(self$targets)[order(unlist(self$targets))] else 0), collapse = crayon::silver(" | ")))
            cat("\n")

            # API details.
            print.class.api(Simulator)
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
            return(which(sapply(private$..simulations, function(simulation) {
                simulation$replications == simulation$completed
            })))
        },


        total = function() {
            return(length(private$..simulations))
        },


        completed = function() {
            return(length(unlist(self$targets)))
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrappers for simulator API -------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
simulator <- function(design) {
    # Build the simulator.
    simulator <- Simulator$new(design)

    return(simulator)
}



#' Note: the order is not guaranteed!
#' @export
merge.simulators <- function(path = NULL, ..., verbose = TRUE) {
    # Crete an empty simulator.
    simulator <- Simulator$new()

    # Initialize the progress bar.
    if(verbose) progress.bar = progress::progress_bar$new(format = "[:bar] merging simulator :current of :total (:elapsed)", clear = FALSE)

    # Console feedback on start.
    if(verbose) cat("Merging simulators.", "\n")

    # Merge from a provided path.
    if(!is.null(path)) {
        # Get the files where the simulators are saved.
        files = list.files(path)

        # Bypass the progress bar and set the total.
        if(verbose) progress.bar$.__enclos_env__$private$total <- length(files)

        # Load and merge the simulators.
        for(file in files) {
            # Tick the progress bar.
            if(verbose) progress.bar$tick()

            # Load the simulator.
            split.simulator <- readRDS(paste0(path, "/", file))

            # Merge the simulator.
            simulator$merge(split.simulator)
        }

    # Merge from a list of simulators.
    } else {
        # Capture all simulators.
        simulators <- list(...)

        # Bypass the progress bar and set the total.
        if(verbose) progress.bar$.__enclos_env__$private$total <- length(simulators)

        # Merge the simulators.
        for (split.simulator in simulators) {
            # Tick the progress bar.
            if(verbose) progress.bar$tick()

            # Merge the simulator.
            simulator$merge(split.simulator)
        }
    }

    # Handle the progress bar completion to avoid printing issues (e.g., if finished earlier than expected ticks, mark it as terminated manually).
    if(verbose && !progress.bar$finished) progress.bar$terminate()

    # Console feedback on end
    if(verbose) cat("Merge completed.", "\n")

    return(simulator)
}



# End of file.
