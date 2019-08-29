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
#   - contains a class that defines a simulation                          #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include SimulationRun.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Simulation class --------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Simulation <- R6::R6Class("Simulation",

    private = list(
        # Input fields.
        ..config = NULL,
        ..hash = NULL,
        ..replications = NULL,


        # Output fields.
        ..runs = list(),


        # Boilerplate.
        ..boot = function(config, replications) {
            # Type check.
            assert("Config" %in% class(config), ..ERRORS..$incorrect.object.type)

            # Set the config.
            private$..config <- config

            # Initialize the replications count.
            private$..replications <- replications

            # Compute the config hash.
            private$..hash <- digest::digest(config, algo = "md5")
        }
    ),


    public = list(
        # Constructor.
        initialize = function(config, replications) {
            # Boot.
            private$..boot(config, replications)
        },


        # Perform and replicate the simulation.
        # TODO: Allow to run chunks of replications (i.e., different processes) that can later be glued based on the simulation hash.
        perform = function(vary.generator = FALSE, verbose = FALSE) {
            # Initialize the progress bar.
            progress.bar = progress::progress_bar$new(total = private$..replications, format = "[:bar] replication :current of :total (:elapsed)", clear = FALSE)

            # Console feedback on start.
            if(verbose) cat("Running simulation `", self$short.hash, "` (", as.character(Sys.time()) ,"):", "\n", sep = "")

            # If no replication ran has been performed, run for the first time.
            if(!length(private$..runs)) {
                # Tick the progress bar manually (i.e., because we loop from 2 onwards). Ew!
                if(verbose) progress.bar$tick()

                # Perform the first replication.
                private$..runs[[1]] <- SimulationRun$new(private$..config)
            }

            # If there are subsequent replications, decide what generator to use and run them.
            if(private$..replications > 1) {
                # Decide on what generator to use (i.e., reuse the first ever created or vary it for each run).
                if(!vary.generator) { generator <- private$..runs[[1]]$generator } else { generator <- NULL }

                # Run and replicate the remaining.
                for (i in 2:private$..replications) {
                    # Tick the progress bar.
                    if(verbose) progress.bar$tick()

                    # Perform the remainder of the replications.
                    private$..runs[[i]] <- SimulationRun$new(config = private$..config, generator = generator)
                }
            }

            # Handle the progress bar completion to avoid printing issues (e.g., if finished earlier than expected ticks, mark it as terminated manually).
            if(!progress.bar$finished) progress.bar$terminate()

            # Console feedback on end.
            if(verbose) cat("Simulation `", self$short.hash ,"` completed. Status: ", self$completed, " replications | ", self$warnings, " warnings | ", self$errors, " errors.", "\n\n", sep = "")
        }
    ),


    active = list(
        # Extract all outcomes from the `private$..runs` as a list.
        outcome.list = function() {
            if(self$completed > 0) {
                return(lapply(private$..runs, function(run) {
                    run$comparator$outcome$values[sapply(run$comparator$outcome$values, function(value) {
                        is.numeric(value) || is.logical(value)
                    })]
                }))
            } else {
                return(NULL)
            }
        },


        # Extract all outcomes from the `private$..runs` as a data frame.
        outcome.frame = function() {
            return(do.call(rbind, lapply(self$outcome.list, as.data.frame)))
        },


        # Average all outcomes extracted from `private$..runs`.
        outcome.means = function() {
            if(self$completed > 0) {
                return(as.list(colMeans(self$outcome.frame, na.rm = TRUE)))
            } else {
                return(NULL)
            }
        },


        config = function() {
            return(private$..config)
        },


        hash = function() {
            return(private$..hash)
        },


        short.hash = function() {
            return(substr(private$..hash, 1, 7))
        },


        replications = function() {
            return(private$..replications)
        },


        runs = function() {
            return(private$..runs)
        },


        warnings = function() {
            if(length(private$..runs)) {
                return(sum(sapply(private$..runs, function(run) {
                    if(length(run)) { return(length(run$warnings)) }
                })))
            } else {
                return(0)
            }
        },


        errors = function() {
            if(length(private$..runs)) {
                return(sum(sapply(private$..runs, function(run) {
                    if(length(run)) { return(length(run$errors)) }
                })))
            } else {
                return(0)
            }
        },


        completed = function() {
            if(length(private$..runs)) {
                return(sum(sapply(private$..runs, function(run) {
                    if(length(run)) { return(1) } else { return(0) }
                })))
            } else {
                return(0)
            }
        }
    )
)



# End of file.
