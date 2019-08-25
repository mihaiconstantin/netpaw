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
        ..config = NULL,
        ..hash = NULL,
        ..replications = NULL,
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
        perform = function(vary.generator = FALSE) {
            # If no replication ran has been performed, run for the first time. 
            if(!length(private$..runs)) {
                # Perform the first replication.
                private$..runs[[1]] <- SimulationRun$new(private$..config)
            }

            # Decide on what generator to use (i.e., reuse the first ever created or vary it for each run).
            if(!vary.generator) { generator <- private$..runs[[1]]$generator } else { generator <- NULL }

            # Run and replicate the remaining.
            for (i in 2:private$..replications) {
                # Perform the remainder of the replications.
                private$..runs[[i]] <- SimulationRun$new(config = private$..config, generator = generator)
            }
        }
    ),


    active = list(
        config = function() {
            return(private$..config)
        },


        hash = function() {
            return(private$..hash)
        },


        replications = function() {
            return(private$..replications)
        },


        runs = function() {
            return(private$..runs)
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
