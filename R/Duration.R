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
#   - contains a class that defines a duration (i.e., for a simulation    #
#     or a simulator object)                                              #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include SimulationRun.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Duration class ----------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Duration <- R6::R6Class("Duration",

    private = list(
        ..simulation.samples = NULL,
        ..run.samples = NULL,
        ..replications = NULL,
        ..durations = NULL,
        ..duration = NULL,


        # Boilerplate.
        ..boot = function(simulation.samples, run.samples, replications) {
            # Set the sample-related fields.
            private$..simulation.samples <- simulation.samples
            private$..run.samples <- run.samples
            private$..replications <- replications
        },


        # Estimate the duration for an arbitrary simulation with an arbitrary number of replications.
        ..estimate.simulation.duration = function(simulation, samples, replications = NULL) {
            # Create duration storage for a simulation.
            duration <- c()

            # # Get `sample` duration samples. 
            for (i in 1:samples) {
                duration <- c(duration, SimulationRun$new(simulation$config)$duration)
            }

            # Decide how many replications to consider in order to provide an estimate.
            if(is.null(replications)) {
                replications <- simulation$replications
            }

            # What is the duration for the amount of replications indicated?
            duration <- mean(duration) * replications

            return(duration)
        },


        # Estimate the duration for a specified simulator.
        ..estimate.simulator.duration = function(simulator, ...) {
            # How many and what simulations should we evaluate to get an idea about the duration?
            subset <- sample(1:simulator$total, private$..simulation.samples, ...)

            # Get the durations for the subset of simulations determined.
            for (i in subset) {
                # Get the simulation duration.
                simulation.duration <- private$..estimate.simulation.duration(simulator$simulations[[i]], samples = private$..run.samples, replications = private$..replications)

                # Store the simulation durations.
                private$..durations <- c(private$..durations, simulation.duration)
            }

            # Compute the actual duration.
            private$..duration <- mean(private$..durations) * simulator$total
        }
    ),


    public = list(
        # Constructor
        initialize = function(simulator, simulation.samples = 100, run.samples = 10, replications = NULL, ...) {
            # Type check.
            assert("Simulator" %in% class(simulator), ..ERRORS..$incorrect.object.type)

            # Boot.
            private$..boot(simulation.samples, run.samples, replications)

            # Estimate the duration.
            private$..estimate.simulator.duration(simulator, ...)
        },


        # Plot method.
        plot = function() {
            # Create the ggplot.
            ggplot2::ggplot(data.frame(durations = private$..durations), mapping = ggplot2::aes(x = "", y = durations)) +
                ggplot2::geom_boxplot() +
                ggplot2::geom_jitter(position = ggplot2::position_jitter(.2), alpha = .3) +
                ggplot2::stat_summary(fun.y = mean, geom = "point", shape = 23, size = 3, fill = "blue") +
                ggplot2::theme_bw() +
                ggplot2::theme(axis.ticks.x = ggplot2::element_blank()) +
                ggplot2::labs(x = paste("Average replication time based on", private$..simulation.samples, "simulation(s) and", private$..run.samples, "replication(s) per simulation"), y = "Seconds") +
                ggplot2::annotate("text", x = 1.5, y = max(private$..durations), label = paste(round(self$hours, 2), "hours", "\n", "(ETA)"))
        }
    ),


    active = list(
        durations = function() {
            return(private$..durations)
        },


        seconds = function() {
            return(private$..duration)
        },


        minutes = function() {
            return(private$..duration / 60)
        },


        hours = function() {
            return(private$..duration / 3600)
        }
    )
)



# End of file.
