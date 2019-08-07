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
#   - this file contains a simple factory used to generate an arbitrary   #
#     amount of R6 objects                                                #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Factory of R6 objects ---------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Factory <- R6::R6Class("Factory",

    private = list(
        ..blueprint = NULL,
        ..amount = NULL,
        ..parallel = NULL,
        ..time = NULL,
        ..warehouse = list(),


        # Run in a single-threaded fashion.
        ..single.thread.implementation = function(...) {
            for(i in 1:private$..amount) {
                private$..warehouse[[i]] <- private$..blueprint$new(...)
            }
        },


        # Run in a multi-threaded fashion.
        ..multi.thread.implementation = function(...) {
            # Create the cluster.
            cluster <- parallel::makeCluster(parallel::detectCores() -  1)

            # Configure the cluster.
            doParallel::registerDoParallel(cluster)
            
            # Stop the cluster no matter what.
            on.exit(parallel::stopCluster(cluster))

            # Perform parallelized computations.
            private$..warehouse <- foreach::foreach(1:private$..amount, .export = "private") %dopar% {
                private$..blueprint$new(...)
            }
        },


        # Select which implementation to use and run.
        ..run = function(...) {
            # Start a counter.
            start <- Sys.time()

            # Select the approach.
            if(private$..parallel && (private$..amount > 1)) { 
                private$..multi.thread.implementation(...)
            } else {
                private$..single.thread.implementation(...)
            }

            # Stop and store the time.
            private$..time <- Sys.time() - start
        }
    ),


    public = list(
        # Constructor.
        initialize = function(blueprint, ..., amount = 1, parallel = FALSE) {
            # Enforce a type check.
            assert(class(blueprint) == "R6ClassGenerator", ..ERRORS..$incorrect.object.type)

            # Set the fields.
            private$..blueprint <- blueprint
            private$..amount <- amount
            private$..parallel <- parallel

            # Run the factory.
            private$..run(...)
        },


        # Randomly select an arbitrary amount of objects from the warehouse.
        sample = function(amount, with.replacement = FALSE) {
            return(sample(private$..warehouse, amount, replace = with.replacement))
        }
    ),


    active = list(
        warehouse = function() {
            return(private$..warehouse)
        },


        size = function() {
            return(length(private$..warehouse))
        },


        first = function() {
            return(private$..warehouse[[1]])
        },


        time = function() {
            return(private$..time)
        },


        parallel = function() {
            return(private$..parallel)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrapper for generating R6 objects ------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
make <- function(type, ...) {
    # Issue the order.
    blueprint <- eval(as.symbol(type))

    # Start the factory.
    factory <- Factory$new(blueprint, ...)

    return(factory)
}



# End of file.
