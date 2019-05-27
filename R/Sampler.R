# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                             _                                                                           #
#                                                            | |                                                                          #
#                                                _ __    ___ | |_  _ __    __ _ __      __                                                #
#                                               | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /                                                #
#                                               | | | ||  __/| |_ | |_) || (_| | \ V  V /                                                 #
#                                               |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                                                  #
#                                                                 | |                                                                     #
#                                                                 |_|                                                                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                                                                                         #
# File contributors:                                                                                                                      #
#   - M.A. Constantin                                                                                                                     #
#                                                                                                                                         #
# File description:                                                                                                                       #
#   - this file contains an abstract class used to define the structure of a data sampler                                                 #
#                                                                                                                                         #
# Classes/ functions/ methods:                                                                                                            #
#   - Sampler (R6 class)                                                                                                                  #
#   - gen.data (function)                                                                                                                 #
#                                                                                                                                         #
# Additional information:                                                                                                                 #
#   - architecture:                                                                                                                       #
#       - abstract`Sampler` class (i.e., a very general definition of a data sampler)                                                     #
#       - specific implementations inherit from `Sampler` and override the `sampler` method (e.g., `IsingSampler` class)                  #
#   - wrapper:                                                                                                                            #
#       - `gen.data` makes use of an internal `Factory` class to generate data                                                            #
#       - the factory uses aliases registered within the `Sampler$..ALIASES..` list                                                       #
#       - each alias maps to a class that explicitly inherits from `Sampler` (i.e., specific implementation)                              #
#   - implementing samplers:                                                                                                              #
#       - see file `register_sampler.R` for details on how to inherit from `Sampler` and implement specific data samplers                 #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Parent sampler class ----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Sampler <- R6::R6Class("Sampler",
    
    private = list(
        ..timestamp = NULL,
        ..options = NULL,
        ..succeeded = NULL,
        ..tolerance = NULL,
        ..max.attempts = NULL,
        ..attempts = 1,
        ..resampler.calls = 0,


        # Determine the invariant columns in the sampled data.
        ..determine.invariant.columns = function() {
            # Check each column in the data for at least `tolerance` responses on a given category.
            return(apply(self$data, 2, is.invariant, private$..tolerance))
        },


        # Drop the invariant columns.
        ..drop.invariant.columns = function(invariant.columns) {
            # Drop specified invariant columns from data.
            return(self$data[, !invariant.columns,  drop = FALSE])
        }
    ),


    public = list(
        type = NULL,
        data = NULL,
        model = NULL,


        # Constructor.
        initialize = function(model, ..., tolerance = 1, max.attempts = 10, progress = TRUE, feedback = TRUE) {
            # Record the timestamp.
            private$..timestamp <- Sys.time()

            # Enforce type` Model` since R misses strong typed arguments.
            assert.condition("Model" %in% class(model), ..ERRORS..$incorrect.object.type)

            # Patch the sampler to store the options used during the sampler call.
            patch.function.within.environment("sampler", self, quote(private$..options <- combine.arguments(self$sampler, as.list(match.call())[-1])))

            # Record the tolerance.
            private$..tolerance = tolerance

            # Set the model.
            self$model <- model
            
            # Set the type of graph based on the class name.
            self$type <- class(self)[1]

            # Generate the graph and update the public member `graph`.
            self$data <- self$sampler(...)

            # If necessary, attempt resampling.
            self$resample(max.attempts = max.attempts, progress = progress, feedback = feedback)
        },


        # Data sampler.
        sampler = function(...) {
            stop(..ERRORS..$non.instantiable.class)
        },


        # Attempt resampling.
        resample = function(max.attempts, progress = TRUE, feedback = TRUE) {
            # Keep track of how many times the resampler was called.
            private$..resampler.calls <- private$..resampler.calls + 1

            # Make sure that the `max.attempts` is greater that 0.
            assert.condition(max.attempts > 0, "Argument `max.attempts` must be grater than 0.")

            # Record the max allowed resampling attempts on first run.
            if(is.null(private$..max.attempts)) {
                private$..max.attempts <- max.attempts
            }
            
            # Resampling is needed.
            if(self$should.resample) {
                # If the number of attempts already equals the max, we already ran out of attempts.
                if(!(private$..attempts < private$..max.attempts)) private$..max.attempts = max.attempts + private$..attempts
                
                # Some user feedback.
                if(feedback) cat("Detected invariant columns. Attempting resampling ", crayon::bold$underline(private$..max.attempts, "times"), ".\n", sep = "")

                # Start the progress bar.
                if(progress) progress.bar <- progress::progress_bar$new(format = "Resampling [:bar] :percent in :elapsed", total = private$..max.attempts - 1, clear = TRUE)

                # Attempt to get good data, but stop after a number of attempts.
                while(self$should.resample && (private$..attempts < private$..max.attempts)) {
                    # Mark that a new resampling attempt has been made.
                    private$..attempts <- private$..attempts + 1

                    # Attempt to sample data again.
                    self$data <- do.call(self$sampler, private$..options)

                    # Update the progress bar.
                    if(progress) progress.bar$tick()
                }

                # Just in case the resampling succeeded earlier that the max allowed attempts, close the progress bar.
                if(progress) progress.bar$terminate()

                # Get the invariant columns after exhausting all attempts.
                invariant.columns <- private$..determine.invariant.columns()

                # If there are still invariant columns after exhausting all resampling attempts then drop the invariant nodes.
                if((sum(invariant.columns) > 0) && (private$..attempts == private$..max.attempts)) {
                    # Mark the status of the data resampling. 
                    private$..succeeded <- FALSE

                    # Drop the invariant nodes.
                    self$data <- private$..drop.invariant.columns(invariant.columns)

                    # User feedback on failure.
                    if(feedback) cat("Failed after", private$..attempts, "attempts. Dropped", sum(invariant.columns) ,"invariant columns.\n")

                } else {
                    # Mark the status of the data resampling. 
                    private$..succeeded <- TRUE

                    # User feedback on success. 
                    if(feedback) cat("Succeeded on resampling attempt ", private$..attempts, ".\n", sep = "")
                }

            # Resampling is not needed.
            } else {
                # Record the positive status.
                private$..succeeded <- TRUE
                
                # Some user feedback.
                if(feedback & (private$..resampler.calls > 1)) cat("Resampling not necessary.\n")
            }
        },


        print = function(with.details = TRUE, with.data = TRUE, with.object = TRUE, ...) {
            # Details about the `R6` sampler object.
            if(with.object) {
                cat("\n")
                cat(crayon::black$bgGreen$bold("Object details:"))
                cat("\n")
                cat(crayon::silver(format(self, ...)), sep = "\n")

                # Details about the `R6` model object.
                print(self$model, with.details = FALSE, with.model = FALSE)    
            }

            # Details about the graph, model, and data.
            if(with.details) {
                # Details about the model.
                print(self$model, with.model = FALSE, with.object = FALSE)

                # Details about the data.
                cat("\n")
                cat(crayon::black$bgGreen$bold("Data details:"))
                cat("\n")
                cat(crayon::silver("  - class(es):", paste(shQuote(class(self)), collapse = ", ")))
                cat("\n")
                cat("  - generation status:", ifelse(private$..succeeded, crayon::green("succeeded"), crayon::red("failed")))
                cat("\n")
                cat("  - dimensions:", paste(self$rows, "by", self$cols))
                cat("\n")
                cat("  - item steps:", paste(self$item.steps, collapse = crayon::silver(" | ")))
                cat("\n")
                cat("  - resampling attempts:", private$..attempts, "/", private$..max.attempts)
                cat("\n")
                cat("  - sampling options:", paste(private$..options, crayon::yellow(paste("(", names(unlist(private$..options)), ")", sep = "")), collapse = crayon::silver(" | ")))
                cat("\n")
                cat("  - tolerance:", private$..tolerance)
                cat("\n")
                cat("  - resampler calls:", private$..resampler.calls)
                cat("\n")
            }

            # The model weights, graph matrix and data head.
            if(with.data) {
                # The graph matrix (i.e., only upper triangle).
                print(self$model, with.details = FALSE, with.object = FALSE)
                
                # The data matrix.
                cat(crayon::black$bgGreen$bold("Data preview:"))
                cat("\n\n")
                print(head(self$data, ...))
                cat("\n")
                cat(crayon::green(". . ."))
                cat("\n")
            }
        }
    ),


    active = list(
        options = function() {
            return(private$..options)
        },


        succeeded = function() {
            return(private$..succeeded)
        },


        tolerance = function() {
            return(private$..tolerance)
        },


        max.attempts = function() {
            return(private$..max.attempts)
        },


        attempts = function() {
            return(private$..attempts)
        },


        rows = function() {
            return(nrow(self$data))
        },


        cols = function() {
            return(ncol(self$data))
        },


        item.steps = function() {
            return(sort(unique(c(self$data))))
        },


        attempted.resampling = function() {
            return(private$..attempts > 1)
        },


        should.resample = function() {
            # TODO: we assume that `weights` element exists of `model` field! Come back at this later.
            # Check that the data have the required number of columns.
            columns.were.dropped <- ncol(self$data) != ncol(self$model$model$weights)

            # Check whether there is at least one invariant column.
            columns.are.invariant <- (sum(private$..determine.invariant.columns()) > 0)

            # Resampling should occur if either of the two conditions is true.
            return(columns.were.dropped || columns.are.invariant)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Storage for keeping track of supported models ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Sampler$..ALIASES.. <- list()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Exported wrapper for generating models ----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
gen.data <- function(sampler.type, ...) {
    # Make sure the sampler type requests is known.    
    if(!sampler.type %in% names(Sampler$..ALIASES..)) {
        stop(..ERRORS..$unsupported.type)
    }
    
    # Match the pretty names to the samplers.
    sampler <- Sampler$..ALIASES..[[sampler.type]]$class

    # Start the factory.
    data.factory <- Factory$new(sampler, ...)

    return(data.factory)
}
