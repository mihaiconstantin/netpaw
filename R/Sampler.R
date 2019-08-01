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
#   - contains an abstract R6 class that defines a sampler and its        #
#     generation and a wrapper that starts a factory                      #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include Meta.R Option.R Model.R Data.R Factory.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Parent sampler class ----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Sampler <- R6::R6Class("Sampler",

    private = list(
        # Objects.
        ..options = NULL,
        ..model = NULL,
        ..data = NULL,


        # Primitives.
        ..resampling.attempts = 0,
        ..max.resampling.attempts = NULL,
        ..invariance.tolerance = NULL,
        ..resampling.succeeded = NULL,


        # Miscellaneous.
        ..verbose = NULL,
        ..progress.bar = NULL,

        # Support methods.


        # Check each column in the data for at least `invariance.tolerance` responses on a given category.
        ..determine.invariant.columns = function() {
            return(apply(private$..data$dataset, 2, is.invariant, private$..invariance.tolerance))
        },


        # Drop specified invariant columns from data.
        ..drop.invariant.columns = function() {
            return(private$..data$dataset[, !private$..determine.invariant.columns(),  drop = FALSE])
        },


        # Data resampling.
        ..resample = function(...) {
            # Announce that resampling must occur.
            if(private$..verbose) cat("Invariant nodes detected. Attempting resampling", private$..max.resampling.attempts, "times.\n") 

            # Start the resampling procedure.
            while(self$needs.resampling() && (private$..resampling.attempts < private$..max.resampling.attempts)) {
                # Tick the progress bar.
                if(private$..verbose) private$..progress.bar$tick()

                # Increment the resampling attempts.
                private$..resampling.attempts <- private$..resampling.attempts + 1

                # Call the sampler again.
                private$..data$dataset <- private$..sampler(...)
            }

            # Handle the progress bar completion to avoid printing issues (e.g., if finished earlier than expected ticks, mark it as terminated manually).
            if(!private$..progress.bar$finished) private$..progress.bar$terminate() 

            # Evaluate the resampling outcome.
            if(self$needs.resampling()) {
                # Indicate failure.
                private$..resampling.succeeded <- FALSE

                # Drop invariant columns.
                private$..data$dataset <- private$..drop.invariant.columns()
            } else {
                # Indicate success.
                private$..resampling.succeeded <- TRUE
            }

            # Inform about the resampling conclusion.
            if(private$..verbose) cat(ifelse(private$..resampling.succeeded, "Resampling succeeded.", "Resampling failed."), "\n")
        },


        # Hooks.
        ..before = function() { invisible() },
        ..after = function() { invisible() },


        # Boilerplate.
        ..boot = function(model, max.resampling.attempts, invariance.tolerance, verbose) {
            # Type check and assertions.
            assert("Model" %in% class(model), ..ERRORS..$incorrect.object.type)
            assert(max.resampling.attempts > 0, "Argument `max.resampling.attempts` must be grater than 0.")

            # Set the injected model.
            private$..model <- model

            # Set the primitive fields.
            private$..max.resampling.attempts <- max.resampling.attempts
            private$..invariance.tolerance <- invariance.tolerance

            # Store the verbose setting and initiate the progress bar.
            private$..verbose = verbose
            private$..progress.bar = progress::progress_bar$new(total = max.resampling.attempts, format = "[:bar] attempt :current of :total (:elapsed)", clear = FALSE)

            # Prepare the Option object and set the meta field.
            private$..options <- Option$new(meta = Meta$new(type = class(self)[1]))

            # Set the values field on the options at runtime.
            patch.function.within.environment("..sampler", private, "private$..options$values <- combine.arguments(private$..sampler, as.list(match.call())[-1])")
        },


        # Data sampling.
        ..sample = function(...) {
            # Run before the generator.
            private$..before()

            # Sample the data.
            private$..data <- Data$new(dataset = private$..sampler(...))

            # Determine if resampling is needed.
            if(self$needs.resampling()) private$..resample(...)

            # Run after the generator.
            private$..after()
        },


        # Data sampler.
        ..sampler = function(...) {
            stop(..ERRORS..$non.instantiable.class)
        }
    ),


    public = list(
        # Constructor.
        initialize = function(model, ..., max.resampling.attempts = 10, invariance.tolerance = 1, verbose = FALSE) {
            # Boot.
            private$..boot(model, max.resampling.attempts, invariance.tolerance, verbose)

            # Generate.
            private$..sample(...)
        },


        # Determine if resampling is needed.
        needs.resampling = function() {
            # Check whether there is at least one invariant column.
            columns.are.invariant <- (sum(private$..determine.invariant.columns()) > 0)

            # Resampling should occur if there is at least one invariant column.
            return(columns.are.invariant)
        }
    ),


    active = list(
        options = function() {
            return(private$..options)
        },


        model = function() {
            return(private$..model)
        },


        data = function() {
            return(private$..data)
        },


        resampling.attempts = function() {
            return(private$..resampling.attempts)
        },


        max.resampling.attempts = function() {
            return(private$..max.resampling.attempts)
        },


        invariance.tolerance = function() {
            return(private$..invariance.tolerance)
        },


        resampling.succeeded = function() {
            return(ifelse(is.null(private$..resampling.succeeded), TRUE, private$..resampling.succeeded))
        },


        rows = function() {
            return(nrow(private$..data$dataset))
        },


        cols = function() {
            return(ncol(private$..data$dataset))
        },


        item.steps = function() {
            return(sort(unique(c(private$..data$dataset))))
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Storage for keeping track of supported samplers -------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Sampler$..ALIASES.. <- list()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Exported wrapper for generating data ------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
generate.data <- function(sampler.type, ...) {
    # Make sure the sampler type requested is known.
    if(!sampler.type %in% names(Sampler$..ALIASES..)) {
        stop(..ERRORS..$unsupported.type)
    }

    # Match the pretty names to the blueprints.
    blueprint <- Sampler$..ALIASES..[[sampler.type]]$class

    # Start the factory.
    sampler.factory <- Factory$new(blueprint, ...)

    return(sampler.factory)
}



# End of file.
