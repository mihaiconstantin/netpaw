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
#   - this file contains an abstract class used to define the structure of a model object                                                 #
#                                                                                                                                         #
# Classes/ functions/ methods:                                                                                                            #
#   - Model (R6 class)                                                                                                                    #
#   - ModelCrossSectional (R6 class)                                                                                                      #
#   - ModelTimeSeries (R6 class)                                                                                                          #
#   - gen.model (function)                                                                                                                #
#                                                                                                                                         #
# Additional information:                                                                                                                 #
#   - architecture:                                                                                                                       #
#       - abstract`Model` class (i.e., a very general definition of a model)                                                              #
#       - more specialized--but still abstract---definitions of a model inherit from the `Model` class (e.g., `ModelCrossSectional`)      #
#       - specific implementations inherit from one of the specialized definitions (i.e., `ModelCrossSectional` or `ModelTimeSeries`)     #
#         and override the `generator` method (e.g., `IsingModel` class)                                                                  #
#   - wrapper:                                                                                                                            #
#       - `gen.model` makes use of an internal `Factory` class to generate models                                                         #
#       - the factory uses aliases registered within the `Model$..ALIASES..` list                                                         #
#       - each alias maps to a class that explicitly inherits from `Model` (i.e., specific implementation)                                #
#   - implementing models:                                                                                                                #
#       - see file `register_models.R` for details on how to inherit from `Model` and implement specific models                           #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Parent model class ------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Model <- R6::R6Class("Model",
    
    private = list(
        ..timestamp = NULL,
        ..options = NULL,


        # Match arguments intelligently even when names are missing.
        ..match.arguments = function() {
            # Unlock the binding in the `self` environment.
            unlockBinding("generator", self)
            
            # Alter the function at runtime.
            body(self$generator) <- patch.function(
                self$generator, 
                quote(private$..options <- combine.arguments(self$generator, as.list(match.call())[-1]))
            )
            
            # Lock the binding in the `self` environment.
            lockBinding("generator", self)
        }
    ),


    public = list(
        # Regular public fields.
        type = NULL,
        model = NULL,


        # Constructor.
        initialize = function() {
            # Prevent the class to be instantiated.
            assert.condition(length(grep("Model", class(self))) > 1, ..ERRORS..$non.instantiable.class)

            # Record the timestamp.
            private$..timestamp = Sys.time()

            # Patch the generator to store the options used during the generator call.
            private$..match.arguments()

            # Set the type of model based on class name.
            self$type = class(self)[1]
        },


        # Model generator.
        generator = function() {
            # Make sure that down the inheritance chain someone overwrites the generator otherwise stop.
            stop(..ERRORS..$non.instantiable.class)
        }
    ),


    active = list(
        options = function() {
            return(private$..options)           
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Storage for keeping track of supported models ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Model$..ALIASES.. <- list()



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Specialized cross-sectional model class ---------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

ModelCrossSectional <- R6::R6Class("ModelCrossSectional",
    inherit = Model,


    private = list(
        # Get a vector of negative and positive integers (i.e., 1 and -1) matching a specified proportion of positive integers.
        ..sample.positive.parameter.ratio = function(flip = FALSE) {
            if(flip) {
                positive.ratio <- sample(c(-1, 1), self$number.parameters, TRUE, prob = c(private$..options$positive.edge.ratio, 1 - private$..options$positive.edge.ratio))
            } else {
                positive.ratio <- sample(c(-1, 1), self$number.parameters, TRUE, prob = c(1 - private$..options$positive.edge.ratio, private$..options$positive.edge.ratio))
            }

            return(positive.ratio)    
        }
    ),


    public = list(
        # Regular public fields.
        graph = NULL,


        initialize = function(graph, ...) {
            # Call the parent constructor.
            super$initialize()

            # Enforce a type R since R misses strong typed arguments.
            assert.condition("Graph" %in% class(graph), ..ERRORS..$incorrect.object.type)

            # Set the graph via dependency injection.
            self$graph <- graph

            # Generate the model.
            self$model <- self$generator(...)
        },


        print = function(with.details = TRUE, with.model = TRUE, with.object = TRUE, ...) {
            # Details about the `R6` model object.
            if(with.object) {
                cat("\n")
                cat(crayon::black$bgGreen$bold("Object details:"))
                cat("\n")
                cat(crayon::silver(format(self, ...)), sep = "\n")

                # Details about the `R6` graph object.
                print(self$graph, with.details = FALSE, with.graph = FALSE)    
            }
            
            # Details about the graph and model.
            if(with.details) {
                # Details about the graph.
                print(self$graph, with.graph = FALSE, with.object = FALSE)
                
                # Details about the model.
                cat("\n")
                cat(crayon::black$bgGreen$bold("Model details:"))
                cat("\n")
                cat(crayon::silver("  - class(es):", paste(shQuote(class(self)), collapse = ", ")))
                cat("\n")
                cat("  - type:", shQuote(crayon::yellow(self$type)))
                cat("\n")
                cat("  - mean absolute:", round(mean(abs(self$model$weights[upper.tri(self$model$weights)])), 3))
                cat("\n")
                cat("  - sd:", round(sd(self$model$weights[upper.tri(self$model$weights)]), 3))
                cat("\n")
                cat("  - range:", paste(round(c(min(self$model$weights), max(self$model$weights)), 3), crayon::yellow(c("(min)", "(max)")), collapse = crayon::silver(" | ")))
                cat("\n")
                cat("  - generation options:", paste(private$..options, crayon::yellow(paste("(", names(unlist(private$..options)), ")", sep = "")), collapse = crayon::silver(" | ")))
                cat("\n")
            }

            # The model weights and graph matrix.
            if (with.model) {
                # The graph matrix (i.e., only upper triangle).
                print(self$graph, with.details = FALSE, with.object = FALSE)
                
                # The weights matrix.
                cat(crayon::black$bgGreen$bold("Model weights upper triangle:"))
                cat("\n\n")
                print(self$model$weights[upper.tri(self$model$weights)], digits = 3)
                cat("\n")
                
                # If the graph is directed also plot the lower triangle.
                if(!is.null(self$graph$options$directed) && self$graph$options$directed == TRUE) {
                    cat(crayon::black$bgGreen$bold("Model weights lower triangle:"))
                    cat("\n\n")
                    print(self$model$weights[lower.tri(self$model$weights)], digits = 3)
                    cat("\n")
                }

                # The threshold vector if applicable.
                if(!is.null(self$model$thresholds)) {
                    cat(crayon::black$bgGreen$bold("Thresholds:"))
                    cat("\n\n")
                    print(self$model$thresholds, digits = 3)
                    cat("\n")
                }
            } 
        },


        plot = function(...) {
            # Store the `qgraph` objects to compute the average layout.
            qgraph.object.graph <- qgraph::qgraph(self$graph$graph, layout = "spring", DoNotPlot = TRUE, ...)
            qgraph.object.weights <- qgraph::qgraph(self$model$weights, layout = "spring", DoNotPlot = TRUE, ...)
            
            # Compute the average layout.
            average.layout <- qgraph::averageLayout(qgraph.object.graph, qgraph.object.weights)
            
            # Color the edges.
            colors <- matrix(NA, ncol(self$model$weights), nrow(self$model$weights))
            colors[self$model$weights > 0] <- ..GRAPHICS..$positive.edge.color
            colors[self$model$weights < 0] <- ..GRAPHICS..$negative.edge.color
            
            # Split the screen.
            par(mfrow = c(1, 2))
            
            # Plot the undirected, unweighted graph.
            plot(self$graph, layout = average.layout)
            
            # Plot the weights matrix (i.e., true model). 
            qgraph::qgraph(self$model$weights, layout = average.layout, edge.color = colors, title = paste("True model edge weights (", self$type, ")", sep = ""), ...)
            
            par(mfrow = c(1, 1))
        }
    ),


    active = list(
        # Compute the number of parameters based on the injected graph object.
        number.parameters = function() {
            number.parameters <- (self$graph$nodes * (self$graph$nodes - 1)) / 2
            
            return(number.parameters)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Specialized time-series model class -------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

ModelTimeSeries <- R6::R6Class("ModelTimeSeries",
    inherit = Model,


    private = list(),


    public = list(
        initialize = function(...) {
            # Call the parent constructor.
            super$initialize()

            # TODO: stop because not implemented.
            stop(..ERRORS..$not.implemented)

            # Generate the model.
            self$model <- self$generator(...)
        }
    ),


    active = list()
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Exported wrapper for generating models ----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
gen.model <- function(model.type, ...) {
    # Make sure the graph type requests is known.    
    if(!model.type %in% names(Model$..ALIASES..)) {
        stop(..ERRORS..$unsupported.type)
    }
    
    # Match the pretty names to the generators.
    generator <- Model$..ALIASES..[[model.type]]$class

    # Start the factory.
    model.factory <- Factory$new(generator, ...)

    return(model.factory)
}
