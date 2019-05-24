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
#   - this file contains an abstract class used to define the structure of a graph object                                                 #
#                                                                                                                                         #
# Classes/ functions/ methods:                                                                                                            #
#   - Graph (R6 class)                                                                                                                    #
#   - gen.graph (function)                                                                                                                #
#                                                                                                                                         #
# Additional information:                                                                                                                 #
#   - architecture:                                                                                                                       #
#       - abstract`Graph` class (i.e., a very general definition of a graph)                                                              #
#       - specific implementations inherit from `Graph` and override the `generator` method (e.g., `RandomGraph` class)                   #
#   - wrapper:                                                                                                                            #
#       - `gen.graph` makes use of an internal `Factory` class to generate graphs                                                         #
#       - the factory uses aliases registered within the `Graph$..ALIASES..` list                                                         #
#       - each alias maps to a class that explicitly inherits from `Graph` (i.e., specific implementation)                                #
#   - implementing graphs:                                                                                                                #
#       - see file `register_graphs.R` for details on how to inherit from `Graph` and implement specific graphs                           #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Parent graph class ------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Graph <- R6::R6Class("Graph",

    private = list(
        ..timestamp = NULL,
        ..options = NULL
    ),
    
    
    public = list(
        # Regular public fields.
        type = NULL,
        graph = NULL,
        
        
        initialize = function(...) {
            # Record the timestamp.
            private$..timestamp <- Sys.time()
            
            # Store the options intended for the generator.
            # This is a fail-safe in case I or you forget to add the match call to the overwritten generator.
            private$..options <- list(...)
            
            # Set the type of graph based on the class name.
            self$type <- class(self)[1]
            
            # Generate the graph and update the public member `graph`.
            self$graph <- self$generator(...)
        },
        
    
        generator = function(...) {
            stop("Unimplemented graph generator.")
        },


        print = function(with.details = TRUE, with.graph = TRUE, ...) {
            # Details about the `R6` object.
            cat("\n")
            cat(crayon::black$bgGreen$bold("Object details:"))
            cat("\n")
            cat(crayon::silver(format(self, ...)), sep = "\n")

            # Details about the graph.
            if (with.details) {
                cat("\n")
                cat(crayon::black$bgGreen$bold("Graph details:"))
                cat("\n")
                cat(crayon::silver("  - class(es):", paste(shQuote(class(self)), collapse = ", ")))
                cat("\n")
                cat("  - type:", shQuote(crayon::yellow(self$type)))
                cat("\n")
                cat("  - dimensions:", paste(dim(self$graph), collapse = "x"))
                cat("\n")
                cat("  - density:", round(self$density, 3))
                cat("\n")
                cat("  - generation options:", paste(self$options, crayon::yellow(paste("(", names(unlist(self$options)), ")", sep = "")), collapse = crayon::silver(" | ")))
                cat("\n")      
            }

            # The graph matrix.
            if (with.graph) {
                cat("\n")
                cat(crayon::black$bgGreen$bold("Graph upper triangle:"))
                cat("\n\n")
                print(self$graph[upper.tri(self$graph)])
                cat("\n")

                # If the graph is directed also plot the lower triangle.
                if(!is.null(self$options$directed) && self$options$directed == TRUE) {
                    cat(crayon::black$bgGreen$bold("Graph lower triangle:"))
                    cat("\n\n")
                    print(self$graph[lower.tri(self$graph)])
                    cat("\n")
                }
            }
        },
        
        
        plot = function(...) {
            qgraph::qgraph(self$graph, ..., layout = "circle", edge.width = 1.5, title = paste("True model graph (", self$type, ")", sep = ""))
        }
    ),


    active = list(
        options = function() {
            return(private$..options)           
        },


        nodes = function() {
            # Determine the dimensions.
            dimensions = dim(self$graph)
            
            # Check the dimensions.
            if(dimensions[1] != dimensions[2]) stop('Wrong type of input: the graph dimensions do not match.')
            
            # Return the number of nodes.
            return(dimensions[1])
        },


        density = function() {
            # Potential connections.
            potential = (self$nodes * (self$nodes - 1)) / 2
            
            # Actual connections.
            actual = sum(self$graph[upper.tri(self$graph)] != 0)
            
            # Density.
            density = actual / potential
            
            return(density)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Storage for keeping track of supported graphs ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Graph$..ALIASES.. <- list()


 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Graph factory class -----------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GraphFactory <- R6::R6Class("GraphFactory",
    private = list(
        ..warehouse = list()
    ),


    public = list(
        initialize = function(graph, ..., amount = 1) {
            stopifnot(class(graph) == "R6ClassGenerator")
            
            for(i in 1:amount) {
                private$..warehouse[[i]] <- graph$new(...)
            }
        }
    ),


    active = list(
        warehouse = function() {
            return(private$..warehouse)
        },


        size = function() {
            return(length(private$..warehouse))
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Exported wrapper for generating graphs ----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#' @export
gen.graph <- function(graph.type, ...) {
    # Make sure the graph type requests is known.    
    if(!graph.type %in% names(Graph$..ALIASES..)) {
        stop("Unsupported graph type. Please request it at `m.a.constantin@uvt.nl`.")
    }
    
    # Match the pretty names to the generators.
    generator <- Graph$..ALIASES..[[graph.type]]$class

    # Start the factory.
    graph.factory <- GraphFactory$new(generator, ...)

    return(graph.factory)
}
