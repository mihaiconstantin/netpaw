# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                                                                                         #
# In this file we are registering UNWEIGHTED GRAPHS.                                                                                      #
#                                                                                                                                         #
# The structure of the file is as follows:                                                                                                #
#   a. starts with a generic prototype of what a graph represents -> aka parent class (i.e., `Graph` in `001_Graph.R`)                    #
#   b. specific graph implementations inherit and respect the parent -> aka child class (e.g., `RandomGraph` in `002_register_graphs.R`)  #
#   c. a factory abstracts away the generation of specific graph implementation -> aka factory class (i.e., `GraphFactory`)               #
#   d. a wrapper around the factory allows users to generate graphs -> aka exported wrapper (i.e., `gen.graph`)                           #
#                                                                                                                                         #
# Note for adding new graphs:                                                                                                             #
#   1. add a new graph child class (i.e., see point b.)                                                                                   #
#   2. overwrite the `generator` public method of parent class to return a matrix                                                         #
#       a. on the top of the generator add the following line of code to ensure pretty argument names                                     #
#           -> private$..options <- as.list(match.call())[-1]                                                                             #
#   3. register the graph name and associated implementation under `Graph$..ALIASES...`                                                   #
#   4. add example of named arguments so the new implementation can be automatically tested                                               #
#   5. run the tests                                                                                                                      #
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
