# In this file we are exporting wrappers around the registered UNWEIGHTED GRAPHS.



# SCOPE: not meant to be modified, unless bug fixes or improvements.



# Exported wrappers -------------------------------------------------------



#' @title Generate an undirected unweighted graph of specified type.
#' @export
gen.graph <- function(graph.type, ...) {
    # Check if the requested graph is supported.
    if(!(graph.type %in% GRAPHS$supported)) {
        stop("Unsupported graph type. Please request it at `m.a.constantin@uvt.nl`.")
    }
    
    # Capture the `...` argument as a list.
    . <- list(...)
    
    # Check if the required arguments of the graph generator are present and respect the imposed constrains.
    if(!check.arguments(GRAPHS[[graph.type]][["args"]], .)) {
        stop("Non-conformable argument(s) provided. See the documentation.")
    }
    
    # Call the graph generator.
    graph <- do.call(GRAPHS[[graph.type]]$generator, .)
    
    # Set the name and the generation options.
    result <- list(
        type = graph.type,
        graph = graph,
        generation.options = .
    )

    # Set the class of the result.
    class(result) <- c('np.graph', 'list')
    
    return(result)
}



# Object methods ----------------------------------------------------------



# Print the graph.
print.np.graph <- function(object, details = TRUE, graph = TRUE, ...) {
    # Details about the graph.
    if (details) {
        cat("\n")
        cat(crayon::black$bgGreen$bold("Graph details:"))
        cat("\n")
        cat(crayon::silver("  - class(es):", paste(shQuote(class(object)), collapse = ", ")))
        cat("\n")
        cat("  - type:", shQuote(crayon::yellow(object$type)))
        cat("\n")
        cat("  - dimensions:", paste(dim(object$graph), collapse = "x"))
        cat("\n")
        cat("  - density:", round(get.graph.density(object$graph), 3))
        cat("\n")
        cat("  - generation options:", paste(object$generation.options, crayon::yellow(paste("(", names(unlist(object$generation.options)), ")", sep = "")), collapse = crayon::silver(" | ")))
        cat("\n")      
    }

    # The graph matrix.
    if (graph) {
        cat("\n")
        cat(crayon::black$bgGreen$bold("Graph upper triangle:"))
        cat("\n\n")
        print(object$graph[upper.tri(object$graph)])
        cat("\n")

        # If the graph is directed also plot the lower triangle.
        if(!is.null(object$generation.options$directed) && object$generation.options$directed == TRUE) {
            cat(crayon::black$bgGreen$bold("Graph lower triangle:"))
            cat("\n\n")
            print(object$graph[lower.tri(object$graph)])
            cat("\n")
        }
    }
}



# Plot the graph.
plot.np.graph <- function(object, ...) {
    qgraph::qgraph(object$graph, ..., layout = "circle", edge.width = 1.5, title = paste("True model graph (", object$type, ")", sep = ""))
}
