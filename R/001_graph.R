# In this file we are registering UNDIRECTED UNWEIGHTED GRAPHS.



# Graphs ------------------------------------------------------------------



# Random graph.
GRAPHS$random <- list(
    # The name of the graph.
    name = "random",

    # The arguments with some predefined constraints. 
    args = list(
        nodes = list(
            name = "nodes",
            type = "int",
            range = c(1, 100)
        ),
        p = list(
            name = "p",
            type = "double",
            range = c(0, 1)
        )
    ),

    # The graph generator.
    generator = function(nodes, p) {
        # Generate the graph.
        graph <- as.matrix(igraph::get.adjacency(igraph::erdos.renyi.game(nodes, p)))
        
        return(graph)
    }
)



# Smallworld graph.
GRAPHS$smallworld <- list(
    # The name of the graph.
    name = "smallworld",

    # The arguments with some predefined constraints.
    args = list(
        nodes = list(
            name = "nodes",
            type = "int",
            range = c(1, 100)
        ),
        neighborhood = list(
            name = "neighborhood",
            type = "int",
            range = c(0, 3)
        ),
        p.rewire = list(
            name = "p.rewire",
            type = "double",
            range = c(0, 1)
        )
    ),

    # The graph generator.
    generator = function(nodes, neighborhood, p.rewire) {
        # Generate the graph.
        graph <- as.matrix(igraph::get.adjacency(igraph::sample_smallworld(1, nodes, neighborhood, p.rewire)))

        return(graph)
    }
)



# Scalefree graph.
GRAPHS$scalefree <- list(
    # The name of the graph.
    name = "scalefree",

    # The arguments with some predefined constraints.
    args = list(
        nodes = list(
            name = "nodes",
            type = "int",
            range = c(1, 100)
        ),
        attachment = list(
            name = "attachment",
            type = "int",
            range = c(0, 3)
        ),
        edges = list(
            name = "edges",
            type = "int",
            range = c(0, 3)
        )
    ),

    # The graph generator.
    generator = function(nodes, attachment, edges) {
        # Generate the graph.
        graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(nodes, power = attachment, m = edges, directed = F)))
        
        return(graph)
    }
)



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
    class(result) <- c('npgraph', 'list')
    
    return(result)
}



# Object methods ----------------------------------------------------------



# Print the graph.
print.npgraph <- function(object, details = TRUE, graph = TRUE, ...) {
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
        cat(crayon::black$bgGreen$bold("Graph (i.e., upper triangle):"))
        cat("\n\n")
        print(object$graph[upper.tri(object$graph)])
        cat("\n")
    }
}



# Plot the graph.
plot.npgraph <- function(object, ...) {
    qgraph::qgraph(object$graph, ..., layout = "circle", edge.width = 1.5, title = paste("True model graph (", object$type, ")", sep = ""))
}
