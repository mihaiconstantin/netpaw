# In this file we are generating UNDIREGTED & UNWEIGHTED GRAPHS of various graphs.



# Graph types -------------------------------------------------------------
graph.random <- function(nodes, p) {
    # Graph.
    graph <- as.matrix(igraph::get.adjacency(igraph::erdos.renyi.game(nodes, p)))
    
    # Return graph.
    return(list(
        graph = graph,
        type = "random",
        options = list(
            p = p
        )
    ))
}



graph.small.world <- function(nodes, neighborhood, p) {
    # Graph.
    graph <- as.matrix(igraph::get.adjacency(igraph::sample_smallworld(1, nodes, neighborhood, p)))
    
    # Return graph.
    return(list(
        graph = graph,
        type = "smallworld",
        options = list(
            p = p,
            neighborhood = neighborhood
        )
    ))
}



graph.scale.free <- function(nodes, attachment, edges) {
    # Graph.
    graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(nodes, power = attachment, m = edges, directed = F)))

    # Return graph.
    # Return graph.
    return(list(
        graph = graph,
        type = "scalefree",
        options = list(
            attachment = attachment,
            edges = edges
        )
    ))
}



# Exported wrappers -------------------------------------------------------
#' @title Generate an undirected unweighted graph.
#' @export
get.graph <- function(type, nodes, ...) {
    # Capture the dot arguments.
    . <- list(...)
    
    # Make sure that the dots are not empty.
    if(length(.) == 0) {
        stop("Invalid `...` arguments. Please check the documentation.")
    }
    
    # Providing the desired graph.
    if(type == "random") {
        if(is.null(.[["p"]])) stop("Missing expected argument(s). See the documentation.")
        graph = graph.random(nodes, .[["p"]])

    } else if(type == "smallworld") {
        if(is.null(.[["neighborhood"]]) || is.null(.[["p"]])) stop("Missing expected argument(s). See the documentation.")
        graph = graph.small.world(nodes, .[["neighborhood"]], .[["p"]])

    } else if(type == "scalefree") {
        if(is.null(.[["attachment"]]) || is.null(.[["edges"]])) stop("Missing expected argument(s). See the documentation.")
        graph = graph.scale.free(nodes, .[["attachment"]], .[["edges"]])
    
    } else {
        stop("Unsupported graph type. Please request it at `m.a.constantin@uvt.nl`")
    }
    
    # Set the class of the result.
    class(graph) <- c('netpowerGraph', 'list')
    
    return(graph)
}



# Object methods ----------------------------------------------------------
print.netpowerGraph <- function(object, details = TRUE, graph = TRUE, ...) {
    # Details about the graph.
    if (details) {
        cat("\n")
        cat("Graph details:")
        cat("\n")
        cat("  - class(es):", paste(shQuote(class(object)), collapse = ", "))
        cat("\n")
        cat("  - type:", shQuote(object$type))
        cat("\n")
        cat("  - options:", paste(shQuote(names(unlist(object$options))), object$options, sep = " = ", collapse = " | "))
        cat("\n")
        cat("  - dimensions:", paste(dim(object$graph), collapse = "x"))
        cat("\n")
        cat("  - density:", round(get.graph.density(object$graph), 3))
        cat("\n")      
    }

    # The graph matrix.
    if (graph) {
        cat("\n")
        cat("Graph matrix:")
        cat("\n\n")
        print(object$graph)
        cat("\n")
    }
}



plot.netpowerGraph <- function(object, ...) {    
    qgraph::qgraph(object$graph, ..., layout = "circle", edge.width = 1.5, title = "Unweighted graph")
}
