# In this file we are generating UNDIREGTED & UNWEIGHTED GRAPHS of various graphs.



graph.random <- function(nodes, p) {
    # Graph.
    graph <- as.matrix(igraph::get.adjacency(igraph::erdos.renyi.game(nodes, p)))
    
    # Return graph.
    return(graph)
}



graph.small.world <- function(nodes, neighborhood, p) {
    # Graph.
    graph <- as.matrix(igraph::get.adjacency(igraph::sample_smallworld(1, nodes, neighborhood, p)))
    
    # Return graph.
    return(graph)
}



graph.scale.free <- function(nodes, attachment, edges) {
    # Graph.
    graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(nodes, power = attachment, m = edges, directed = F)))

    # Return graph.
    return(graph)
}



#' @title Generate an undirected unweighted graph.
#' @export
get.graph <- function(type, nodes, ..., positive.edge.ratio = 1) {
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
    
    # Determine the positive edge ratio.
    number.parameters = (nodes * (nodes - 1)) / 2
    positive.ratio <- sample(c(-1, 1), number.parameters, TRUE, prob = c(1 - positive.edge.ratio, positive.edge.ratio))
    
    # Apply the positive edge ration.
    graph[upper.tri(graph)] <- graph[upper.tri(graph)] * positive.ratio
    graph[lower.tri(graph)] <- t(graph)[lower.tri(graph)]
    
    return(graph)
}
