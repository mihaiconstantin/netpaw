# In this file we are generating UNDIREGTED & UNWEIGHTED GRAPHS for various architectures. 
# All functions must return an adjency matrix.



#' @title .
#' @export
architecture_random <- function(nodes, ...) {
	# Graph.
	graph <- as.matrix(igraph::get.adjacency(igraph::erdos.renyi.game(nodes, ...)))
	
	# Return graph.
	return(graph)
}



#' @title .
#' @export
architecture_small_world <- function(nodes, ...) {
	# Graph.
	graph <- as.matrix(igraph::get.adjacency(igraph::sample_smallworld(1, nodes, ...)))
	
	# Return graph.
	return(graph)
}



#' @title .
#' @export
architecture_scale_free <- function(nodes, ...) {
	# Graph.
	graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(nodes, ..., directed = F)))

	# Return graph.
	return(graph)
}
