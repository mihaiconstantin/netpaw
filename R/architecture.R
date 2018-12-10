# In this file we are generating UNDIREGTED & UNWEIGHTED GRAPHS for various architectures. 
# All functions must return an adjency matrix.



architecture.random <- function(nodes, p) {
	# Graph.
	graph <- as.matrix(igraph::get.adjacency(igraph::erdos.renyi.game(nodes, p)))
	
	# Return graph.
	return(graph)
}



architecture.small.world <- function(nodes, neighborhood, p) {
	# Graph.
	graph <- as.matrix(igraph::get.adjacency(igraph::sample_smallworld(1, nodes, neighborhood, p)))
	
	# Return graph.
	return(graph)
}



architecture.scale.free <- function(nodes, attachment, edges) {
	# Graph.
	graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(nodes, power = attachment, m = edges, directed = F)))

	# Return graph.
	return(graph)
}



#' @title .
#' @export
get.architecture <- function(type, nodes, ...) {
	# Capture the dot arguments.
	. <- list(...)

	# Make sure that the dots are not empty.
	if(length(.) == 0) {
		stop("Invalid `...` arguments. Please check the documentation.")
	}

	# Providing a random graph.
	if(type == "random") {
		# Stop the execution if the correct arguments are not specified.
		if(is.null(.[["p"]])) stop("Missing expected argument(s). See the documentation.")

		# Provide the graph.
		return(
			architecture.random(nodes, .[["p"]])
		)
	}

	# Providing a small world graph.
	if(type == "smallworld") {
		# Stop the execution if the correct arguments are not specified.
		if(is.null(.[["neighborhood"]]) || is.null(.[["p"]])) stop("Missing expected argument(s). See the documentation.")

		# Provide the graph.
		return(
			architecture.small.world(nodes, .[["neighborhood"]], .[["p"]])
		)
	}


	# Providing a scale free graph. 
	if(type == "scalefree") {
		# Stop the execution if the correct arguments are not specified.
		if(is.null(.[["attachment"]]) || is.null(.[["edges"]])) stop("Missing expected argument(s). See the documentation.")

		# Provide the graph.
		return(
			architecture.scale.free(nodes, .[["attachment"]], .[["edges"]])
		)
	}
}
