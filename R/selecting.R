# This file contains functions used for selecting the true model.



#' @title .
#' @export
select_true_model <- function(nodes, architecture, connectivity, model, ...) {
	
	# Random graphs.
	if(architecture == 1) { 
		undirected_unweighted_graph = architecture_random(nodes, connectivity$probability) 
	}

	# Scale-free graphs.
	if(architecture == 2) { 
		undirected_unweighted_graph = architecture_small_world(nodes, connectivity$neighbours, connectivity$rewiring_p) 
	}

	# Small world graphs.
	if(architecture == 3) { 
		undirected_unweighted_graph = architecture_scale_free(nodes, connectivity$attachmenet_p, connectivity$edges_per_step) 
	}

	# Prepare the true model data structure.	
	true_network = list(
		architecture = undirected_unweighted_graph
	)

	# Determine the parameters for the Ising model.
	if(model == 1) {
		parameters = parameters_ising_model(nodes)
		true_network$weights = undirected_unweighted_graph * parameters$weights
		true_network$thresholds = parameters$thresholds
	}

	# Determine the parameters for the GGM model.
	if(model == 2) {
		parameters = parameters_ggm_model(nodes)
		true_network$weights = undirected_unweighted_graph * parameters$weights
	}

	# Return.
	return(true_network)
}
