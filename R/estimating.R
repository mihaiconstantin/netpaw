# This file contains functions related to estimating the model.


#' @title .
#' @export
run_cell <- function(participants, nodes, architecture, connectedness) {
	# User feedback at start.
	cat('\t-> cell configuration:', participants, 'participants |', nodes, 'nodes |', architecture, 'architecture |', connectedness, 'connectedness\n')
	
	# Operationalizing connectivity for each type of graph.
	connectivity = operationalize_connectedness(architecture, connectedness)

	# Building the true network.
	true_network = build_true_network(nodes, architecture, connectivity)

	# Sampling data.
	# # MOVE THIS TO A SEPARTEE FILE.
	# data = IsingSampler::IsingSampler(participants, true_network$graph, true_network$thresholds, nIter = 100, method = 'MH')

	# Estimating the observed network.
	estimated_network = IsingFit::IsingFit(filter_nodes_with_little_variance(data), plot = F, progressbar = F)

	# Preparing the return list.
	result = list(
		config = c(participants = participants, nodes = nodes, connectedness = connectedness, architecture = architecture),
		true = true_network,
		estimated = list(
			graph = estimated_network$weiadj,
			thresholds = estimated_network$thresh
		)
	)

	# User feedback at the end.
	cat('\t-> cell done \u2713 \n')

	return(result)
}



