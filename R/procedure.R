# This file contains the functions needed for running and replicating the procedure.


#' @title .
#' @export
run_cell <- function(participants, nodes, architecture, connectedness, model) {
	# User feedback at start.
	cat('\t-> cell configuration:', participants, 'participants |', nodes, 'nodes |', architecture, 'architecture |', connectedness, 'connectedness. ')
	
	# Select the true model.
	true_model = select_true_model(nodes, architecture, connectedness, model)

	# Sample data based on the true model.
	data = sample_data(participants, model, true_model)

	# Estimate the observed network. TODO: Add invariance check here.
	estimated_model = estimate_model(model, data)

	# Store the results of a simulated design cell. TODO: Compute DV here, or after the fact?
	result = list(
		config = c(participants = participants, nodes = nodes, connectedness = connectedness, architecture = architecture),
		true = list(
			weights = true_model$weights,
			tresholds = ifelse(model == 1, true_model$thresholds, NA)
		),
		estimated = list(
			weights = estimated_network$weiadj,
			tresholds = ifelse(model == 1, true_model$thresholds, NA)
		)
	)

	# User feedback at the end.
	cat('Cell done \u2713. \n')

	return(result)
}
































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




#' @title .
#' @export
run_cells <- function(cells) {
	# User feedback at start.
	cat('-> Running simulation for', nrow(cells), 'cells:\n')

	# Storing the results per cell.
	results = list()

	# Running the cells.
	for (cell in 1:nrow(cells))
	{
		cell_result = run_cell(cells[cell, 1], cells[cell, 2], cells[cell, 3], cells[cell, 4])
		results[[cell]] = cell_result
	}

	# User feedback at end.
	cat('-> Completed all', nrow(cells), 'cells.\n')

	return(results)
}



#################################################################################
#################################################################################
#################################################################################



#' @title .
#' @export
run_cells_with_replication <- function(cells, replications) {
	# User feedback at start.
	cat('Design replications requested:', replications, '.\n', sep = '')

	# Storing the results per replication (i.e., each replication contains
	# the results of each design cell).
	results = list()

	# Running the cells with replication.
	for (replication in 1:replications)
	{
		# Notify about the current replication that is running.
		cat(
			'\n', rep('-', 30), '\n',
			'Replication: ', replication, '.\n',
			rep('-', 30), '\n',
			sep = ''
		)
	
		results[[replication]] = run_cells(cells)
	}

	# User feedback at end.
	cat('\nCompleted all', replications, 'replications.\n\n')

	return(results)
}
