# This file contains the functions needed for running and replicating the procedure.

# TODO:
# 1. fix the feedback test (~)
# 2. make function to run portions of the design on different PCs (~ ~)
# 3. look for option to use multicore package (~ ~)
# 4. create class methods for 'netPowerCell', 'netPowerCells', 'netPowerReplication'
# 	- print and plot



#' @title .
#' @export
run_cell <- function(participants, nodes, architecture, connectedness, model) {
	# User feedback at start.
	cat('\t-> cell configuration:', participants, 'participants |', nodes, 'nodes |', architecture, 'architecture |', connectedness, 'connectedness |', model, 'model. ')
	
	# Select the true model.
	true_model <- select_true_model(nodes, architecture, connectedness, model)

	# Sample data based on the true model.
	data <- sample_data(participants, true_model)

	# Estimate the observed network.
	estimated_model <- estimate_model(model, data$data)

	# Prepare the results.
	result <- prepare_cell_results(participants, nodes, architecture, connectedness, model, data, true_model, estimated_model)
	class(result) <- c('netPowerCell', 'list')

	# User feedback at the end.
	cat('Cell done \u2713. \n')

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
		cell_result = run_cell(cells[cell, 1], cells[cell, 2], cells[cell, 3], cells[cell, 4], cells[cell, 5])
		results[[cell]] = cell_result
	}
	
	# Set the class to allow for fancier handling of the nested data structure.
	class(results) <- c('netPowerCells', 'list')

	# User feedback at end.
	cat('-> Completed all', nrow(cells), 'cells.\n')

	return(results)
}



#' @title .
#' @export
replicate_cells <- function(cells, replications) {
	# User feedback at start.
	cat('Design replications requested:', replications, '.\n', sep = '')

	# Storing the results per replication (i.e., each replication contains the results of each design cell).
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

	# Set the class to allow for fancier handling of the nested data structure.
	class(results) <- class('netPowerReplication', 'list')

	# User feedback at end.
	cat('\nCompleted all', replications, 'replications.\n\n')

	return(results)
}
