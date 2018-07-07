# This file contains the functions needed for replicating the estimated model.



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
