# This file contains the functions needed for running and replicating design cells.


# Run a single design cell (not available to the end users).
run.cell <- function(sample.size, model.type, graph.type, nodes, ...) {
    
	# Select the true model.
    model <- gen.model(model.type, graph.type, nodes, ...)
    
	# Sample data based on the true model.
    data <- gen.data(sample.size, model)

	# Estimate the observed network.
    fit <- estimate.model(data)

    # Prepare the cell results (i.e., of class `npcell`).
    result <- extract.results(model, fit)

    return(result)
}




#' @title Run multiple design cell aka a `design` (not available to the end users).
#' @export
run.design <- function(design) {
	# User feedback at start.
	cat('-> Running simulation for', crayon::yellow(nrow(design)), 'cells:\n')

	# Storing the results per cell.
	results = list()

	# Running the cells.
	for (cell in 1:nrow(cells))
	{
		cell_result = tryCatch(
			{
				# Try to run the cell.
				run_cell(cells[cell, 1], cells[cell, 2], cells[cell, 3], cells[cell, 4], cells[cell, 5])
			}, 
			error = function(error) {
				# Provide feedback in the console.
				cat('Cell error. Skipped. \n')

				# Inform about the error (i.e., for logging.)
				info = paste0('Simulation error at index ', cell, ' in run_cells(). Cell config: ', cells[cell, 1], ' ', cells[cell, 2], ' ', cells[cell, 3], ' ', cells[cell, 4], ' ', cells[cell, 5], '.')
				message(paste0(cat('\t'), 'SIMERR: ', Sys.time(), '. ', info))
				message(paste0(cat('\t'), 'Actual error: ', error))
				
				# Return the info string to the results object.
				return(info)
			}
		)
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
	cat('Design replications requested: ', replications, '.\n', sep = '')

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
	class(results) <- c('netPowerReplication', 'list')

	# User feedback at end.
	cat('\nCompleted all', replications, 'replications.\n\n')

	return(results)
}
