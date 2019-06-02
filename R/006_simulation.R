# This file contains the functions needed for running and replicating design cells.


# Run a single design cell (not available to the end users).
run.cell <- function(sample.size, model.type, graph.type, nodes, ..., verbose = TRUE) {
    # Start the timer time.
    start = proc.time()
    
    # Should we show user feedback?
    if(verbose) {
        # User feedback at the before execution.
        cat(
            # The title.
            crayon::black$bgYellow$bold("Cell config:"),         "\n",
            
            # The obvious arguments.
            "  - sample size: ", crayon::yellow(sample.size),    "\n",
            "  - model: ", crayon::yellow(model.type),           "\n",
            "  - graph: ", crayon::yellow(graph.type),           "\n",
            "  - nodes: ", crayon::yellow(nodes),                "\n",

            # The not so obvious arguments.
            paste(paste("  - ", gsub("\\.", " ", names(unlist(list(...)))), sep = ""), crayon::yellow(list(...)), sep = ": ", collapse = "\n"),  "\n",

            # `cat` options.
            sep = ""
        )
    }

    # Select the true model.
    model <- gen.model(model.type, graph.type, nodes, ...)
    
	# Sample data based on the true model.
    data <- gen.data(sample.size, model)

	# Estimate the observed network.
    fit <- estimate.model(data)

    # Prepare the cell results (i.e., of class `npcell`).
    result <- extract.results(model, fit)

    # Append the duration.
    result$time = list(
        system = Sys.time(),
        # Stop the timer time.
        duration = (proc.time() - start)[[3]]
    )

    # Should we show user feedback?
    if(verbose) {
        # User feedback after execution.
        cat("  - execution time: ", crayon::green(paste(round(result$time$duration, 3), "s", sep = "")), "\n", sep = "")
    }

    return(result)
}




#' @title Run multiple design cell aka a `design` (not available to the end users).
#' @export
run.design <- function(design) {
	# User feedback at start.
	cat(crayon::white$bgBlue$bold('Simulating', nrow(design$matrix), 'cells:\n\n'))

	# Storing the results per cell.
	results = list()

	# Running the cells.
	for (cell.index in 1:nrow(design$matrix))
	{
		cell.result = tryCatch(
			{
				# Extract the non-NA cell parameters.
                config = design$matrix[cell.index, !is.na(design$matrix[1, ])]

                # Remove the prefixes for graph and model options.
                names(config) <- gsub(paste("graph.options.", config$graph, ".", sep = ""), "", names(config))
                names(config) <- gsub(paste("model.options.", config$model, ".", sep = ""), "", names(config))
                
                # Drop the last column because it indicates the number of replications.
                config <- config[-length(config)]
                
                # Try to run the cell.
                result <- do.call("run.cell", config)

                # Provide feedback about the success:
                cat("  - status: ", crayon::green("succeeded"), "\n\n")

                # Return the results.
                return(result)
			}, 
			error = function(error) {
				# Provide feedback about the error.
                cat("  - status: ", crayon::red("failed"), "\n\n")
                
				# Collect and inform about the error (i.e., for logging.)
				info = paste('Simulation error at design row ', cell.index, ' in `run.design()`. Cell config: ', paste(config, paste("(", names(config), ")", sep = ""), sep = " ", collapse = " | "), '.', sep = "")
                message(paste(cat('\t'), 'SIMERR: ', Sys.time(), '. ', info))
				message(paste(cat('\t'), 'Actual error: ', error))
				
				# Return the info string to the results object.
				return(info)
			}
		)
		results[[cell.index]] = cell.result
	}
	
	# Set the class to allow for fancier handling of the nested data structure.
	# class(results) <- c('netPowerCells', 'list')

	# User feedback at end.
	cat(crayon::white$bgBlue$bold('Completed all', nrow(design$matrix), 'cells:\n'))

	return(results)
}



#' @title .
#' @export
replicate.design <- function(cells, replications) {
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
