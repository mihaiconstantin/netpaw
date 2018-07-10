# Functions useful for running the simulation on multiple computers.



#' @title . 
#' @export
initialize_design <- function() {
	participants 	= seq(50, 1000, 50)
	nodes 			= c(10, 20, 30)
	architectures 	= c(random = 1, small_world = 2, scale_free = 3)
	connectedness 	= c(low = 1, medium = 2, large = 3)
	models 			= c(ising = 1, ggm = 2)

	# Building the factorial design.
	design = build_design(participants,
						  nodes,
						  architectures,
						  connectedness,
						  models)
	return(design)
}



#' @title .
#' @export
run_cell_range <- function(start, end, directory = ".", replications = 100, console) {
	# Starting time.
	t0 = Sys.time()

	# Create a unique identifier for the data.
	uuid = paste0(as.numeric(Sys.time()), '_', console, '_', round(runif(1), 5), '_')

	# Set the working directory.
	setwd(directory)

	# Open the connection to the log file.
	logs <- file(paste0(uuid, start, "_to_", end, "_logs.Rlog"), open = "wt")

	# Store all incoming messages, warnings, and errors.
	sink(logs, type = "message")

	# Perform the simulation.
	designs = initialize_design()
	results = replicate_cells(designs[start:end, ], replications)
	
	# Store the results.
	saveRDS(object = results, 				file = paste0(uuid, start, "_to_", end, "_results.RData"))
	saveRDS(object = designs[start:end, ], 	file = paste0(uuid, start, "_to_", end, "_designs.RData"))

	# Ending time.
	t1 = Sys.time()

	# Notify that everything went fine.
	cat(paste0(uuid, start, "_to_", end, "_results.RData"), "was sucessfuly written at", directory, "\n\n")
	cat(paste0('Start: ', t0, '. End: ', t1, '. Delta: ', t1 - t0, '.'))

	# Close the connection to the log file and disable the sink.
	sink(type = "message")
	close(logs)
}
