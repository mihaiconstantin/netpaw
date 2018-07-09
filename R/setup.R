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
run_cell_range <- function(start, end, directory = ".", replications = 100)
{
	# Set the working directory.
	setwd(directory)

	# Open the connection to the log file.
	logs <- file(paste0(start, "_to_", end, "_logs.Rlog"), open = "wt")

	# Store all incoming messages, warnings, and errors.
	sink(logs, type = "message")

	# Perform the simulation.
	designs = initialize_design()
	results = replicate_cells(designs[start:end, ], replications)
	
	# Store the results.
	saveRDS(object = results, 				file = paste0(start, "_to_", end, "_results.RData"))
	saveRDS(object = designs[start:end, ], 	file = paste0(start, "_to_", end, "_designs.RData"))

	# Notify that everything went fine.
	cat(paste0(start, "_to_", end, "_results.RData"), "was sucessfuly written at", directory, "\n\n")

	# Close the connection to the log file and disable the sink.
	sink(type = "message")
	close(logs)
}
