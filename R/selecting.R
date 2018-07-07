# This file contains functions used for selecting the true model.



#' @title .
#' @export
select_true_model <- function(nodes, architecture, connectedness, model, ...) {
	# Get the right architect.
	architect = operationalize_architecture(architecture)

	# Determine the right density based on the specified architecture.
	connectivity = operationalize_connectedness(architecture, connectedness)

	# Determine the right model generator.
	model_generator = operationalize_model_generator(model)

	# Prepare the paramters.
	parameters = list(
		nodes = nodes,
		architect = architect,
		# Additional arguments intended for the `...` can be passed here (i.e., positive_ratio).
		... = ...
	)
	parameters = append_architeture_parameters(parameters, architecture, connectivity)

	# Generate the true network.
	true_network = do.call('model_generator', parameters)

	# Return.
	return(true_network)
}



#' @title .
#' @export
append_architeture_parameters <- function(parameters, architecture_code, connectivity) {
	if(architecture_code == 1) {
		parameters$p.or.m = connectivity$probability
	}

	if(architecture_code == 2) {
		parameters$nei = connectivity$neighbors
		parameters$p = connectivity$rewiring_p
	}

	if(architecture_code == 2) {
		parameters$power = connectivity$attachmenet_p
		parameters$m = connectivity$edges_per_step
	}

	return(parameters)
}
