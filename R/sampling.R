# This file contains the functions for sampling data under various PMRF models.
# The data is checked for invariant columns (i.e., nodes).



#' @title .
#' @export
sample_data <- function(participants, model, true_model) {
	# Determine the data sampler for which model to use.
	data_sampler = operationalize_data_generator(model)

	# Call the sampler.
	data = data_sampler(participants, true_model)

	return(data)
}



#' @title .
#' @export
ising_data_sampler <- function(participants, true_model) {
	# Sample data.
	data = IsingSampler::IsingSampler(participants, true_model$weights, true_model$thresholds, nIter = 100, method = 'MH')

	return(data)
}



#' @title .
#' @export
ggm_data_sampler <- function(participants, true_model, nLevels = 5) {
	# Create the ordinal data sampler.
	ggm_sampler = bootnet::ggmGenerator(TRUE, nLevels)

	# Sample the data.
	data = ggm_sampler(participants, true_model$weights)

	return(data)
}
