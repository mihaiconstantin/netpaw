# This file contains the functions for sampling data under various PMRF models.
# The data is checked for invariant columns (i.e., nodes).



#' @title .
#' @export
sample_data <- function(participants, true_model, resampling_attepmts = 10) {
	# Determine the data sampler for which model to use.
	data_sampler = operationalize_data_generator_string(true_model$model)

	# Call the sampler.
	data = list(
		data = data_sampler(participants, true_model),
		attempts = 1, 
		status = 'ok'
	)
	
	# check if a resampling is needed and perform it for 10 times at most.
	if(should_resample(data$data) > 0) {
		print('Resampling!')
		data = attempt_resampling(participants, true_model, data_sampler, resampling_attepmts)
	} 

	return(data)	
}



#' @title .
#' @export
attempt_resampling <- function(participants, true_model, data_sampler, attempts = 10) {
	# Starting at 2nd attempt with an optimistic view that a good dataset will be found.
	attempt = 2
	status = 'ok'

	# Initial resample.
	data = data_sampler(participants, true_model)

	# Attempt to get a good dataset, but no more than 10 times.
	while((should_resample(data) > 0) && (attempt <= attempts)) 
	{
		attempt = attempt + 1
		data = data_sampler(participants, true_model)
		print(should_resample(data))
	}

	# Determine if the approach was successful and remove the invariant nodes, but mark the data as not safe.
	if(should_resample(data) > 0) {
		status = 'not ok'
		data = filter_invariant_nodes(data)
	}

	# Return the data list object: attempts, status, actual data.
	return(
		list(
			data = data,
			attempts = attempt,
			status = status
		)
	)
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
