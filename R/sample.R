# This file contains functions for sampling data for various PMRF models.
# The data is checked for invariant columns (i.e., nodes).



sampler.ising <- function(n, model, nIter = 100, method = "MH") {
	# Sample data.
	data = IsingSampler::IsingSampler(n, model$weights, model$thresholds, nIter = 100, method = "MH")

	return(data)
}



sampler.ggm <- function(n, model, levels = 5) {
	# Fetch the weights.
	weights <- model$weights

	# Check for positive semi-definite.
	if (any(eigen(diag(ncol(weights)) - weights)$values < 0)) {
		stop("Precision matrix is not positive semi-definite")
	}

	# Get the covariance matrix.
	sigma <- cov2cor(solve(diag(ncol(weights)) - weights))
	
	# Sample data.
	data <- mvtnorm::rmvnorm(n, sigma = sigma)

	# Split the data into item steps.
	for (i in 1:ncol(data)) {
		data[, i] <- as.numeric(cut(data[, i], sort(c(-Inf, rnorm(levels - 1), Inf))))
	}

	# Return the data.
	return(data)
}




#' @title Sample data based on specified PMRF model.
#' @export
get.data <- function(n, model, attempts = 5, ...) {
	# Sample data with respect to the specified PMRF model.
	if(model$model == "ising") 
		sampler.fun = sampler.ising
	else if(model$model == "ggm")
		sampler.fun = sampler.ggm
	else
		stop("Unsupported model type. Please request it at ...")

	# Call the sampler.
	# Status 0 means sampling went fine; 1 indicates presence of invariant nodes.
	data = list(
		data = sampler.fun(n, model, ...),
		attempts = 0, 
		status = 0,
		model = model$model
	)
	
	# check if a resampling is needed and perform it for 10 times at most.
	if(should.resample(data$data) > 0) {
		# User feedback:
		cat("Invariant nodes detected -> attempting resampling... ")
		
		data = attempt.resampling(n, model, sampler.fun, attempts, ...)
	} 

	return(data)	
}



attempt.resampling <- function(n, model, sampler.fun, attempts, ...) {
	# Starting at 2nd attempt with an optimistic view that a good dataset will be found.
	attempt = 1
	status = 0
	feedback = paste0("Succeeded on attempt ", attempt, ".\n")

	# Initial resample.
	data = sampler.fun(n, model, ...)
 	
	# Attempt to get a good dataset, but no more than 10 times.
	while((should.resample(data) > 0) && (attempt <= attempts)) 
	{
		attempt = attempt + 1
		data = sampler.fun(n, model, ...)
	}

	# Determine if the approach was successful and remove the invariant nodes, but mark the data as not safe.
	if(should.resample(data) > 0) {
		status = 1
		data = drop.invariant.nodes(data)
		feedback = paste("Failed after", attempt, "resampling attempts. Dropping invariant nodes:", dim(model$weights)[2] - dim(data)[2],  "out of", dim(model$weights)[2], " total nodes.\n")
	}

	# User feedback:
	cat(feedback)

	# Return the data list object: attempts, status, actual data.
	return(
		list(
			data = data,
			attempts = attempt,
			status = status,
			model = model$model
		)
	)
}



# # # ------------------------------------
# # # Healper functions for sampling data. 
# # # ------------------------------------


should.resample <- function(data, tolerance = 1) {
	# Check each column in the dataset for at least 2 responses on a given category.
	variance.checks = apply(data, 2, is.invariant, tolerance)

	# Determine how many invariant nodes and return the integer (i.e., > 0 suggests resampling).
	invariant.nodes = sum(variance.checks)

	return(invariant.nodes)
}


is.invariant <- function(node, tolerance = 1) {
	node = as.factor(node)
	frequencies = table(node)
	categories = length(frequencies)
	
	if(categories <= 2)
	{
		nobs = length(node)
		min.frequency = min(frequencies)
		max.frequency = max(frequencies)
		
		if(min.frequency <= tolerance || max.frequency >= nobs - tolerance) {
			return(TRUE) 
		}
		else {
			return(FALSE)
		}
	} else {
		return(FALSE)
	}
}


drop.invariant.nodes <- function(data, tolerance = 1) {
	invariant.nodes = apply(data, 2, is.invariant, tolerance)
	return(data[, !invariant.nodes,  drop = FALSE])
}
