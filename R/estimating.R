# This file contains functions related to estimating the model.



#' @title .
#' @export
estimate_model <- function(model, data) {
	# Determine which estimator to use based on the model type.
	model_estimator = operationalize_model_estimator(model)

	# Fit the model.
	estimated_model = model_estimator(data)

	# Extract only the relevant results.
	result = extract_from_fitted_model(model, estimated_model)

	return(result)
}



#' @title .
#' @export
ising_model_estimator <- function(data) {
	# result = IsingFit::IsingFit(data, plot = F, progressbar = F)	
	result = IsingFitEssential(data)

	return(result)
}



#' @title .
#' @export
ggm_model_estimator <- function(data) {
	result = bootnet::estimateNetwork(data, default = 'EBICglasso', verbose = F, memorysaver = T)
	
	# Remove the names, not needed.
	rownames(result$graph) <- colnames(result$graph) <- NULL

	return(result)
}
