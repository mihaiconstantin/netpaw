# This file contains helper functions not bound to particular network architectures or models.



#' @title .
#' @export
extract_from_fitted_model <- function(model_code, estimation_result) {
	# Extracting parameters for Ising model.
	if(model_code == 1) {
		extraction = list(
			weights = estimation_result$weiadj,
			thresholds = estimation_result$thresholds
		)
	}

	# Extracting parameters for GGM model.
	if(model_code == 2) {
		extraction = list(
			weights = estimation_result$graph,
			thresholds = 'n.a.'
		)
	}

	return(extraction)
}



#' @title .
#' @export
is_invariant <- function(node, tolerance = 1) {
	node = as.factor(node)
	frequencies = table(node)
	categories = length(frequencies)
	
	if(categories <= 2)
	{
		nobs = length(node)
		min_frequency = min(frequencies)
		max_frequency = max(frequencies)
		
		if(min_frequency <= tolerance || max_frequency >= nobs - tolerance) {
			return(TRUE) 
		}
		else {
			return(FALSE)
		}
	} else {
		return(FALSE)
	}
}



#' @title .
#' @export
should_resample <- function(data, tolerance = 1) {
	# Check each column in the dataset for at least 2 responses on a given category.
	variance_checks = apply(data, 2, is_invariant, tolerance)

	# Determine how many invariant nodes and return the integer (i.e., > 0 suggests resampling).
	invariant_nodes = sum(variance_checks)

	return(invariant_nodes)
}



#' @title .
#' @export
filter_invariant_nodes <- function(data, tolerance = 1) {
	invariant_nodes = apply(data, 2, is_invariant, tolerance)
	return(data[, !invariant_nodes,  drop = FALSE])
}
