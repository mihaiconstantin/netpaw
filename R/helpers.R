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
			weights = estimation_result$graph
		)
	}

	return(extraction)
}



#' @title .
#' @export
check_node_variance <- function(nodeValues) {
    nodeValues = as.factor(nodeValues)
    valuesFrequency = table(nodeValues)
    minFrequency = min(valuesFrequency)
    maxFrequency = max(valuesFrequency)
    if(minFrequency <= 1 || maxFrequency >= length(nodeValues) - 1) return(F) else return(T)
}



#' @title .
#' @export
filter_nodes_with_little_variance <- function(data) {
	allowed_nodes = apply(data, 2, check_node_variance)
	return(data[, allowed_nodes])
}



# Activate only if required.
#' @title .
#' @export
sample_ising_data_with_variance <- function(participants, true_network_graph, true_network_thresholds) {
	cat('\t\t-> sampling data | ')
	
	attepmt = 1
	data = IsingSampler::IsingSampler(participants, true_network_graph, true_network_thresholds, nIter = 100, method = 'MH')

	# In case there is a column with no variance, resample.
	while(any(colSums(data) == 0) || any(colSums(data) == dim(data)[1])) 
	{
		attepmt = attepmt + 1
		data = IsingSampler::IsingSampler(participants, true_network_graph, true_network_thresholds, nIter = 100, method = 'MH')
		print(colSums(data))
	}
	
	cat(attepmt, 'needed\n')

	return(data)
}