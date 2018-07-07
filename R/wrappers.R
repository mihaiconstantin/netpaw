# This file contains some general wrappers used to avoid code repetition.



# Activate this if required.
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









