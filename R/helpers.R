# This file contains helper functions not bound to particular network architectures or models.



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