# This file contains the functions for sampling data under various PMRF models.
# The data is checked for invariant columns (i.e., nodes).



#' @title .
#' @export
sample_data <- function(nodes, model) {

}






ising_data_sampler <- function(participants, weights, thresholds) {
	data = IsingSampler::IsingSampler(participants, weights, thresholds, nIter = 100, method = 'MH')


}






ggm_data_sampler <- function() {

	else {
		if (!is.matrix(input)) {
			stop("'input' is not a matrix or list.")
		}
		graph <- input
		intercepts <- rep(0, ncol(graph))
	}
	if (!all(diag(graph) == 0 | diag(graph) == 1)) {
		graph <- cov2cor(graph)
	}
	
	diag(graph) <- 0
	Sigma <- cov2cor(solve(diag(ncol(graph)) - graph))
	Data <- mvtnorm::rmvnorm(n, sigma = Sigma)

	for (i in 1:ncol(Data)){
	  Data[,i] <- as.numeric(cut(Data[,i],sort(c(-Inf,rnorm(5.000000-1),Inf))))
	}
	return(Data)




}
