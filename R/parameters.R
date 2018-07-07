# In this file we are generating model parameters for various PRMF models.
# Currently we generate for: 
# 	- ISING: weights + thresholds
# 	- GGM: weights

# Notes:
# 	- the parameters are generated to resemble psychopathology (i.e., absolute weights and negative thresholds)



#' @title .
#' @export
parameters_ising_model <- function(nodes) {
	# Weights.
	weights = ising_weights(nodes)
	
	# Thresholds.
	thresholds <- -abs(rnorm(nodes, colSums(weights) / 2, abs(colSums(weights) / 6)))

	# Return list.
	return(list(
		weights = weights, 
		thresholds = thresholds
	))
}



#' @title .
#' @export
parameters_ggm_model <- function(nodes) {
	# Weights.
	weights = ggm_weights(nodes)
	
	# Return list.
	return(list(
		weights = weights
	))
}



#' @title .
#' @export
ising_weights <- function(nodes) {
	number_parameters = (nodes * (nodes - 1)) / 2
	parameters = abs(rnorm(number_parameters, mean = 0, sd = 1))

	weights = matrix(0, nodes, nodes)
	weights[upper.tri(weights)] <- parameters
	weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]
	# weights = pmax(weights, t(weights))

	return(weights)
}

#' @title .
#' @export
ising_weights <- function(nodes, architecture_generator, ..., positive_ratio = 1) {
    # Undireghted, unweighted network structure.
	weights = architecture_generator(nodes, ...)

	# Sampling the parameters.
	number_parameters = (nodes * (nodes - 1)) / 2
	ratio <- sample(c(-1, 1), number_parameters, TRUE, prob = c(1 -positive_ratio, positive_ratio))
	parameters <- ratio * abs(rnorm(number_parameters, mean = 0, sd = 1))

    # Applying the parameters to the network structure.
	weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
	weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

	return(weights)
}


#' @title .
#' @export
ggm_weights <- function(nodes, architecture_generator, ..., range = c(.5, 1), positive_ratio = 1, constant = 1.5) {
    # Undireghted, unweighted network structure.
    weights = architecture_generator(nodes, ...)

    # Sampling the parameters.
    number_parameters = (nodes * (nodes - 1)) / 2
    ratio <- sample(c(-1, 1), number_parameters, TRUE, prob = c(positive_ratio, 1 - positive_ratio))
    parameters <- ratio * runif(number_parameters, min(range), max(range))

    # Applying the parameters to the network structure.
    weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
    weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

    # Creating the precision matrix (i.e., inverse of covariance matrix, or
    # concentration matrix) as Yin and Li (2011) and bootnet::genGGM().
    diag(weights) <- constant * rowSums(abs(weights))
    diag(weights) <- ifelse(diag(weights) == 0, 1, diag(weights))
    weights <- weights / diag(weights)[row(weights)]
    weights <- (weights + t(weights)) / 2

    # Creating the partial corelation matrix from the precision matrix as qgraph::wi2net.
    weights <- -cov2cor(weights)
    diag(weights) <- 0

    return(weights)
}
