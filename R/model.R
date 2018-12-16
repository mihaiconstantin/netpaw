# In this file we are generating model parameters for various PRMF models.



model.ising <- function(nodes, architecture, ..., positive.edge.ratio = 0.5, mean = 0, sd = 1) {
    # Undireghted, unweighted network structure.
	weights <- get.architecture(type = architecture, nodes = nodes, ..., positive.edge.ratio = positive.edge.ratio)

	# Sampling the parameters.
	number_parameters = (nodes * (nodes - 1)) / 2
	ratio <- sample(c(-1, 1), number_parameters, TRUE, prob = c(positive.edge.ratio, 1 - positive.edge.ratio))
	parameters <- ratio * abs(rnorm(number_parameters, mean, sd))

    # Applying the parameters to the network structure.
	weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
	weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

	# Creating the threshold parameters.
	thresholds <- -abs(rnorm(nodes, colSums(weights) / 2, abs(colSums(weights) / 6)))

	# Return list.
	return(list(
		model = 'ising',
		weights = weights, 
		thresholds = thresholds
	))
}



model.ggm <- function(nodes, architecture, ..., positive.edge.ratio = 0.5, range = c(0.5, 1), constant = 1.5) {
    # Undireghted, unweighted network structure.
    weights <- get.architecture(type = architecture, nodes = nodes, ...)

    # Sampling the parameters.
    number_parameters = (nodes * (nodes - 1)) / 2
    ratio <- sample(c(-1, 1), number_parameters, TRUE, prob = c(positive.edge.ratio, 1 - positive.edge.ratio))
    parameters <- ratio * runif(number_parameters, min(range), max(range))

    # Applying the parameters to the network structure.
    weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
    weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

    # Creating the precision matrix (i.e., inverse of covariance matrix---concentration matrix) as Yin and Li (2011) and bootnet::genGGM().
    diag(weights) <- constant * rowSums(abs(weights))
    diag(weights) <- ifelse(diag(weights) == 0, 1, diag(weights))
    weights <- weights / diag(weights)[row(weights)]
    weights <- (weights + t(weights)) / 2

    # Creating the partial corelation matrix from the precision matrix as qgraph::wi2net.
    weights <- -cov2cor(weights)
    diag(weights) <- 0

	# Return list.
	return(list(
		model = 'ggm',
		weights = weights,
		thresholds = 'n.a.'
	))
}



#' @title Generate a PMRF (i.e., GGM or Ising).
#' @export
get.model <- function(type, nodes, architecture, ...) {
	# Capture the dot arguments.
	. <- list(...)

	# Make sure that the dots are not empty.
	if(length(.) == 0) {
		stop("Invalid `...` arguments. Please check the documentation.")
	}

	# Handle the parameter generation for the Ising model.
	if(type == "ising") {		
		return(
			model.ising(nodes, positive.edge.ratio, architecture, ...)
		)
	}

	# Handle the parameter generation for the GGM model.
	if(type == "ggm") {		
		return(
			model.ggm(nodes, positive.edge.ratio, architecture, ...)
		)
	}
}
