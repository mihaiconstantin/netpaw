# In this file we are generating model parameters for various PRMF models.



model.ising <- function(architecture, mean = 0, sd = 1) {
    # Undireghted, unweighted network structure.
    weights <- get.architecture(type = architecture, nodes = nodes, ..., positive.edge.ratio = positive.edge.ratio)

    # Sampling the parameters.
    number_parameters = (nodes * (nodes - 1)) / 2
    parameters <- abs(rnorm(number_parameters, mean, sd))

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



model.ggm <- function(architecture, range = c(0.5, 1), constant = 1.5) {
    # Since we get the partial correlation matrix by taking the negative standardization of the precision 
    # matrix (i.e., see line 52) we need to redefine what the positive.edge.proportion argument means.
    positive.edge.ratio = 1 - positive.edge.ratio
    
    # Undireghted, unweighted network structure.
    weights <- get.architecture(type = architecture, nodes = nodes, ..., positive.edge.ratio = positive.edge.ratio)

    # Sampling the parameters.
    number_parameters = (nodes * (nodes - 1)) / 2
    parameters <- runif(number_parameters, min(range), max(range))

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
get.model <- function(type, architecture, ...) {
    # Make sure that the dots are not empty.
    if(length(list(...)) == 0) stop("Invalid `...` arguments. Please check the documentation.")

    # Check if the arguments psssed via the ... are relevant for the ising and ggm, otherwise ignore them and use the defauts.


    # Handle the parameter generation for the supported models.
    if(type == "ising") {       
        return(
            model.ising(nodes, architecture, ...)
        )

    } else if(type == "ggm") {
        return(
            model.ggm(nodes, architecture, ...)
        )
    
    } else {
        stop("Unsupported model type. Please request it at `m.a.constantin@uvt.nl`.")
    }
}
