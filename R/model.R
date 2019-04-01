# In this file we are generating model parameters for various PRMF models.



model.ising <- function(graph.type, nodes, ..., mean = 0, sd = 1) {
    # Undireghted, unweighted network structure.
    graph <- get.graph(graph.type, nodes, ...)

    # Preparing the weights matrix.
    weights <- graph
    
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
        graph = graph,
        weights = weights, 
        thresholds = thresholds
    ))
}



model.ggm <- function(graph.type, nodes, ..., range = c(0.5, 1), constant = 1.5) {
    # Since we get the partial correlation matrix by taking the negative standardization of the precision 
    # matrix (i.e., see line 61) we need to redefine what the positive.edge.proportion argument means.
    # We do this by flipping the `meaning` of `positive.edge.ratio` argument of `get.graph`.
    . <- list(...)
    
    if (length(.) != 0 && !is.null(.[["positive.edge.ratio"]])) {
        .[["positive.edge.ratio"]] = 1 - .[["positive.edge.ratio"]]
    } else {
        # The reason for this is the following: if we want 100% positive edges, then we need to
        # specify that we want only negative edges (i.e., 0%) and due to the inverse sign we
        # get positive edges as a result. Below things are written explicitly. Second one 
        # represents the default positive edge ratio.
        .[["positive.edge.ratio"]] = 1 - 1
    }
    
    # Prepare the arguments for the graph.
    graph.args <- c(list(
        type = graph.type,
        nodes = nodes
    ), .)
    
    # Undireghted, unweighted network structure.
    graph <- do.call("get.graph", args = graph.args)

    # Preparing the weights matrix.
    weights <- graph

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
get.model <- function(type, graph.type, nodes, ...) {
    # Handle the parameter generation for the supported models.
    if(type == "ising") {       
        return(model.ising(graph.type, nodes, ...))

    } else if(type == "ggm") {
        return(model.ggm(graph.type, nodes, ...))
    
    } else {
        stop("Unsupported model type. Please request it at `m.a.constantin@uvt.nl`.")
    }
}
