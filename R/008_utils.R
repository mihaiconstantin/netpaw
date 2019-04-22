# In this file we are storing general utils.



# Constants ---------------------------------------------------------------
POSITIVE.EDGE.COLOR = "#3F51B5"
NEGATIVE.EDGE.COLOR = "#F44336"



# Get the number of nodes from a graph or weighted matrix.
get.number.nodes <- function(graph) {
    # Determine the dimensions.
    dimensions = dim(graph)
    
    # Check the dimensions.
    if(dimensions[1] != dimensions[2]) stop('Wrong type of input: the graph dimensions do not match.')
    
    # Return the number of nodes.
    return(dimensions[1])
}



# Compute the density (i.e., based on non-zero edges) for an unweighted graph.
get.graph.density <- function(graph) {
    # Number of nodes.
    nodes = get.number.nodes(graph)
    
    # Potential connections.
    potential = (nodes * (nodes - 1)) / 2
    
    # Actual connections.
    actual = sum(graph[upper.tri(graph)] != 0)
    
    # Density.
    graph.density = actual / potential
    
    return(graph.density)
}



# Generate a covariance matrix.
# For details on what the arguments mean check: https://stats.stackexchange.com/a/215647/116619
get.cov <- function(nvars, svec.min = 0, svec.max = 1, pmat = NA, svec = NA) {
    # Construct the P matrix.
    if(is.na(pmat)) {
        # Generate the P orthogonal matrix.
        pmat <- qr.Q(qr(matrix(rnorm(nvars ^ 2), nvars)))
    }
    
    # Set the values of the s vector.
    if(length(svec) == 1 && is.na(svec)) {
        svec <- runif(nvars, svec.min, svec.max)
        svec <- svec[order(svec, decreasing = TRUE)]
        
    } else {
        # If user provided s vector then check that it is non-negative and in decreasing order.
        check.order = all(svec == svec[order(svec, decreasing = TRUE)])
        check.sign = any(svec < 0) 
        
        if(!check.order || check.sign) stop('Vector `svec` must contain positive elements in decreasing order.')
    }
    
    # Check that the dimensions of P and S match.
    if(ncol(pmat) != length(svec)) stop('Dimensions `svec` length must be equal to the dimensions of `pmat`.')
    
    # Generate the covariance matrix.
    covmat <- crossprod(pmat, pmat * svec)
    
    return(covmat)        
}



# Generate partial correlation matrices.
get.pcor <- function(nvars) {
    # Generate the covariance matrix.
    covmat <- get.cov(nvars)
    
    # Compute the precision matrix.
    precmat <- solve(covmat)
    
    # Compute the partial correlations.
    pcormat <- -cov2cor(precmat)
    
    # Set the diagonal to 0.
    diag(pcormat) <- 0
    
    return(pcor.mat)
}
