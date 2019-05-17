# In this file we store general util functions.



# Package specific functions ----------------------------------------------



# Get the number of nodes from a graph or weighted matrix.
get.number.nodes <- function(graph) {
    # Determine the dimensions.
    dimensions = dim(graph)
    
    # Check the dimensions.
    if(dimensions[1] != dimensions[2]) stop('Wrong type of input: the graph dimensions do not match.')
    
    # Return the number of nodes.
    return(dimensions[1])
}



# Flatten wierd nested lists.
# Copyright Michael (https://stackoverflow.com/a/41882883/5252007).
flatten.nested.list <- function(nested.list) {
    more.lists <- sapply(nested.list, function(x) is.list(x))
    
    output <- c(nested.list[!more.lists], unlist(nested.list[more.lists], recursive=FALSE))
    
    if(sum(more.lists)) {
        Recall(output)
        
    } else {
        return(output)
    }
}



# Check whether a list of arguments exists in the `...` and it conforms.
check.arguments <- function(args, target.list) {
    result <- sapply(args, function(arg) {
        # Is the argument present in the target list?
        is.missing <- is.null(target.list[[arg$name]])
        
        # Since the argument is missing, it is by default non-conformable.
        if(is.missing) {
            is.conformable = FALSE
            
            # Some user feedback.
            message(paste("Missing expected argument '", arg$name ,"'. Code: M.GR#001.", sep = ""))
        
        # Is the argument conformable?
        } else {
            is.conformable <- ((target.list[[arg$name]] >= min(arg$range)) && (target.list[[arg$name]] <= max(arg$range)))
            
            if(!is.conformable) {
                # Some user feedback.
                message(paste("Argument '", arg$name, " = ", target.list[[arg$name]] ,"' is non-conformable. Code: M.GR#002.", sep = ""))
            }
        }
        
        # What conclusion do we draw?
        conclusion <- (!is.missing && is.conformable)
        
        # Return the check conclusion.
        return(conclusion)
    })
    
    return(all(result))
}



# Give a list of arguments (i.e., as defined in the context of `netpaw`) generate numeric values.
generate.arguments <- function(args.list) {
    # Generate arguments accordingly.
    parameters <- lapply(args.list, function(arg) {
        # Choose the correct type.
        if(arg$type == "int") {
            value <- floor(runif(1, min(arg$range), max(arg$range) + 1))
            
        } else if(arg$type == "double") {
            value <- runif(1, min(arg$range), max(arg$range))
        }  
        
        return(value)
    })

    return(parameters)
}



# Generic functions (i.e., may be exported) -------------------------------



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
    # Construct the `P` matrix.
    if(is.na(pmat)) {
        # Generate the `P` orthogonal matrix.
        pmat <- qr.Q(qr(matrix(rnorm(nvars ^ 2), nvars)))
    }
    
    # Set the values of the `s` vector.
    if(length(svec) == 1 && is.na(svec)) {
        svec <- runif(nvars, svec.min, svec.max)
        svec <- svec[order(svec, decreasing = TRUE)]
        
    } else {
        # If user provided `s` vector then check that it is non-negative and in decreasing order.
        check.order = all(svec == svec[order(svec, decreasing = TRUE)])
        check.sign = any(svec < 0) 
        
        if(!check.order || check.sign) stop('Vector `svec` must contain positive elements in decreasing order.')
    }
    
    # Check that the dimensions of `P` and `S` match.
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
