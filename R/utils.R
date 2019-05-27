# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                             _                                                                           #
#                                                            | |                                                                          #
#                                                _ __    ___ | |_  _ __    __ _ __      __                                                #
#                                               | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /                                                #
#                                               | | | ||  __/| |_ | |_) || (_| | \ V  V /                                                 #
#                                               |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                                                  #
#                                                                 | |                                                                     #
#                                                                 |_|                                                                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                                                                                         #
# File contributors:                                                                                                                      #
#   - M.A. Constantin                                                                                                                     #
#                                                                                                                                         #
# File description:                                                                                                                       #
#   - this file contains various utilities used throughout the package                                                                    #
#                                                                                                                                         #
# Classes/ functions/ methods:                                                                                                            #
#   - many small functions                                                                                                                #
#                                                                                                                                         #
# Additional information:                                                                                                                 #
#   - n.a.                                                                                                                                #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Package specific functions ----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Conveniently patch function bodies.
patch.function <- function(fun, patch, position = 1) {
    # Deparse the body.
    fun.body <- deparse(body(fun))
    
    # Append the patch to function body where intended.
    patched.fun.body <- paste0(
        c(fun.body[1:position], patch, fun.body[(position + 1):length(fun.body)]),
        collapse = "\n"
    )

    # Parse and treat as an expression.
    expr <- as.expression(parse(text = patched.fun.body))

    return(expr)
}



# Patch a function binding within an environment (i.e., by reference). 
patch.function.within.environment <- function(binding, environment, patch) {
    # Unlock the binding in the environment.
    unlockBinding(binding, environment)
    
    # Alter the function.
    body(environment[[binding]]) <- patch.function(environment[[binding]], patch)
    
    # Lock the binding in the `self` environment.
    lockBinding(binding, environment)

    # Prevent return printing NULL.
    invisible() 
}



# Assert something or fail with custom error message.
assert.condition <- function(truth, error.message) {
    if(!truth) {
        stop(error.message)
    }
}



# Flatten wierd nested lists.
# Copyright Michael (https://stackoverflow.com/a/41882883/5252007).
flatten.nested.list <- function(nested.list) {
    more.lists <- sapply(nested.list, function(x) is.list(x))
    
    output <- c(nested.list[!more.lists], unlist(nested.list[more.lists], recursive = FALSE))
    
    if(sum(more.lists)) {
        Recall(output)
        
    } else {
        return(output)
    }
}



# Validate a list of arguments associated with a function against the `...` object.
validate.arguments <- function(args, fun, target.list) {
    # The arguments as specified in the function definition.
    definition.args <- as.list(args(fun))
    
    result <- sapply(args, function(arg) {
        # Does the argument have a default value?
        has.default <- !is.symbol(definition.args[[arg$name]])
        
        # Is the argument present in the target list?
        is.missing <- is.null(target.list[[arg$name]])
        
        # The argument has a default value, but that value is being overwritten by the user.
        # The argument doesn't have a default value and it not missing.
        if((has.default && !is.missing) || (!has.default && !is.missing)) {
            is.conformable <- conformity.check(target.list[[arg$name]], arg)
        
        # The argument doesn't have an default value and it's also missing.
        } else if(!has.default && is.missing) {
            # Since the argument is missing and no default exists, it is by default non-conformable.
            is.conformable = FALSE
            
            # Some user feedback.
            message(paste("Missing expected argument '", arg$name ,"'. Code: M.GR#001.", sep = ""))
        
        } else {
            # If argument has a default value that is not overwritten by the user we are good.
            is.conformable = TRUE    
        }
        
        return(is.conformable)
    })
    
    return(all(result))
}



# Argument conformity check.
conformity.check <- function(value, arg) {
    if(arg$type == "int" || arg$type == "double") {
        # If the argument value is integer or double check the bounds.
        is.conformable <- ((value >= min(arg$range)) && (value <= max(arg$range)))
        
    } else if(arg$type == "bool") {
        # If the argument value is logical check that only logical values are passed.
        is.conformable <- is.logical(value) && !is.na(value)
        
    } else {
        # Safety net in case I add new argument types and I forget to add conformity checks.
        stop("Unrecognized argument type; currently supported: 'int', 'double', and 'bool'. CODE: E.UT#001")
    }
    
    # Some user feedback.
    if(!is.conformable) {
        message(paste("Argument '", arg$name, " = ", value ,"' is non-conformable. Code: M.GR#002.", sep = ""))
    }
    
    return(is.conformable)
}



# Combine arguments from a function definition and a target list (e.g., '...' object).
combine.arguments <- function(fun, target.list) {
    # Get the arguments the definition arguments as a list.
    definition.args <- as.list(args(fun))
    
    # Remove the last redundant element.
    definition.args <- definition.args[-length(definition.args)]
    
    overwritten.args <- sapply(names(definition.args), function(arg.name) {
        if(!is.symbol(definition.args[[arg.name]]) && is.null(target.list[[arg.name]])) {
            return(definition.args[[arg.name]])
            
        } else {
            return(target.list[[arg.name]])
        }
    }, simplify = FALSE)
    
    return(overwritten.args)
}



# Give a list of arguments (i.e., as defined in the context of `netpaw`) generate numeric values.
generate.arguments <- function(args.list, ...) {
    # Capture the `...` overwrites as a list.
    . <- list(...)

    # Generate or overwrite arguments accordingly.
    parameters <- lapply(args.list, function(arg) {
        # First check to see if we desire to overwrite the generation with specific values.
        if(!is.null(.[[arg$name]])) {
            value <- .[[arg$name]]

        } else {
            # Choose the correct type.
            if(arg$type == "int") {
                value <- floor(runif(1, min(arg$range), max(arg$range) + 1))
                
            } else if(arg$type == "double") {
                value <- runif(1, min(arg$range), max(arg$range))
            
            } else if(arg$type == "bool") {
                value <- sample(0:1, 1, TRUE, c(.5, .5)) == TRUE
            }
        }
        
        return(value)
    })

    return(parameters)
}



# Sample a vector of positive and negative integers according to a ratio. 
sample.positive.parameter.ratio <- function(number.parameters, ratio) {
    positive.ratio <- sample(c(-1, 1), number.parameters, TRUE, prob = c(1 - ratio, ratio))

    return(positive.ratio)    
}



# Determine if a column is invariant.
is.invariant = function(column, tolerance = 1) {
    column <- as.factor(column)
    frequencies <- table(column)
    categories <- length(frequencies)
    
    if(categories <= 2)
    {
        nobs <- length(column)
        min.frequency <- min(frequencies)
        max.frequency <- max(frequencies)
        
        if(min.frequency <= tolerance || max.frequency >= nobs - tolerance) {
            return(TRUE) 
        }
        else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Generic functions (i.e., may be exported) -------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

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
