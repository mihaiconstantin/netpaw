# This file contains functions for sampling data for various PMRF models.
# The data is checked for invariant columns (i.e., nodes).



# Data generator types ----------------------------------------------------

sampler.ising <- function(n, model, nIter = 100, method = "MH") {
    # Sample data.
    data <- IsingSampler::IsingSampler(n, model$weights, model$thresholds, nIter = nIter, method = method)

    return(data)
}



sampler.ggm <- function(n, model, levels = 5) {
    # Check for positive semi-definite.
    if (any(eigen(diag(ncol(model$weights)) - model$weights)$values < 0)) {
        stop("Precision matrix is not positive semi-definite.")
    }

    # Get the covariance (? correlation perhaps) matrix.
    sigma <- cov2cor(solve(diag(ncol(model$weights)) - model$weights))
    
    # Sample data.
    data <- mvtnorm::rmvnorm(n, sigma = sigma)

    # Split the data into item steps.
    for (i in 1:ncol(data)) {
        data[, i] <- as.numeric(cut(data[, i], sort(c(-Inf, rnorm(levels - 1), Inf))))
    }

    # Return the data.
    return(data)
}



# Exported wrapper --------------------------------------------------------

#' @title Sample data for a specified PMRF model.
#' @export
gen.data <- function(n, model, resampling.attempts = 5, ...) {
    # Check that a `npmodel` object is used.
    if(!inherits(model, "npmodel")) {
        stop("Argument `model` must be an object of class `npmodel`.")
    }

    # Select the sampler.
    if(model$type == "ising") {
        sampler.fun = sampler.ising
        
    } else if(model$type == "ggm") {
        sampler.fun = sampler.ggm
        
    } else {
        stop("Unsupported model type. Please request it at `m.a.constantin@uvt.nl`.")
    }

    # Call the sampler; 
    dataset = sampler.fun(n, model, ...)
    
    # Set the results object; status 0 means sampling went fine; 1 indicates presence of invariant node(s).
    result <- list(
        model = model$type,
        dataset = dataset,
        rows = nrow(dataset),
        cols = ncol(dataset),
        item.steps = sort(unique(c(dataset))),
        resampling.attempts = 0, 
        status = 0
    )
    
    # Check if a resampling is needed and perform it for 5 times at most.
    if(should.resample(dataset) > 0) {
        # User feedback.
        cat("Invariant nodes detected -> attempting resampling... ")
        
        # Resampling.
        result <- attempt.resampling(n, model, sampler.fun, resampling.attempts, ...)
    } 

    # Set the class of the output.
    class(result) <- c('npdata', 'list')

    return(result)
}



# Support functions for sampling data -------------------------------------

attempt.resampling <- function(n, model, sampler.fun, resampling.attempts, ...) {
    # Starting at 2nd attempt with an optimistic view that a good dataset will be found.
    attempt <- 1
    status <- 0
    
    # Initial resample.
    dataset <- sampler.fun(n, model, ...)
    
    # Attempt to get a good dataset, but no more than 10 times.
    while((should.resample(dataset) > 0) && (attempt <= resampling.attempts)) 
    {
        attempt <- attempt + 1
        dataset <- sampler.fun(n, model, ...)
    }

    # Determine if the approach was successful and remove the invariant nodes, but mark the data as not safe.
    if(should.resample(dataset) > 0) {
        status <- 1
        dataset <- drop.invariant.nodes(dataset)
        feedback <- paste("Failed after", attempt, "resampling attempts. Dropping invariant nodes:", dim(model$weights)[2] - dim(dataset)[2], "out of", dim(model$weights)[2], "total nodes.\n")
    
    } else {
        feedback <- paste0("Succeeded on attempt ", attempt, ".\n")
    }

    # User feedback:
    cat(feedback)

    # Return the data list object: resampling.attempts, status, actual data.
    return(
        list(
            model = model$type,
            dataset = dataset,
            rows = nrow(dataset),
            cols = ncol(dataset),
            item.steps = sort(unique(c(dataset))),
            resampling.attempts = attempt,
            status = status
        )
    )
}



# Helper functions for sampling data --------------------------------------

should.resample <- function(data, tolerance = 1) {
    # Check each column in the dataset for at least 2 responses on a given category.
    variance.checks <- apply(data, 2, is.invariant, tolerance)

    # Determine how many invariant nodes and return the integer (i.e., > 0 suggests resampling).
    invariant.nodes <- sum(variance.checks)

    return(invariant.nodes)
}



is.invariant <- function(node, tolerance = 1) {
    node <- as.factor(node)
    frequencies <- table(node)
    categories <- length(frequencies)
    
    if(categories <= 2)
    {
        nobs <- length(node)
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



drop.invariant.nodes <- function(data, tolerance = 1) {
    invariant.nodes = apply(data, 2, is.invariant, tolerance)
    return(data[, !invariant.nodes,  drop = FALSE])
}



# Object methods ----------------------------------------------------------

print.npdata <- function(object, data = TRUE, short = TRUE, ...) {
    # Details about the data.
    cat("\n")
    cat(crayon::black$bgGreen$bold("Data details:"))
    cat("\n")
    cat(crayon::silver("  - class(es):", paste(shQuote(class(object)), collapse = ", ")))
    cat("\n")
    cat("  - generating model:", shQuote(crayon::yellow(object$model)))
    cat("\n")
    cat("  - dimensions:", paste(object$rows, "by", object$cols))
    cat("\n")
    cat("  - item steps:", paste(sort(unique(c(object$dataset))), collapse = crayon::silver(" | ")))
    cat("\n")
    cat("  - resampling attempts:", object$resampling.attempts)
    cat("\n")
    cat("  - generation status:", ifelse(object$status == 0, crayon::green("succeeded"), crayon::red("failed")))
    cat("\n")
    
    # Should we show the data?
    if(data) {
        # The data matrix.
        cat("\n")
        cat(crayon::black$bgGreen$bold("Dataset preview:"))
        cat("\n\n")

        if(short) {
            print(head(object$data, ...))
            cat("\n")
            cat(crayon::green(". . ."))
            cat("\n")
        } else {
            print(object$data, ...)
        }
    }

    cat("\n")
}
