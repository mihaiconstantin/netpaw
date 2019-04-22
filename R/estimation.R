# This file contains functions related to estimating PMRF models.



# Estimator types ---------------------------------------------------------

estimator.ising <- function(data) {
    # Estimate the model.
    result <- IsingFitEssential(data)

    return(result)
}



estimator.ggm <- function(data) {
    # Estimate the model.
    result <- bootnet::estimateNetwork(data, default = 'EBICglasso', verbose = FALSE, memorysaver = TRUE)
    
    # Remove the names, not needed.
    rownames(result$graph) <- colnames(result$graph) <- NULL

    return(result)
}



# Exported wrapper --------------------------------------------------------

#' @title Estimated the model based on the type of data provided.
#' @export
estimate.model <- function(data) {
    # Check that a `npdata` object is used.
    if(!inherits(data, "npdata")) {
        stop("Argument `data` must be an object of class `npdata`.")
    }

    # Determine which estimator to use based on the model type. 
    # Ensure that regardless of the estimation function, the result object looks the same.
    result <- list(
        # Record the model type.
        type = data$model, 

        # Store data and related infromation.
        data = data
    )

    # Estimating Ising.
    if(data$model == "ising") {
        # Fir the Ising model.
        model.fit <- estimator.ising(data$data)
        
        # Store the estimated paramaters.
        result$fit <- list(
            weights = model.fit$weiadj,
            thresholds = model.fit$thresholds
        )

    # Estimating GGM.
    } else if(data$model == "ggm") {
        # Fit the GGM model.
        model.fit <- estimator.ggm(data$data)
        
        # Store the estimated paramaters.
        result$fit <- list(
            weights = model.fit$graph,
            thresholds = NULL
        )
    
    # Unrecognized model type.
    } else {
        stop("Unsupported model type during estimation. Please request it at `m.a.constantin@uvt.nl`.")
    }

    # Set the class of the output.
    class(result) <- c('npfit', 'list')

    return(result)
}



# Object methods ----------------------------------------------------------

print.npfit <- function(object, weights = TRUE, ...) {
    # Details about the model.
    cat("\n")
    cat(crayon::black$bgGreen$bold("Fit details:"))
    cat("\n")
    cat(crayon::silver("  - class(es):", paste(shQuote(class(object)), collapse = ", ")))
    cat("\n")
    cat("  - type:", shQuote(crayon::yellow(object$type)))
    cat("\n")
    cat("  - mean absolute:", round(mean(abs(object$fit$weights[upper.tri(object$fit$weights)])), 3))
    cat("\n")
    cat("  - sd:", round(sd(object$fit$weights[upper.tri(object$fit$weights)]), 3))
    cat("\n")
    cat("  - range:", paste(round(c(min(object$fit$weights), max(object$fit$weights)), 3), crayon::yellow(paste("(", c("min", "max"), ")", sep = "")), collapse = crayon::silver(" | ")))
    cat("\n")
    
    # Details about the data.
    print(object$data, short = TRUE)

    if (weights) {
        # The weights matrix.
        cat("\n")
        cat(crayon::black$bgGreen$bold("Weights matrix:"))
        cat("\n\n")
        print(object$fit$weights, digits = 3)
        cat("\n")
        
        # The threhsold vector if applicable.
        cat(crayon::black$bgGreen$bold("Thresholds:"))
        cat("\n\n")
        print(object$fit$thresholds, digits = 3)
        cat("\n")
    }
}



plot.npfit <- function(object, ...) {
    # Color the edges.
    colors <- matrix(NA, ncol(object$fit$weights), nrow(object$fit$weights))
    colors[object$fit$weights > 0] <- POSITIVE.EDGE.COLOR
    colors[object$fit$weights < 0] <- NEGATIVE.EDGE.COLOR
        
    # Plot the weights matrix (i.e., estimated model). 
    qgraph::qgraph(object$fit$weights, layout = "spring", edge.color = colors, title = paste("Estimated model edge weights (", object$type, ")", sep = ""))    
}
