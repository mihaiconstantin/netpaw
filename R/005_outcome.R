# This file contains functions for comparing the true and estimated models.

# TODO: Add multiple plots for the replicated cells.
# - `corrplot`
# - network graphs for individually replicated cells
# - `ggplot2` sensitivity graphs with box plots


# Exported wrapper --------------------------------------------------------

#' @title Compare and compute the results for a single run.
#' @export
extract.results <- function(true.model, estimated.model) {

    # Prepare the configuration that was run.
    config <- list(
        sample.size = estimated.model$data$rows,
        graph.type = true.model$graph$type,
        model.type = true.model$type,
        nodes = ncol(true.model$weights),
        graph.generation.options = true.model$graph$generation.options,
        model.generation.options = true.model$generation.options
    )

    # Prepare the outcomes.
    outcomes <- compute.outcomes(true.model$weights, estimated.model$fit$weights)

    # Store the results.
    result <- list(
        # Set the config to the results.
        config = config,

        # Set the true model parameters.
        true.model = true.model,

        # Set the estimated model parameters.
        fit = estimated.model,

        # Set the outcomes.
        outcomes = outcomes
    )

    # Set the correct classes for the list objects. Maybe move to 006?
    class(result$config) <- c("npconfig", "list")
    class(result$outcomes) <- c("npoutcome", "list")
    
    # Set the class for the cell results.
    class(result) <- c("npsimulatedcell", "list")

    return(result)
}



# Helper functions --------------------------------------------------------

compute.outcomes <- function(true.model.weights, estimated.model.weights) {
	# Get only the true and estimated edges.
	true = true.model.weights[upper.tri(true.model.weights)]
	esti = estimated.model.weights[upper.tri(estimated.model.weights)]

	# Check if perfect recovery in terms of presence/ absence of an edge.
	perfect = all((true == 0) == (esti == 0))

	# Check the size of both graphs (i.e., useful of nodes were dropped due to restamping issues).
	equal.size = dim(true.model.weights)[1] == dim(estimated.model.weights)[1]

	# True/ False Positive/ Negative rates.
	TP <- sum(true != 0 & esti != 0)
	FP <- sum(true == 0 & esti != 0)
	TN <- sum(true == 0 & esti == 0)
	FN <- sum(true != 0 & esti == 0)

	# Compound indicators based on the rates.
	sensitivity <- TP / (TP + FN) # Aka power.
	specificity <- TN / (TN + FP)
	type.one 	<- FP / (FP + TN)
	type.two 	<- FN / (TP + FN)

    # Compute type S error rate.
    # TODO: type S error.
    type.s <- "not yet implemented"

    # Compute type M error rate.
    # TODO: type M error.
    type.m <- "not yet implemented"

	# Edge weights correlation.
	correlation <- ifelse(equal.size, cor(true, esti), NA)
	
	# Density for true and estimated graphs. 
	density.true = sum(true != 0) / length(true)
	density.esti = sum(esti != 0) / length(esti)

	# Store everything into a list.
	results = list(
		perfect 				= perfect,
		true.positive 			= TP, 
		false.positive 			= FP, 
		true.negative 			= TN, 
		false.negative 			= FN,
		sensitivity 			= sensitivity,
		specificity 			= specificity,
		type.one 				= type.one,
		type.two 				= type.two,
		type.s 				    = type.s,
		type.m 				    = type.m,
		edge.correlation 		= correlation,
		density.true.model 		= density.true,
		density.estimated.model = density.esti,
        equal.size 				= equal.size
	)

	return(results)
}



# Object methods ----------------------------------------------------------


print.npconfig <- function(object, ...) {
    # Format the object for easier printing.
    config <- flatten.nested.list(object)

    # Remove name prefixes.
    names(config) <- gsub("graph.generation.options.", "", names(config))
    names(config) <- gsub("model.generation.options.", "", names(config))

    # Details about the cell configuration.
    cat("\n")
    cat(crayon::black$bgGreen$bold("Requested configuration:"))
    cat("\n")
    cat(crayon::silver("  - class(es):", paste(shQuote(class(object)), collapse = ", ")))
    cat("\n")
    cat(paste(paste("  - ", gsub("\\.", " ", names(unlist(config))), sep = ""), crayon::yellow(config), sep = ": ", collapse = "\n"))
    cat("\n")
}



print.npoutcome <- function(object, ...) {
    # Details about the cell outcomes.
    cat("\n")
    cat(crayon::black$bgGreen$bold("Outcomes:"))
    cat("\n")
    cat(crayon::silver("  - class(es):", paste(shQuote(class(object)), collapse = ", ")))
    cat("\n")
    cat(paste(paste("  - ", gsub("\\.", " ", names(unlist(object))), sep = ""), crayon::green(object), sep = ": ", collapse = "\n"))
    cat("\n")
}


# Maybe move to 006?
print.npsimulatedcell <- function(object, ...) {
    # Details about the graph.
    print(object$config, ...)

    # Details about the true model.
    print(object$true.model, ...)

    # Details about the estimated model.
    print(object$fit, ...)

    # Details about the outcomes.
    print(object$outcomes, ...)
}
