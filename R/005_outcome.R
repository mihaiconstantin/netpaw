# This file contains functions for comparing the true and estimated models.



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
        true.model = list(
            weights = true.model$weights[upper.tri(true.model$weights)],
            thresholds = true.model$tresholds
        ),

        # Set the estimated model parameters.
        fit = list(
            weights = estimated.model$fit$weights[upper.tri(estimated.model$fit$weights)],
            thresholds = estimated.model$fit$tresholds
        ),

        # Set some information about the data.
        data = fit$data,

        # Set the outcomes.
        outcomes = outcomes
    )

    # Set the correct class.
    class(result) <- c("npcell", "list")
}



# Helper functions --------------------------------------------------------

compute.outcomes <- function(true.model.weights, estimated.model.weights) {
	# Get only the true and estimated edges.
	true = true.model.weights[upper.tri(true.model.weights)]
	esti = estimated.model.weights[upper.tri(estimated.model.weights)]

	# Check if perfect estimation.
	perfect = all((true == 0) == (esti == 0))

	# Check the size of both graphs (i.e., useful of nodes were dropped due to resampling problems).
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
		edge.correlation 		= correlation,
		density.true.model 		= density.true,
		density.estimated.model = density.esti,
		equal.size 				= equal.size
	)

	return(results)
}
