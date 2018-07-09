# This file contains functions for building the data structures for simulated cells.



#' @title .
#' @export
prepare_cell_results <- function(participants, nodes, architecture, connectedness, model, data, true_model, estimated_model) {
	# Store the raw results.
	raw_results <- prepare_raw_results(participants, nodes, architecture, connectedness, model, data, true_model, estimated_model)

	# Store the computed results (i.e., computed dependent variables).
	computed_results <- prepare_computed_results(true_model, estimated_model)

	# Creat the main result.
	results = list(
		raw = raw_results,
		computed = computed_results
	)

	return(results)
}



#' @title .
#' @export
prepare_raw_results <- function(participants, nodes, connectedness, architecture, model, data, true_model, estimated_model) {
	raw_results = list(
		config = c(participants = participants, nodes = nodes, connectedness = connectedness, architecture = architecture, model = model),
		data = data,
		true = list(
			weights = true_model$weights,
			tresholds = true_model$thresholds
		),
		estimated = list(
			weights = estimated_model$weights,
			tresholds = estimated_model$thresholds
		)
	)
	return(raw_results)
}



#' @title .
#' @export
prepare_computed_results <- function(true_model, estimated_model) {
	# Get only the true and estimated edges.
	true = true_model$weights[upper.tri(true_model$weights)]
	esti = estimated_model$weights[upper.tri(estimated_model$weights)]

	# Check if perfect estimation.
	perfect = all((true == 0) == (esti == 0))

	# True/ False Positive/ Negative rates.
	TP <- sum(true != 0 & esti != 0)
	FP <- sum(true == 0 & esti != 0)
	TN <- sum(true == 0 & esti == 0)
	FN <- sum(true != 0 & esti == 0)

	# Compound indicators based on the rates.
	sensitivity <- TP / (TP + FN) # Aka power.
	specificity <- TN / (TN + FP)
	type_one 	<- FP / (FP + TN)
	type_two 	<- FN / (TP + FN)

	# Edge weights correlation.
	correlation <- cor(true, esti)
	
	# Density for true and estimated graphs. 
	density_true = sum(true != 0) / length(true)
	density_esti = sum(esti != 0) / length(esti)

	# Check the size of both graphs (i.e., useful of nodes were dropped due to resampling problems).
	equal_size = dim(true_model$weights)[1] == dim(estimated_model$weights)[1]

	# Store everything into a list.
	computed_results = list(
		perfect 				= perfect,
		true_positive 			= TP, 
		false_positive 			= FP, 
		true_negative 			= TN, 
		false_negative 			= FN,
		sensitivity 			= sensitivity,
		specificity 			= specificity,
		type_one 				= type_one,
		type_two 				= type_two,
		edge_correlation 		= correlation,
		density_true_model 		= density_true,
		density_estimated_model = density_esti,
		equal_size 				= equal_size
	)

	return(computed_results)
}
