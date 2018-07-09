# This file contains methods that are are used for operationalizing various aspects of the procedure.
# These functions are generally meant to allow for a clean interface of higher-order functions.



#' @title .
#' @export
operationalize_connectedness <- function(architecture_code, connectedness_code) {
	opertaionalization = list()

	# Random graphs (probability):
	# 	low = 	 .1
	# 	medium = .2
	# 	large =  .3
	if (architecture_code == 1) {
		if(connectedness_code == 1) { opertaionalization$probability = .1 }
		if(connectedness_code == 2) { opertaionalization$probability = .2 }
		if(connectedness_code == 3) { opertaionalization$probability = .3 }
	}

	# Scale-free graphs (neighbors | rewiring_p):
	#   low     = 2 | .1
	#   medium  = 2 | .5 
	#   large   = 2 |  1
	if (architecture_code == 2) {
		opertaionalization$neighbors = 2
		if(connectedness_code == 1) { opertaionalization$rewiring_p = .1 }
		if(connectedness_code == 2) { opertaionalization$rewiring_p = .5 }
		if(connectedness_code == 3) { opertaionalization$rewiring_p =  1 }
	}
	
	# Small world graphs (attachmenet_p | edges_per_step):
	# 	low 	= 1 | 1
	# 	medium 	= 1 | 2
	# 	large 	= 1 | 3
	if (architecture_code == 3) {
		opertaionalization$edges_per_step = 1
		if(connectedness_code == 1) { opertaionalization$attachmenet_p = 1 }
		if(connectedness_code == 2) { opertaionalization$attachmenet_p = 2 }
		if(connectedness_code == 3) { opertaionalization$attachmenet_p = 3 }
	}
	
	return(opertaionalization)
}



#' @title .
#' @export
operationalize_architecture <- function(architecture_code) {
	architect = ifelse(
		architecture_code == 1, 
		architecture_random, 
		ifelse(
			architecture_code == 2, 
			architecture_small_world, 
			architecture_scale_free
			)
		)
	return(architect)
}



#' @title .
#' @export
operationalize_model_generator <- function(model_code) {
	model_generator = ifelse(
		model_code == 1, 
		parameters_ising_model, 
		parameters_ggm_model
		)
	return(model_generator)
}



#' @title .
#' @export
operationalize_data_generator <- function(model_code) {
	data_sampler = ifelse(
		model_code == 1, 
		ising_data_sampler, 
		ggm_data_sampler
		)
	return(data_sampler)
}



#' @title .
#' @export
operationalize_data_generator_string <- function(model_string) {
	data_sampler = ifelse(
		model_string == 'ising', 
		ising_data_sampler, 
		ifelse(
			model_string == 'ggm',
			ggm_data_sampler,
			stop('Internally set model name not recognized.')
		)
	)
	return(data_sampler)
}



#' @title .
#' @export
operationalize_model_estimator <- function(model_code) {
	model_estimator = ifelse(
		model_code == 1, 
		ising_model_estimator, 
		ggm_model_estimator
		)
	return(model_estimator)
}