# This file contains methods that are are used for operationalizing various aspects of the procedure.
# These functions are generally meant to allow for a clean interface of higher-order functions.



#' @title .
#' @export
operationalize_connectedness <- function(architecture, connectedness) {
	opertaionalization = list()

	# Random graphs (probability):
	# 	low = 	 .1
	# 	medium = .2
	# 	large =  .3
	if (architecture == 1) {
		if(connectedness == 1) { opertaionalization$probability = .1 }
		if(connectedness == 2) { opertaionalization$probability = .2 }
		if(connectedness == 3) { opertaionalization$probability = .3 }
	}

	# Scale-free graphs (neighbors | rewiring_p):
	#   low     = 2 | .1
	#   medium  = 2 | .5 
	#   large   = 2 |  1
	if (architecture == 2) {
		opertaionalization$neighbors = 2
		if(connectedness == 1) { opertaionalization$rewiring_p = .1 }
		if(connectedness == 2) { opertaionalization$rewiring_p = .5 }
		if(connectedness == 3) { opertaionalization$rewiring_p =  1 }
	}
	
	# Small world graphs (attachmenet_p | edges_per_step):
	# 	low 	= 1 | 1
	# 	medium 	= 1 | 2
	# 	large 	= 1 | 3
	if (architecture == 3) {
		opertaionalization$edges_per_step = 1
		if(connectedness == 1) { opertaionalization$attachmenet_p = 1 }
		if(connectedness == 2) { opertaionalization$attachmenet_p = 2 }
		if(connectedness == 3) { opertaionalization$attachmenet_p = 3 }
	}
	
	# Empirical graphs.

	return(opertaionalization)
}
