# In this file contains S3 methods for the netPower objects.



#' @title .
#' @export
plot.netPowerCell <- function(object, ...) {
	# Suppress qgraph warnings.
	toggle_console_warnings('off')
	
	# Dividing the canvas.    
	par(mfrow = c(1, 2))
	
	true = qgraph::qgraph(
		object$raw$true$weights, 
		title = 'True model',
		...,
		layout = 'spring', edge.labels = TRUE, edge.label.cex = 1, edge.label.bg = FALSE, edge.label.color = 'black'
	)
	
	qgraph::qgraph(
		object$raw$estimated$weights, 
		title = paste0('Estimated model (SEN: ', 
					   round(object$computed$sensitivity, 2), ' | SPE: ', 
					   round(object$computed$specificity, 2), ' | COR: ',
					   round(object$computed$edge_correlation, 2), ')'
				),
		...,
		layout = true$layout, edge.labels = TRUE, edge.label.cex = 1, edge.label.bg = FALSE, edge.label.color = 'black'
	)
	
	# Restoring the canvas.
	par(mfrow = c(1, 2))
	
	# Enable warnings.
	toggle_console_warnings('on')
}



#' @title .
#' @export
print.netPowerCell <- function(object, ...) {
	cat('\n', 'Simulation cell results: ', '\n')
	cat('\t', '  ', rep('-', 50),
		'\n',
		'\t -> config:', 
		object$raw$config[1], 'participants |', 
		object$raw$config[2], 'nodes |', 
		ifelse(object$raw$config[3] == 1, 'random', ifelse(object$raw$config[3] == 2, 'small world', 'scale free')), 'architecture |', 
		object$raw$config[4], 'connectedness |', 
		ifelse(object$raw$config[5] == 1, 'ising', 'ggm'), 'model.',
		'\n',
		'\t', '  ', rep('-', 50),
		'\n'
	)
	cat('\t -> sensitivity:', object$computed$sensitivity, '\n')
	cat('\t -> specificity:', object$computed$specificity, '\n')
	cat('\t -> type one error:', object$computed$type_one, '\n')
	cat('\t -> type two error:', object$computed$type_two, '\n')
	cat('\t -> edge correlation:', object$computed$edge_correlation, '\n')
	cat('\t -> equal # nodes:', ifelse(object$computed$equal_size, 'yes', 'no'), '\n')
	cat('Try plot(result) for a visual inspection.', '\n\n')
}
