# In this file we are generating model parameters for various PRMF models.



# Model types -------------------------------------------------------------
model.ising <- function(graph.type, nodes, ..., positive.edge.ratio = 0.5, mean = 0, sd = 1) {
    # Undirected, unweighted network structure.
    graph <- get.graph(graph.type, nodes, ...)

    # Prepare the weights matrix.
    weights <- graph$graph
    
    # Determine the number of parameters.
    number_parameters = (nodes * (nodes - 1)) / 2
    
    # Decide the number of positive and negative edges.
    ratio <- positive.parameter.ratio(number_parameters, positive.edge.ratio)

    # Sample the parameters.
    parameters <- abs(rnorm(number_parameters, mean, sd)) * ratio

    # Apply the parameters to the network structure.
    weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
    weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

    # Create the threshold parameters.
    thresholds <- -abs(rnorm(nodes, colSums(weights) / 2, abs(colSums(weights) / 6)))

    # Return list.
    return(list(
        model = 'ising',
        graph = graph,
        weights = weights, 
        thresholds = thresholds
    ))
}



model.ggm <- function(graph.type, nodes, ..., positive.edge.ratio = 0.5, range = c(0.5, 1), constant = 1.5) {
    # Undirected, unweighted network structure.
    graph <- get.graph(graph.type, nodes, ...)

    # Prapre the weights matrix.
    weights <- graph$graph

    # Determine the number of parameters.
    number_parameters = (nodes * (nodes - 1)) / 2
    
    # Decide the number of positive and negative edges.
    ratio <- positive.parameter.ratio(number_parameters, 1 - positive.edge.ratio)

    # Sample the parameters.
    parameters <- runif(number_parameters, min(range), max(range)) * ratio

    # Apply the parameters to the network structure.
    weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
    weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

    # Create the precision matrix (i.e., inverse of covariance matrix---concentration matrix) as Yin and Li (2011) and bootnet::genGGM().
    diag(weights) <- constant * rowSums(abs(weights))
    diag(weights) <- ifelse(diag(weights) == 0, 1, diag(weights))
    weights <- weights / diag(weights)[row(weights)]
    weights <- (weights + t(weights)) / 2

    # Create the partial corelation matrix from the precision matrix as qgraph::wi2net.
    weights <- -cov2cor(weights)
    diag(weights) <- 0

    # Return list.
    return(list(
        model = 'ggm',
        graph = graph,
        weights = weights,
        thresholds = 'n.a.'
    ))
}



# Exported wrapper --------------------------------------------------------
#' @title Generate a PMRF (i.e., GGM or Ising).
#' @export
get.model <- function(type, graph.type, nodes, ...) {
    # Handle the parameter generation for the supported models.
    if(type == "ising") {       
        result <- model.ising(graph.type, nodes, ...)

    } else if(type == "ggm") {
        result <- model.ggm(graph.type, nodes, ...)
    
    } else {
        stop("Unsupported model type. Please request it at `m.a.constantin@uvt.nl`.")
    }
    
    # Set the class of the output.
    class(result) <- c('netpowerTrueModel', 'list')
    
    return(result)
}



# Helpers --------------------------------------------------------
positive.parameter.ratio <- function(number.parameters, ratio) {
    positive.ratio <- sample(c(-1, 1), number.parameters, TRUE, prob = c(1 - ratio, ratio))

    return(positive.ratio)    
}



# Object methods ----------------------------------------------------------
print.netpowerTrueModel <- function(object, graph = TRUE, ...) {
    # Details about the graph.
    print(object$graph, graph = FALSE)
    
    # Details about the model.
    cat("\n")
    cat("Model details:")
    cat("\n")
    cat("  - class(es):", paste(shQuote(class(object)), collapse = ", "))
    cat("\n")
    cat("  - type:", shQuote(object$model))
    cat("\n")
    cat("  - mean absolute:", mean(abs(object$weights[upper.tri(object$weights)])))
    cat("\n")
    cat("  - sd:", sd(object$weights[upper.tri(object$weights)]))
    cat("\n")
    cat("  - range:", paste(shQuote(c("min", "max")), round(c(min(object$weights), max(object$weights)), 3), sep = " = ", collapse = " | "))
    cat("\n")
    
    if (graph) {
        # The graph matrix.
        print(object$graph, details = FALSE)
        
        # The weights matrix.
        cat("Weights matrix:")
        cat("\n\n")
        print(object$weights, digits = 3)
        cat("\n")
        
        # The threhsold vector if applicable.
        cat("Thresholds:")
        cat("\n\n")
        print(object$thresholds, digits = 3)
        cat("\n")
    }
}



plot.netpowerTrueModel <- function(object, ...) {
    # Store the qgraph objects to compute the average layout.
    qgraph.object.graph <- qgraph::qgraph(object$graph$graph, layout = "spring", DoNotPlot = TRUE)
    qgraph.object.weights <- qgraph::qgraph(object$weights, layout = "spring", DoNotPlot = TRUE)
    
    # Compute the average layout.
    average.layout = qgraph::averageLayout(qgraph.object.graph, qgraph.object.weights)
    
    # Color the edges.
    colors = matrix(NA, ncol(object$weights), nrow(object$weights))
    colors[object$weights > 0] = POSITIVE.EDGE.COLOR
    colors[object$weights < 0] = NEGATIVE.EDGE.COLOR
    
    # Split the screen.
    par(mfrow = c(1, 2))
    
    # Plot the undirected, unweighted graph.
    plot(object$graph, layout = average.layout)
    
    # Plot the weights matrix (i.e., true model). 
    qgraph::qgraph(object$weights, layout = average.layout, edge.color = colors, title = "Weighted graph")
    
    par(mfrow = c(1, 1))
}
