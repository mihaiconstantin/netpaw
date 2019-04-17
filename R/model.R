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



model.ggm <- function(graph.type, nodes, ..., range = c(0.5, 1), constant = 1.5) {
    # Since we get the partial correlation matrix by taking the negative standardization of the precision 
    # matrix (i.e., see line 61) we need to redefine what the positive.edge.proportion argument means.
    # We do this by flipping the `meaning` of `positive.edge.ratio` argument of `get.graph`.
    . <- list(...)
    
    if (length(.) != 0 && !is.null(.[["positive.edge.ratio"]])) {
        .[["positive.edge.ratio"]] = 1 - .[["positive.edge.ratio"]]
    } else {
        # The reason for this is the following: if we want 100% positive edges, then we need to
        # specify that we want only negative edges (i.e., 0%) and due to the inverse sign we
        # get positive edges as a result. Below things are written explicitly. Second one 
        # represents the default positive edge ratio.
        .[["positive.edge.ratio"]] = 1 - 1
    }
    
    # Prepare the arguments for the graph.
    graph.args <- c(list(
        type = graph.type,
        nodes = nodes
    ), .)
    
    # Undireghted, unweighted network structure.
    graph <- do.call("get.graph", args = graph.args)

    # Preparing the weights matrix.
    weights <- graph$graph

    # Sampling the parameters.
    number_parameters = (nodes * (nodes - 1)) / 2
    parameters <- runif(number_parameters, min(range), max(range))

    # Applying the parameters to the network structure.
    # TODO: Consider remving the positive edge ration from the graph file and add it here.
    weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
    weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

    # Creating the precision matrix (i.e., inverse of covariance matrix---concentration matrix) as Yin and Li (2011) and bootnet::genGGM().
    diag(weights) <- constant * rowSums(abs(weights))
    weights <- weights / diag(weights)[row(weights)]
    weights <- (weights + t(weights)) / 2

    # Creating the partial corelation matrix from the precision matrix as qgraph::wi2net.
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
    
    # Set the class of the result.
    class(result) <- c('netpowerTrueModel', 'list')
    
    return(result)
}



# Object methods ----------------------------------------------------------
print.netpowerTrueModel <- function(object, graph = TRUE, ...) {
    # Details about the graph.
    print(object$graph, graph = FALSE)
    
    # Details about the model.
    cat("\n")
    cat("Model details:")
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
    
    # Get the non-zero edges.
    graph.weights = object$weights[upper.tri(object$weights)]
    edges = graph.weights[graph.weights != 0]
    
    colors = rep(NA, length(edges))
    colors[edges > 0] = POSITIVE.EDGE.COLOR
    colors[edges < 0] = NEGATIVE.EDGE.COLOR
    
    # Split the screen.
    par(mfrow = c(1, 2))
    
    # Plot the undirected, unweighted graph.
    plot(object$graph, layout = average.layout)
    
    # Plot the weights matrix (i.e., true model). 
    qgraph::qgraph(object$weights, layout = average.layout, edge.color = colors, title = "Weighted graph")
    
    par(mfrow = c(1, 1))
}
