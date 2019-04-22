# In this file we are generating model parameters for various PMRFs.



# Model types -------------------------------------------------------------

model.ising <- function(graph.type, nodes, ..., positive.edge.ratio = 0.5, mean = 0, sd = 1) {
    # Undirected, unweighted network structure.
    graph <- gen.graph(graph.type, nodes, ...)

    # Prepare the weights matrix.
    weights <- graph$graph
    
    # Determine the number of parameters.
    number.parameters <- (nodes * (nodes - 1)) / 2
    
    # Decide the number of positive and negative edges.
    ratio <- sample.positive.parameter.ratio(number.parameters, positive.edge.ratio)

    # Sample the parameters.
    parameters <- abs(rnorm(number.parameters, mean, sd)) * ratio

    # Apply the parameters to the network structure.
    weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
    weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

    # Create the threshold parameters.
    thresholds <- -abs(rnorm(nodes, colSums(weights) / 2, abs(colSums(weights) / 6)))

    # Return list.
    return(list(
        type = 'ising',
        graph = graph,
        weights = weights, 
        thresholds = thresholds,
        generation.options = list(
            positive.edge.ratio = positive.edge.ratio,
            mean = mean,
            sd = sd
        )
    ))
}



model.ggm <- function(graph.type, nodes, ..., positive.edge.ratio = 0.5, range = c(0.5, 1), constant = 1.5) {
    # Undirected, unweighted network structure.
    graph <- gen.graph(graph.type, nodes, ...)

    # Prapre the weights matrix.
    weights <- graph$graph

    # Determine the number of parameters.
    number.parameters <- (nodes * (nodes - 1)) / 2
    
    # Decide the number of positive and negative edges.
    ratio <- sample.positive.parameter.ratio(number.parameters, 1 - positive.edge.ratio)

    # Sample the parameters.
    parameters <- runif(number.parameters, min(range), max(range)) * ratio

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
        type = 'ggm',
        graph = graph,
        weights = weights,
        thresholds = NULL,
        generation.options = list(
            positive.edge.ratio = positive.edge.ratio,
            min = min(range), 
            max = max(range), 
            constant = constant
        )
    ))
}



# Exported wrapper --------------------------------------------------------

#' @title Generate a PMRF (i.e., GGM or Ising).
#' @export
gen.model <- function(model.type, graph.type, nodes, ...) {
    # Handle the parameter generation for the supported models.
    if(model.type == "ising") {       
        result <- model.ising(graph.type, nodes, ...)

    } else if(model.type == "ggm") {
        result <- model.ggm(graph.type, nodes, ...)
    
    } else {
        stop("Unsupported model type. Please request it at `m.a.constantin@uvt.nl`.")
    }
    
    # Set the class of the output.
    class(result) <- c('npmodel', 'list')
    
    return(result)
}



# Helpers --------------------------------------------------------

sample.positive.parameter.ratio <- function(number.parameters, ratio) {
    positive.ratio <- sample(c(-1, 1), number.parameters, TRUE, prob = c(1 - ratio, ratio))

    return(positive.ratio)    
}



# Object methods ----------------------------------------------------------

print.npmodel <- function(object, graph = TRUE, ...) {
    # Details about the graph.
    print(object$graph, graph = FALSE)
    
    # Details about the model.
    cat("\n")
    cat(crayon::black$bgGreen$bold("Model details:"))
    cat("\n")
    cat(crayon::silver("  - class(es):", paste(shQuote(class(object)), collapse = ", ")))
    cat("\n")
    cat("  - type:", shQuote(crayon::yellow(object$type)))
    cat("\n")
    cat("  - mean absolute:", round(mean(abs(object$weights[upper.tri(object$weights)])), 3))
    cat("\n")
    cat("  - sd:", round(sd(object$weights[upper.tri(object$weights)]), 3))
    cat("\n")
    cat("  - range:", paste(round(c(min(object$weights), max(object$weights)), 3), crayon::yellow(c("(min)", "(max)")), collapse = crayon::silver(" | ")))
    cat("\n")
    cat("  - generation options:", paste(object$generation.options, crayon::yellow(paste("(", names(unlist(object$generation.options)), ")", sep = "")), collapse = crayon::silver(" | ")))
    cat("\n")
    
    if (graph) {
        # The graph matrix.
        print(object$graph, details = FALSE)
        
        # The weights matrix.
        cat(crayon::black$bgGreen$bold("Weights matrix:"))
        cat("\n\n")
        print(object$weights, digits = 3)
        cat("\n")
        
        # The threhsold vector if applicable.
        cat(crayon::black$bgGreen$bold("Thresholds:"))
        cat("\n\n")
        print(object$thresholds, digits = 3)
        cat("\n")
    }
}



plot.npmodel <- function(object, ...) {
    # Store the qgraph objects to compute the average layout.
    qgraph.object.graph <- qgraph::qgraph(object$graph$graph, layout = "spring", DoNotPlot = TRUE)
    qgraph.object.weights <- qgraph::qgraph(object$weights, layout = "spring", DoNotPlot = TRUE)
    
    # Compute the average layout.
    average.layout <- qgraph::averageLayout(qgraph.object.graph, qgraph.object.weights)
    
    # Color the edges.
    colors <- matrix(NA, ncol(object$weights), nrow(object$weights))
    colors[object$weights > 0] <- POSITIVE.EDGE.COLOR
    colors[object$weights < 0] <- NEGATIVE.EDGE.COLOR
    
    # Split the screen.
    par(mfrow = c(1, 2))
    
    # Plot the undirected, unweighted graph.
    plot(object$graph, layout = average.layout)
    
    # Plot the weights matrix (i.e., true model). 
    qgraph::qgraph(object$weights, layout = average.layout, edge.color = colors, title = "Weighted graph")
    
    par(mfrow = c(1, 1))
}
