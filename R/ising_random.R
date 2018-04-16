# ising_random ------------------------------------------------------

#' @title Generate Ising parameters for a random network architecture.
#'
#' @description
#' This function is used to generate random Ising model parameters for a random network architecture.
#'
#' @details
#' First, we sample a binary dataset N ^ 2, where N is the number of nodes in the network. The
#' probability of sampling a value of 1 is equal to the probability specified by the function
#' argument p, and the probability of sampling a 0 is 1 - p.
#'
#' Next, we create a new symmetrical matrix from the sampled dataset and set its main diagonal
#' to 0. This represents the unweighted graph.
#'
#' Next, we create an weighted graph from the unweighted graph. To do so we generate a matrix
#' with values from the normal distribution (M = 0, SD = 1) with the same number of columns
#' and rows and the unweighted matrix. We also make this matrix symmetrical (i.e., no need to
#' set the main diagonal to 0). Now, the weighted random graph is obtained by multiplying
#' the unweighted graph by the symmetrical random matrix.
#'
#' @usage ising_random(number_nodes, p, seed = F)
#'
#' @param number_nodes (int scalar) Number of nodes.
#' @param p            (double scalar) Probability of obtaining an edge in the network.
#' @param seed         (boolean scalar | int scalar) Should the sampling occur under seeding? Defaults to FALSE.
#'
#' @return             A list containing the graph (i.e., interaction parameters) and the thresholds.
#'
#' @export
#'
ising_random <- function(number_nodes, p, seed = F) {
    # Keep everything but `p` constrained.
    if(seed) set.seed(seed)

    # Network graph.
    graph <- matrix(sample(0:1, number_nodes ^ 2, TRUE, prob = c(1 - p, p)), number_nodes, number_nodes) * rnorm(number_nodes ^ 2)
    graph <- pmax(graph, t(graph))
    diag(graph) <- 0

    # Model thresholds.
    thresholds <- -rowSums(graph) / 2

    # Return list.
    network = list()
    network$graph = graph
    network$thresholds = thresholds

    return(network)
}




# ising_random_debug ------------------------------------------------------

#' @title Draw and plot the creation of a random Ising graph.
#'
#' @description
#' This function is used to illustrate how the random graphs for the Ising model are sampled.
#'
#' @details
#' First, we sample a binary dataset N ^ 2, where N is the number of nodes in the network. The
#' probability of sampling a value of 1 is equal to the probability specified by the function
#' argument p, and the probability of sampling a 0 is 1 - p.
#'
#' Next, we create a new symmetrical matrix from the sampled dataset and set its main diagonal
#' to 0. This represents the unweighted graph.
#'
#' Next, we create an weighted graph from the unweighted graph. To do so we generate a matrix
#' with values from the normal distribution (M = 0, SD = 1) with the same number of columns
#' and rows and the unweighted matrix. We also make this matrix symmetrical (i.e., no need to
#' set the main diagonal to 0). Now, the weighted random graph is obtained by multiplying
#' the unweighted graph by the symmetrical random matrix.
#'
#' My two cents on this procedure. I think that we end up with a biased number of edges in
#' the unweighted graph. The reason for this may have to do with the way we sample the binary
#' data in the first place. Say we specified p = .8 for a network with 10 nodes, then, we
#' expect our binary sample to show close to 80% of edges (i.e., the sum of all cells ~80 ).
#' However, these edges are placed at random in the matrix (i.e., this is not a symmetrical
#' matrix). Thus, when we transform the matrix to be symmetrical, we end up with more edges
#' than we originally requested (i.e., 80% because of the p = .8).
#'
#' I am not sure if this is a problem or not. But would the following make sense?
#' Instead of sampling a binary dataset of N ^ 2 with a p = .8 that represents the edges in
#' the network, we sample the interaction parameters. Specifically, we first determine the
#' number of interaction parameters as N * (N - 1) / 2, then we sample a binary vector of
#' the same size as the number if interaction parameters with p = .8. Next, we use this
#' vector to build the unweighted graph, meaning that both the upper and the lower triangle
#' of the matrix are set to this vector. The difference being that both triangles hold values
#' sampled with the same probability.
#'
#' @usage ising_random_debug(number_nodes, p, set_seed = F, plot = T,
#'                    procedure = 1, debug = F, feedback = F)
#'
#' @param number_nodes (int scalar) Number of nodes.
#' @param p            (double scalar) Probability of obtaining an edge in the network.
#' @param set_seed     (boolean scalar | int scalar) Should the sampling occur under seeding? Defaults to FALSE.
#' @param plot         (boolean scalar) Should the unweighted and weighted graphs be plotted? Defaults to TRUE.
#' @param procedure    (int scalar) Should the binary sampling occur for the edges or the interaction parameters?
#'                                     Values: 1 for edges or 2 for interaction parameters. Defaults to 1.
#' @param debug        (boolean scalar) Should the feedback regarding the number of edges be included in the output? Defaults to FALSE.
#' @param Feedback     (boolean scalar) Should the feedback be displayed to the console? Defaults to FALSE.
#'
#' @return             A list containing the sampled data, the unweighted graph and the weighted graph. If \code{debug} is set
#'                     to TRUE additional information is included.
#' @export
#'
ising_random_debug <- function(number_nodes, p, set_seed = F, plot = T, procedure = 1, debug = F, feedback = F)
{
    # Set seed if necessary.
    if (set_seed) {
        set.seed(set_seed)
        cat('\n', 'With seed:', set_seed, '\n\n')
    }


    # Determine how we construct the unweighted graph.
    if (procedure != 1 && procedure != 2) stop('Incorrect procedure specified. Allowed values 1 or 2.')

    if (procedure == 1) {
        # We are sampling a binary dataset of edges.
        binary_sample = matrix(sample(0:1, number_nodes ^ 2, TRUE, prob = c(1 - p, p)), number_nodes, number_nodes)

        # Unweighted graph.
        unweighted_graph <- pmax(binary_sample, t(binary_sample))
        diag(unweighted_graph) <- 0
    } else {
        # We are sampling a binary vector of interaction parameters.
        interaction_parameters = number_nodes * (number_nodes - 1) / 2
        binary_sample = sample(0:1, interaction_parameters, TRUE, prob = c(1 - p, p))

        # Unweighted graph.
        unweighted_graph <- diag(0, number_nodes, number_nodes)
        unweighted_graph[upper.tri(unweighted_graph, diag = F)] = binary_sample
        unweighted_graph[lower.tri(unweighted_graph)] <- t(unweighted_graph)[lower.tri(unweighted_graph)]
    }

    # Compute how many edges we expected and how many we actually got.
    sampled_total       = if (procedure == 1) number_nodes ^ 2 else interaction_parameters
    sampled_received    = sum(binary_sample)
    sampled_expected    = round(sampled_total * p)
    sampled_delta       = sampled_expected - sampled_received
    sampled_observed_p  = round(sampled_received / sampled_total, 3)

    # Compute the edges that made it in the unweighted graph.
    # Shouldn't we take into a account the fact that the diagonal is not relevant and subtract N from N ^ 2?
    unweighted_total        = if (procedure == 1) number_nodes ^ 2 else interaction_parameters
    unweighted_expected     = round(unweighted_total * p)
    unweighted_received     = sum(unweighted_graph)
    unweighted_delta        = unweighted_expected - unweighted_received
    unweighted_observed_p   = round(unweighted_received / unweighted_total, 3)

    # If requested, display the feedback to the console.
    if (feedback) {
        # Feedback for the binary sampled matrix or vector.
        cat(
            paste('Sampled with p = ', p, ':', sep = ''),
            paste('  -> total:',        sampled_total),
            paste('  -> expected:',     sampled_expected),
            paste('  -> received:',     sampled_received),
            paste('  -> delta:',        sampled_delta),
            paste('  -> observed p:',   sampled_observed_p),
            '\n', sep = '\n')

        # Feedback for the edges that made it in the unweighted graph.
        cat(
            paste('Edges in the unweighted graph:'),
            paste('  -> total:',        unweighted_total),
            paste('  -> expected:',     unweighted_expected),
            paste('  -> received:',     unweighted_received),
            paste('  -> delta:',        unweighted_delta),
            paste('  -> observed p:',   unweighted_observed_p),
            '\n', sep = '\n')
    }

    # Weighted graph
    random_weights <- matrix(rnorm(number_nodes ^ 2), number_nodes, number_nodes)
    random_weights <- pmax(random_weights, t(random_weights))
    weighted_graph <- unweighted_graph * random_weights


    # Plot both side by side.
    if (plot) {
        par(mfrow = c(1, 2))
            unweighted_graph_plot = qgraph::qgraph(unweighted_graph, layout = 'spring')
            qgraph::qgraph(weighted_graph, layout = unweighted_graph_plot$layout)
        par(mfrow = c(1, 1))
    }


    # Create the return object.
    graph = list()
    graph$sampled = binary_sample
    graph$unweighted = unweighted_graph
    graph$weighted = weighted_graph

    # Adding additional debug information if requested.
    if (debug) {
        graph$debug$sample = matrix(c(sampled_total, sampled_expected, sampled_received, sampled_delta, sampled_observed_p), 1, 5)
        graph$debug$unweighted = matrix(c(unweighted_total, unweighted_expected, unweighted_received, unweighted_delta, unweighted_observed_p), 1, 5)
        colnames(graph$debug$sample) <- colnames(graph$debug$unweighted) <- c('total', 'expected', 'received', 'delta', 'observed p')
    }

    return(graph)
}
