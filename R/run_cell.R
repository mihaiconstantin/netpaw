
#' @title Run the simulation for a single configuration.
#'
#' @description
#' This function performs the simulation procedure for a single combination of factor levels (e.g.,
#' 100 participants x 15 nodes x .3 density). The simulation is ran without replication.
#' Check \code{run_cells_with_replication} for replication purposes.
#'
#' TODO: Describe the steps of the simulation.
#'
#' @usage run_cell(participants, nodes, density)
#'
#' @param participants (int scalar) A number of participants, e.g., 100.
#' @param nodes        (int scalar) A number of nodes (i.e., variables), e.g., 15.
#' @param density      (double scalar) A value indicating the density, e.g., .3
#'
#' @return              A list containing three sub-lists: \code{$config}, \code{$true}, and \code{$estimated}.
#'                      \code{$config} contains a vector with the levels specified for this cell (i.e., same order
#'                      as specified by the function definition). \code{$graph}) contains a matrix representing the
#'                      network structure. \code{$thresholds}) holds a vector representing the model thresholds.
#'
#' @export
#'
run_cell <- function(participants, nodes, density) {
    # User feedback at start.
    cat(
        '\t-> cell configuration:', participants, 'participants |', nodes, 'nodes |', density, 'density.'
    )

    # Generating the true network.
    true_network = ising_random(nodes, density)

    # Sampling data.
    data = IsingSampler::IsingSampler(participants, true_network$graph, true_network$thresholds, nIter = 100, method = 'MH')

    # Estimating the observed network.
    estimated_network = IsingFit::IsingFit(data, plot = F, progressbar = F)

    # Preparing the return list.
    result = list(
        config = c(participants, nodes, density),
        true = true_network,
        estimated = list(
            graph = estimated_network$weiadj,
            thresholds = estimated_network$thresh
        )
    )

    # User feedback at end.
    cat(' Done. \u2713 \n')

    return(result)
}
