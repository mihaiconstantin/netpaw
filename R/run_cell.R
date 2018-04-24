
#' @title Runs the simulation for a single configuration (i.e., design cell).
#'
#' @description
#' Applies the simulation procedure for a single combination of factor levels (e.g.,
#' 100 participants x 15 nodes x .3 density x random architecture). The simulation is
#' ran without replication. Check \code{\link{run_cells_with_replication}} for
#' replication purposes.
#'
#' TODO: Describe the steps of the simulation.
#'
#' @usage run_cell(participants, nodes, density)
#'
#' @param participants  (int scalar) A number of participants, e.g., 100.
#' @param nodes         (int scalar) A number of nodes (i.e., variables), e.g., 15.
#' @param density       (double scalar) A value indicating the density, e.g., .3.
#' @param architectures (int scalar) A value indicating the network architecture, i.e., c(random = 1, small_world = 2, scale_free = 3, empirical = 4).
#'
#' @return              A list containing three sub-lists: \code{$config}, \code{$true}, and \code{$estimated}.
#'                      \enumerate {
#'                          \item \code{$config} contains a vector with the levels specified for this cell
#'                          \item \code{$graph}) contains a matrix representing the network structure
#'                          \item \code{$thresholds}) holds a vector representing the model thresholds
#'                      }
#'
#' @export
#'
run_cell <- function(participants, nodes, density, architecture)
{
    # User feedback at start.
    cat(
        '\t-> cell configuration:', participants, 'participants |', nodes, 'nodes |', density, 'density |', architecture, 'arch.'
    )

    # Generating the true network.
    if (architecture == 1) {
    
        true_network = ising_random(nodes, density)
    
    } else if (architecture == 2) {
    
        true_network = ising_small_world()
    
    } else if (architecture == 3) { 
    
        true_network = ising_scale_free()
    
    } else if (architecture == 4) { 
    
        true_network = ising_empirical()
    }

    # Sampling data.
    data = IsingSampler::IsingSampler(participants, true_network$graph, true_network$thresholds, nIter = 100, method = 'MH')

    # Estimating the observed network.
    estimated_network = IsingFit::IsingFit(data, plot = F, progressbar = F)

    # Preparing the return list.
    result = list(
        config = c(participants, nodes, density, architecture),
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
