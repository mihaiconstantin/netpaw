
#' @title Runs the simulation for a selected number of design cells with replication.
#'
#' @description
#' This function is a wrapper around \code{\link{run_cells}} and it applies the simulation procedure
#' for all specified specified, \strong{with replication}.
#'
#' @usage run_cells_with_replication(cells, replications)
#'
#' @param cells         (matrix) A matrix containing the specifications of the cells that will be ran.
#'                               Usually a subset of the result of \code{\link{build_desing}}.
#' @param replications  (int scalar) The number of times to replicate the specified cells.
#'
#' @return A list containing, for each replication, the output of \code{\link{run_cell}}.
#'
#' @export
#'
run_cells_with_replication <- function(cells, replications) {
    # User feedback at start.
    cat('Design replications requested:', replications, '.\n', sep = '')

    # Storing the results per replication (i.e., each replication contains
    # the results of each design cell).
    results = list()

    # Running the cells with replication.
    for (replication in 1:replications)
    {
        # Notify about the current replication that is running.
        cat(
            '\n', rep('-', 30), '\n',
            'Replication: ', replication, '.\n',
            rep('-', 30), '\n',
            sep = ''
        )
    
        results[[replication]] = run_cells(cells)
    }

    # User feedback at end.
    cat('\nCompleted all', replications, 'replications.\n\n')

    return(results)
}
