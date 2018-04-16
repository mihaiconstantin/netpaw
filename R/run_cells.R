
#' @title Run the simulation for the selected design cells.
#'
#' @description
#' This function is a wrapper around \code{run_cell} and it performs the simulation procedure
#' for all the design cells specified. The simulation is ran without replication. Check
#' \code{run_cells_with_replication} for replication purposes.
#'
#' TODO: Improve the documentation: Link to the other functions.
#'
#' @usage run_cell(participants, nodes, density)
#'
#' @param cells (matrix) A matrix containing the specifications for the cells that will be ran.
#'                       Usually a subset of the result of \code{build_desing}.
#'
#' @return A list containing the output of \code{run_cell} for each cell specified.
#'
#' @export
#'
run_cells <- function(cells) {
    # User feedback at start.
    cat('-> Running simulation for', nrow(cells), 'cells:\n')

    # Storing the results per cell.
    results = list()

    # Running the cells.
    for (cell in 1:nrow(cells))
    {
        cell_result = run_cell(cells[cell, 1], cells[cell, 2], cells[cell, 3])
        results[[cell]] = cell_result
    }

    # User feedback at end.
    cat('-> Completed all', nrow(cells), 'cells.\n\n')

    return(results)
}
