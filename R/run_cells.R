
#' @title Runs the simulation for a selected number of design cells.
#'
#' @description
#' This function is a wrapper around \code{\link{run_cell}} and it applies the simulation procedure
#' for all specified cells. The simulation is ran without replication. Check \code{\link{run_cells_with_replication}} 
#' for replication purposes.
#'
#' @usage run_cells(cells)
#'
#' @param cells (matrix) A matrix containing the specifications of the cells that will be ran.
#'                       Usually a subset of the result of \code{\link{build_design}}.
#'
#' @return A list containing the output of \code{\link{run_cell}} for each specified cell.
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
        cell_result = run_cell(cells[cell, 1], cells[cell, 2], cells[cell, 3], cells[cell, 4])
        results[[cell]] = cell_result
    }

    # User feedback at end.
    cat('-> Completed all', nrow(cells), 'cells.\n')

    return(results)
}
