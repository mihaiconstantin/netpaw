
#' @title Build the factorial design of the study.
#'
#' @description
#' This function builds the factorial design of the study based on the elements of the
#' vectors specified as arguments. Each argument specified (i.e., vector) is regarded
#' as a factor, and each value of the vectors is regarded as a level. 
#'
#' TODO: This function will be extended to work with any number of factors.
#
#' @usage build_design(participants, nodes, densities)
#'
#' @param participants (int vector) Varying number of participants, e.g., \code{c(10, 50, 100)}.
#' @param nodes        (int vector) Varying number of nodes (i.e., variables), e.g., \code{c(10, 15, 30)}.
#' @param densities    (double vector) Varying number of densities, e.g., \code{c(.2, .5, .8)}.
#'
#' @return A matrix representing the factorial design of the study, where each row is a design cell.
#'
#' @export
#'
build_design <- function(participants, nodes, densities) {
    # The design matrix.
    design = matrix(NA, 0, 3)

	for (a in 1:length(participants))
	{
		for (b in 1:length(nodes))
		{
			for (c in 1:length(densities))
			{
                design = rbind(design, c(participants[a], nodes[b], densities[c]))
			}
		}
	}
    colnames(design) <- c('participants', 'nodes', 'density')

    # User feedback.
    cat(
        '-> Design generated:',
        '\n\t-> our factorial design has', nrow(design), 'cells',
        '\n\t-> each row in the matrix stands for one design cell',
        '\n\n'
    )

    return(design)
}