
#' @title Build the factorial design of the study.
#'
#' @description
#' This function builds the factorial design of the study based on the elements of the
#' vectors specified as arguments. Each argument specified (i.e., vector) is regarded
#' as a factor, and each value of the vectors is regarded as a level.
#'
#' TODO: Add one more factor: network architecture (i.e., after asking Claudia).
#
#' @usage build_design(participants, nodes, densities, architectures)
#'
#' @param participants (int vector) Varying number of participants, e.g., \code{c(10, 50, 100)}.
#' @param nodes        (int vector) Varying number of nodes (i.e., variables), e.g., \code{c(10, 15, 30)}.
#' @param densities    (double vector) Varying number of densities, e.g., \code{c(.2, .5, .8)}.
#' @param architecture (int vector) Network architectures, e.g., \code{c(random = 1, small_world = 2, scale_free = 3, empirical = 4)}.
#'
#' @return A matrix representing the factorial design of the study, where each row is a design cell.
#'
#' @export
#'
build_design <- function(participants, nodes, densities, architectures)
{
    # The design matrix.
    design = matrix(NA, 0, 4)

	for (a in 1:length(participants))
	{
		for (b in 1:length(nodes))
		{
			for (c in 1:length(densities))
			{
			    for(d in 1:length(architectures))
			    {
                    design = rbind(design, c(participants[a], nodes[b], densities[c], architectures[d]))
			    }
			}
		}
	}
    colnames(design) <- c('participants', 'nodes', 'density', 'architecture')

    # User feedback.
    cat(
        '-> Design generated:',
        nrow(design), 'cells',
        '(each row in the matrix stands for one design cell).',
        '\n\n'
    )

    return(design)
}
