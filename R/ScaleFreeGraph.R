# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                              _                                          #
#                             | |                                         #
#                 _ __    ___ | |_  _ __    __ _ __      __               #
#                | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /               #
#                | | | ||  __/| |_ | |_) || (_| | \ V  V /                #
#                |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                 #
#                                  | |                                    #
#                                  |_|                                    #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                         #
# File contributors:                                                      #
#   - M.A Constantin                                                      #
#                                                                         #
# File description:                                                       #
#   - generated from template on 2019-06-04 16:54:33 by M.A Constantin    #
#   - you may add your description here                                   #
#                                                                         #
# Seminal paper:                                                          #
#   - https://dx.doi.org/10.1126/science.1173299                          #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include Graph.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Generating ScaleFree model parameters -----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

ScaleFreeGraph <- R6::R6Class("ScaleFreeGraph",
    inherit = Graph,


    private = list(
        ..generator = function(nodes, attachment, edges, directed = FALSE) {
            # Generate the graph.
            graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(n = nodes, power = attachment, m = edges, directed = directed)))

            return(graph)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Alias for ScaleFree graph -----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# "ScaleFree" graph is known as "scalefree".

Graph$..ALIASES..$scalefree <- list(
    name = "scalefree",
    class = ScaleFreeGraph,

    # Add a set of example arguments used to automatically test your implementation.
    example.args = list(nodes = 10, attachment = 2, edges = .5, directed = FALSE)
)



# End of file.
