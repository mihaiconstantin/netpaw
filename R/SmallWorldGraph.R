# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                              _                                           #
#                             | |                                          #
#                 _ __    ___ | |_  _ __    __ _ __      __                #
#                | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /                #
#                | | | ||  __/| |_ | |_) || (_| | \ V  V /                 #
#                |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                  #
#                                  | |                                     #
#                                  |_|                                     #
# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                          #
# File contributors:                                                       #
#   - M.A Constantin                                                       #
#                                                                          #
# File description:                                                        #
#   - generated from template on 2019-06-04 16:46:51 by M.A Constantin     #
#   - you may add your description here                                    #
#                                                                          #
# Seminal paper:                                                           #
#   - https://dx.doi.org/10.1038/30918                                     #
#                                                                          #
# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Generating SmallWorld model parameters ------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

SmallWorldGraph <- R6::R6Class("SmallWorldGraph",
    inherit = Graph,


    private = list(
        ..generator = function(nodes, neighborhood, p.rewire) {
            # Generate the graph.
            graph <- as.matrix(igraph::get.adjacency(igraph::sample_smallworld(dim = 1, size = nodes, nei = neighborhood, p = p.rewire)))

            return(graph)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Alias for SmallWorld graph ------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# "SmallWorld" graph is known as "smallworld".

Graph$..ALIASES..$smallworld <- list(
    name = "smallworld",
    class = SmallWorldGraph,
    example.args = list(
        nodes = 10, 
        neighborhood = 2, 
        p.rewire = .5
    )
)



# End of file.
