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
#   - generated from template on 2019-06-04 13:13:18 by M.A Constantin    #
#   - you may add your description here                                   #
#                                                                         #
# Seminal paper:                                                          #
#   - https://dx.doi.org/10.1126/science.286.5439.509                     #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Generating Random model parameters --------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

RandomGraph <- R6::R6Class("RandomGraph",
    inherit = Graph,


    private = list(
        ..generator = function(nodes, p, directed = FALSE) {
            # Generate the graph.
            graph <- as.matrix(igraph::get.adjacency(igraph::sample_gnp(n = nodes, p = p, directed = directed)))
            
            return(graph)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Alias for Random graph --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# "Random" graph is known as "random".

Graph$..ALIASES..$random <- list(
    name = "random",
    class = RandomGraph,
    example.args = list(
        nodes = 10, 
        p = .5, 
        directed = FALSE
    )
)



# End of file.
