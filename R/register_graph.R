# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                             _                                                                           #
#                                                            | |                                                                          #
#                                                _ __    ___ | |_  _ __    __ _ __      __                                                #
#                                               | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /                                                #
#                                               | | | ||  __/| |_ | |_) || (_| | \ V  V /                                                 #
#                                               |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                                                  #
#                                                                 | |                                                                     #
#                                                                 |_|                                                                     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                                                                                         #
# File contributors:                                                                                                                      #
#   - M.A. Constantin                                                                                                                     #
#                                                                                                                                         #
# File description:                                                                                                                       #
#   - this file contains specific graph classes that inherit from the abstract class `Graph`                                              #
#                                                                                                                                         #
# Classes/ functions/ methods:                                                                                                            #
#   - RandomGraph (R6 class)                                                                                                              #
#   - SmallWorldGraph (R6 class)                                                                                                          #
#   - ScaleFreeGraph (R6 class)                                                                                                           #
#                                                                                                                                         #
# Additional information:                                                                                                                 #
#   - steps to implement a graph:                                                                                                         #
#       1. add a new R6 class that inherits from the `Graph` abstract class                                                               #
#       2. overwrite the public `generator` method and add the graph generating algorithm                                                 #
#           a. ensure that the algorithm returns a standard `R` matrix object                                                             #
#       3. add an alias for the R6 class to the `Graph$..ALIASES..` list                                                                  #
#           a. the list of aliases must contain the following named elements:                                                             #
#               i.   `name` -> the name of the implemented graph in lowercase as a string                                                 #
#               ii.  `class` -> the name of the implemented class without quotation marks (i.e., the R6 generator)                        #
#               iii. `example.args` -> a list of named arguments and arbitrary values as in the signature of the `generator`              #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# Start implementing below this line.



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Random graph child class ------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

RandomGraph <- R6::R6Class("RandomGraph",
    inherit = Graph,
    
    
    public = list(
        generator = function(nodes, p, directed = FALSE) {
            # Generate the graph.
            graph <- as.matrix(igraph::get.adjacency(igraph::sample_gnp(n = nodes, p = p, directed = directed)))
            
            return(graph)
        }
    )
)

# Register alias.

Graph$..ALIASES..$random <- list(
    name = "random",
    class = RandomGraph,
    example.args = list(
        nodes = 10, p = .5, directed = FALSE
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Small world graph child class -------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

SmallWorldGraph <- R6::R6Class("SmallWorldGraph",
    inherit = Graph,
    
    
    public = list(
        generator = function(nodes, neighborhood, p.rewire) {
            # Generate the graph.
            graph <- as.matrix(igraph::get.adjacency(igraph::sample_smallworld(dim = 1, size = nodes, nei = neighborhood, p = p.rewire)))

            return(graph)
        }
    )
)

# Register alias.

Graph$..ALIASES..$smallworld <- list(
    name = "smallworld",
    class = SmallWorldGraph,
    example.args = list(
        nodes = 10, neighborhood = 2, p.rewire = .5
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Scale free graph child class --------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

ScaleFreeGraph <- R6::R6Class("ScaleFreeGraph",
    inherit = Graph,
    
    
    public = list(
        generator = function(nodes, attachment, edges, directed = FALSE) {
            # Generate the graph.
            graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(n = nodes, power = attachment, m = edges, directed = directed)))
            
            return(graph)
        }
    )
)

# Register alias.

Graph$..ALIASES..$scalefree <- list(
    name = "scalefree",
    class = ScaleFreeGraph,
    example.args = list(
        nodes = 10, attachment = 2, edges = 2, directed = FALSE
    )
)



# Stop implementing at this line.
