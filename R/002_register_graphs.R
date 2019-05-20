# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
#                                                                                                                                         #
# In this file we are registering UNWEIGHTED GRAPHS.                                                                                      #
#                                                                                                                                         #
# The structure of the file is as follows:                                                                                                #
#   a. starts with a generic prototype of what a graph represents -> aka parent class (i.e., `Graph` in `001_Graph.R`)                    #
#   b. specific graph implementations inherit and respect the parent -> aka child class (e.g., `RandomGraph` in `002_register_graphs.R`)  #
#   c. a factory abstracts away the generation of specific graph implementation -> aka factory class (i.e., `GraphFactory`)               #
#   d. a wrapper around the factory allows users to generate graphs -> aka exported wrapper (i.e., `gen.graph`)                           #
#                                                                                                                                         #
# Note for adding new graphs:                                                                                                             #
#   1. add a new graph child class (i.e., see point b.)                                                                                   #
#   2. overwrite the `generator` public method of parent class to return a matrix                                                         #
#       a. on the top of the generator add the following line of code to ensure pretty argument names                                     #
#           -> private$..options <- as.list(match.call())[-1]                                                                             #
#   3. register the graph name and associated implementation under `Graph$..ALIASES...`                                                   #
#   4. add example of named arguments so the new implementation can be automatically tested                                               #
#   5. run the tests                                                                                                                      #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #    



# Start implementing graphs below this line.



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Random graph child class ------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

RandomGraph <- R6::R6Class("RandomGraph",
    inherit = Graph,
    
    
    public = list(
        generator = function(nodes, p, directed = FALSE) {
            # Match the options intelligently. Ideally I should abstract this in the parent class. Not sure how to go about it...
            private$..options <- as.list(match.call())[-1]
            
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
            # Match the options intelligently. Ideally I should abstract this in the parent class. Not sure how to go about it...
            private$..options <- as.list(match.call())[-1]

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
    args = list(
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
            # Match the options intelligently. Ideally I should abstract this in the parent class. Not sure how to go about it...
            private$..options <- as.list(match.call())[-1]

            # Generate the graph.
            graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(n = nodes, power = attachment, m = edges, directed = directed)))
            
            return(graph)
        }
    )
)

# Register alias.

Graph$..ALIASES..$scalefree <- list(
    name = "smallworld",
    class = ScaleFreeGraph,
    args = list(
        nodes = 10, attachment = 2, edges = 2, directed = FALSE
    )
)



# Stop implementing graphs below this line.
