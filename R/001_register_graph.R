# In this file we are registering UNWEIGHTED GRAPHS.



# SCOPE: can be modified by collaborators.



# MODIFICATION INSTRUCTIONS: to be added...



# Graphs ------------------------------------------------------------------



# Random graph.
GRAPHS$random <- list(
    # The name of the graph.
    name = "random",

    # The arguments with some predefined constraints. 
    args = list(
        nodes = list(
            name = "nodes",
            type = "int",
            range = c(1, 100)
        ),
        p = list(
            name = "p",
            type = "double",
            range = c(0, 1) 
        ),
        directed = list(
            name = "directed",
            type = "bool",
            range = c(FALSE, TRUE)
        )
    ),

    # The graph generator.
    generator = function(nodes, p, directed = FALSE) {
        # Generate the graph.
        graph <- as.matrix(igraph::get.adjacency(igraph::sample_gnp(n = nodes, p = p, directed = directed)))
        
        return(graph)
    }
)



# Smallworld graph.
GRAPHS$smallworld <- list(
    # The name of the graph.
    name = "smallworld",

    # The arguments with some predefined constraints.
    args = list(
        nodes = list(
            name = "nodes",
            type = "int",
            range = c(1, 100)
        ),
        neighborhood = list(
            name = "neighborhood",
            type = "int",
            range = c(0, 3)
        ),
        p.rewire = list(
            name = "p.rewire",
            type = "double",
            range = c(0, 1)
        )
    ),

    # The graph generator.
    generator = function(nodes, neighborhood, p.rewire) {
        # Generate the graph.
        graph <- as.matrix(igraph::get.adjacency(igraph::sample_smallworld(dim = 1, size = nodes, nei = neighborhood, p = p.rewire)))

        return(graph)
    }
)



# Scalefree graph.
GRAPHS$scalefree <- list(
    # The name of the graph.
    name = "scalefree",

    # The arguments with some predefined constraints.
    args = list(
        nodes = list(
            name = "nodes",
            type = "int",
            range = c(1, 100)
        ),
        attachment = list(
            name = "attachment",
            type = "int",
            range = c(0, 3)
        ),
        edges = list(
            name = "edges",
            type = "int",
            range = c(0, 3)
        ),
        directed = list(
            name = "directed",
            type = "bool",
            range = c(FALSE, TRUE)
        )
    ),

    # The graph generator.
    generator = function(nodes, attachment, edges, directed = FALSE) {
        # Generate the graph.
        graph <- as.matrix(igraph::get.adjacency(igraph::sample_pa(n = nodes, power = attachment, m = edges, directed = directed)))
        
        return(graph)
    }
)
