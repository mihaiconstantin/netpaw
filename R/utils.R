# In this file we are storing general utils.



# Get the number of nodes from a graph or weighted matrix.
get.number.nodes <- function(graph) {
    # Determine the dimensions.
    dimensions = dim(graph)
    
    # Check the dimensions.
    if(dimensions[1] != dimensions[2]) stop('Wrong type of input: the graph dimensions do not match.')
    
    # Set the number of nodes.
    return(dimensions)
}
