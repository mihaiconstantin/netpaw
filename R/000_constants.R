# This file holds constants used throughout the entire package.



# Metadata ----------------------------------------------------------------

POSITIVE.EDGE.COLOR = "#3F51B5"
NEGATIVE.EDGE.COLOR = "#F44336"



# Package graphs ----------------------------------------------------------

UNDIRECTED.UNWEIGHTED.GRAPHS = list(
    supported = c(
        "random", 
        "smallworld", 
        "scalefree"
    )
)



# Package models ----------------------------------------------------------

CROSS.SECTIONAL.MODELS = list(
    supported = c(
        "ggm", 
        "ising"
    )
)

TIME.SERIES.MODELS = list(
    supported = c()
)
