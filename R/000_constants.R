# This file holds constants used throughout the entire package.



# Package graphs ----------------------------------------------------------



GRAPHS <- list(
    supported = c(
        "random", 
        "smallworld", 
        "scalefree"
    )
)



# Package models ----------------------------------------------------------



MODELS.CROSS.SECTIONAL <- list(
    supported = c(
        "ggm", 
        "ising"
    )
)



MODELS.TIME.SERIES <- list(
    supported = c()
)



# Package classes  --------------------------------------------------------



CLASSES <- list(
    graph = c(
        "np.graph" # Information about the generated unweighted graph.
    ),
    
    model = c(
        "np.cs.model", # True model for cross-sectional data. 
        "np.ts.model", # True model for time-series data.
        "np.cs.fit",   # Estimated model for cross-sectional data.
        "np.ts.fit"    # Estimated model for time-series data.
    ),

    data = c(
        "np.data" # Information about the generated data.
    ),

    outcome = c(
        "np.cs.out", # Outcome from comparison between true and fit (i.e., cross-sectional).
        "np.ts.out"  # Outcome from comparison between true and fit (i.e., time-series).
    ),

    procedure = c(
        "np.sim.cell", # Information about a single cell, ran only one time.
        "np.sim.repl"  # Information about a single cell, ran multiple time (i.e., replicated).
    ),

    setup = c(
        "np.config", # Information about the config of a single cell.
        "np.design" # Information about the factorial design of design levels.
    )
)



# Metadata ----------------------------------------------------------------



POSITIVE.EDGE.COLOR <- "#3F51B5"

NEGATIVE.EDGE.COLOR <- "#F44336"
