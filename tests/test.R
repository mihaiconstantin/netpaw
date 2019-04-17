# Progress:
#   - (OK) graph algorithms
#   - (progress - generting model parameters for GGM) model parameters
#   - (OK) data sampling
#   - (OK) estimation
#   - (CONTINUE) procedure
#   - results
#   - storage

# Source the script file(s).
source('R/utils.R')
source('R/graph.R')
source('R/model.R')
source('R/data.R')
source('R/estimation.R')
# source('R/procedure.R')
# source('R/results.R')
# source('R/storage.R')

# Run the relevant commands.

g = get.graph("random", 5, p = .5, positive.edge.ratio = .5)
plot(g)

m = get.model("ggm", "random", 3, p = .5, positive.edge.ratio = .5)
plot(m)

m
