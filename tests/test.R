# Progress:
#   - (OK) graph algorithms
#   - (OK) model parameters
#   - (OK) data sampling
#   - (OK) estimation
#   - (progress) procedure
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

true.model = get.model('ggm', 'random', 10, p = .3, positive.edge.ratio = 1)
data = get.data(300, true.model)
esti.model = estimate.model(data)

par(mfrow = c(1, 2))
    graph = qgraph::qgraph(true.model$weights)
    qgraph::qgraph(esti.model$weights, layout = graph$layout)
par(mfrow = c(1, 1))

