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
#   - this file contains specific model classes that inherit from a specialized abstract class of a model (e.g., `ModelCrossSectional`)   #
#                                                                                                                                         #
# Classes/ functions/ methods:                                                                                                            #
#   - IsingModel (R6 class)                                                                                                               #
#   - GgmModel (R6 class)                                                                                                                 #
#                                                                                                                                         #
# Additional information:                                                                                                                 #
#   - steps to implement a model:                                                                                                         #
#       1. add a new R6 class that inherits from `ModelCrossSectional` or `ModelTimeSeries`                                               #
#       2. overwrite the public `generator` method and add the model generating algorithm                                                 #
#           a. ensure that the algorithm returns a list of the following format:                                                          #
#               i.  `weights` -> a standard `R` matrix; symmetric if generating a cross-sectional model                                   #
#               ii. ... -> whatever other relevant information                                                                            #
#       3. add an alias for the R6 class to the `Model$..ALIASES..` list                                                                  #
#           a. the list of aliases must contain the following named elements:                                                             #
#               i.   `name` -> the name of the implemented model in lowercase as a string                                                 #
#               ii.  `class` -> the name of the implemented class without quotation marks (i.e., the R6 generator)                        #
#               iii. `example.args` -> a list of named arguments and arbitrary values as in the signature of the `generator`              #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# Start implementing below this line.



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Ising model child class -------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


IsingModel <- R6::R6Class("IsingModel",
    inherit = ModelCrossSectional,


    public = list(
        generator = function(positive.edge.ratio = 1, mean = 0, sd = 1) {
            # Prepare the weights matrix.
            weights <- self$graph$graph
                        
            # Sample a vector of positive and negative edges matching the positive edge ration specified.
            ratio <- private$..sample.positive.parameter.ratio()

            # Sample the parameters.
            parameters <- abs(rnorm(self$number.parameters, mean, sd)) * ratio

            # Apply the parameters to the network structure.
            weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
            weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

            # Create the threshold parameters.
            thresholds <- -abs(rnorm(self$graph$nodes, colSums(weights) / 2, abs(colSums(weights) / 6)))

            return(list(
                weights = weights, 
                thresholds = thresholds
            ))
        }
    )
)

# Register alias.

Model$..ALIASES..$ising <- list(
    name = "ising",
    class = IsingModel,
    example.args = list(
        positive.edge.ratio = .5, mean = 0, sd = 1
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# GGM model child class ---------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GgmModel <- R6::R6Class("GgmModel",
    inherit = ModelCrossSectional,


    public = list(
        # Credits to Yin and Li (2011) and bootnet::genGGM().
        generator = function(positive.edge.ratio = 1, min = 0.5, max = 1, constant = 1.5) {
            # Prepare the weights matrix.
            weights <- self$graph$graph

            # Sample a vector of positive and negative edges matching the positive edge ration specified.
            ratio <- private$..sample.positive.parameter.ratio(flip = TRUE)

            # Sample the parameters.
            parameters <- runif(self$number.parameters, min, max) * ratio

            # Apply the parameters to the network structure.
            weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
            weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

            # Create the precision matrix.
            diag(weights) <- constant * rowSums(abs(weights))
            diag(weights) <- ifelse(diag(weights) == 0, 1, diag(weights))
            weights <- weights / diag(weights)[row(weights)]
            weights <- (weights + t(weights)) / 2

            # Create the partial correlation matrix from the precision matrix as `qgraph::wi2net`.
            weights <- -cov2cor(weights)
            diag(weights) <- 0

            return(list(
                weights = weights
            ))
        }
    )
)

# Register alias.

Model$..ALIASES..$ggm <- list(
    name = "ggm",
    class = GgmModel,
    example.args = list(
        positive.edge.ratio = 0.5, min = 0.5, max = 1, constant = 1.5
    )
)



# Stop implementing at this line.
