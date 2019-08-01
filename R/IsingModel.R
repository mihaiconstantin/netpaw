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
#   - M.A. Constantin                                                     #
#                                                                         #
# File description:                                                       #
#   - generated from template on 2019-07-31 15:15:49 by M.A. Constantin   #
#   - you may add your description here                                   #
#                                                                         #
# Seminal paper:                                                          #
#   - https://dx.doi.org/srep05918                                        #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include Generator.R Sampler.R Estimator.R Comparator.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Generating Ising model parameters ---------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

IsingGenerator <- R6::R6Class("IsingGenerator",
    inherit = CrossSectionalGenerator,


    private = list(
        ..generator = function(positive.edge.ratio = 1, mean = 0, sd = 1) {
            # Prepare the weights matrix.
            weights <- private$..graph$adjacency

            # Sample a vector of positive and negative edges matching the positive edge ration specified.
            ratio <- private$..sample.positive.parameter.ratio()

            # Sample the parameters.
            parameters <- abs(rnorm(self$number.parameters, mean, sd)) * ratio

            # Apply the parameters to the network structure.
            weights[upper.tri(weights)] <- weights[upper.tri(weights)] * parameters
            weights[lower.tri(weights)] <- t(weights)[lower.tri(weights)]

            # Create the threshold parameters.
            thresholds <- -abs(rnorm(private$..graph$number.nodes, colSums(weights) / 2, abs(colSums(weights) / 6)))

            return(list(
                weights = weights,
                thresholds = thresholds
            ))
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Sampling data based on Ising model parameters ---------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

IsingSampler <- R6::R6Class("IsingSampler",
    inherit = Sampler,


    private = list(
        ..sampler = function(n, nIter = 100, method = "MH") {
            # Sample data.
            data <- IsingSampler::IsingSampler(n, private$..model$weights, private$..model$details$thresholds, nIter = nIter, method = method)

            return(data)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Estimating Ising model from data ----------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

IsingEstimator <- R6::R6Class("IsingEstimator",
    inherit = Estimator,


    private = list(
        ..frequentist = function(and = TRUE, gamma = 0.25, lower.bound.lambda = NA) {
            # Estimate the model.
            model <- IsingFitEssential(private$..data$dataset, AND = and, gamma = gamma, lowerbound.lambda = lower.bound.lambda)

            return(model)
        },


        ..bayesian = function(argument.1, argument.2) {
            # Implementation here or throw the following error if you wish to leave unimplemented:
            stop(..ERRORS..$not.implemented)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Comparing Ising models --------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

IsingComparator <- R6::R6Class("IsingComparator",
    inherit = Comparator,


    private = list(
        ..comparator = function(argument.1, argument.2) {
            # Add your implementation here.
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Aliases for Ising model -------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Generator alias.

Generator$..ALIASES..$ising <- list(
    name = "ising",
    class = IsingGenerator,

    # Add a set of example arguments used to automatically test your implementation.
    example.args = list(
        argument.1 = 1, 
        argument.2 = 2
    )
)



# Sampler alias.

Sampler$..ALIASES..$ising <- list(
    name = "ising",
    class = IsingSampler,

    # Add a set of example arguments used to automatically test your implementation.
    example.args = list(
        argument.1 = 1, 
        argument.2 = 2
    )
)



# Estimator alias.

Estimator$..ALIASES..$ising <- list(
    name = "ising",
    class = IsingEstimator,

    # Add a set of example arguments used to automatically test your implementation.
    example.args = list(
        frequentist = list(
            argument.1 = 1, 
            argument.2 = 2
        ),
        bayesian = list(
            argument.1 = 1, 
            argument.2 = 2
        )
    )
)



# Comparator alias.

Comparator$..ALIASES..$ising <- list(
    name = "ising",
    class = IsingComparator,

    # Add a set of example arguments used to automatically test your implementation.
    example.args = list(
        argument.1 = 1, 
        argument.2 = 2
    )
)



# End of file.
