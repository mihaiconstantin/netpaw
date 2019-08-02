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
#   - generated from template on 2019-08-02 12:11:31 by M.A. Constantin   #
#   - you may add your description here                                   #
#                                                                         #
# Seminal paper:                                                          #
#   - https://dx.doi.org/10.1037/met0000167                               #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Includes.
#' @include Generator.R Sampler.R Estimator.R Comparator.R



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Generating Ggm model parameters ------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GgmGenerator <- R6::R6Class("GgmGenerator",
    inherit = CrossSectionalGenerator,


    private = list(
        ..generator = function(positive.edge.ratio = 1, min = 0.5, max = 1, constant = 1.5) {
            # Prepare the weights matrix.
            weights <- private$..graph$adjacency

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



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Sampling data based on Ggm model parameters ------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GgmSampler <- R6::R6Class("GgmSampler",
    inherit = Sampler,


    private = list(
        ..sampler = function(n, levels = 5) {
            # Sample data.
            data <- mvtnorm::rmvnorm(n, sigma = self$model$to.correlation())

            # Split the data into item steps.
            for (i in 1:ncol(data)) {
                data[, i] <- as.numeric(cut(data[, i], sort(c(-Inf, rnorm(levels - 1), Inf))))
            }

            return(data)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Estimating Ggm model from data -------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GgmEstimator <- R6::R6Class("GgmEstimator",
    inherit = Estimator,


    private = list(
        ..frequentist = function(argument.1, argument.2) {
            # Add your implementation here.
        },


        ..bayesian = function(argument.1, argument.2) {
            # Implementation here or throw the following error if you wish to leave unimplemented:
            stop(..ERRORS..$not.implemented)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Comparing Ggm models -----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

GgmComparator <- R6::R6Class("GgmComparator",
    inherit = Comparator,


    private = list(
        ..comparator = function(argument.1, argument.2) {
            # Add your implementation here.
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Aliases for Ggm model ----------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Generator alias.

Generator$..ALIASES..$ggm <- list(
    name = "ggm",
    class = GgmGenerator,

    # Add a set of example arguments used to automatically test your implementation.
    example.args = list(positive.edge.ratio = 1, min = 0.5, max = 1, constant = 1.5)
)



# Sampler alias.

Sampler$..ALIASES..$ggm <- list(
    name = "ggm",
    class = GgmSampler,

    # Add a set of example arguments used to automatically test your implementation.
    example.args = list(n, levels = 5)
)



# Estimator alias.

Estimator$..ALIASES..$ggm <- list(
    name = "ggm",
    class = GgmEstimator,

    # Add a set of example arguments used to automatically test your implementation.
    example.args =(
        frequentist = list(argument.1 = 1, argument.2 = 2), 
        bayesian = list(argument.1 = 1, argument.2 = 2)
    )
)



# Comparator alias.

Comparator$..ALIASES..$ggm <- list(
    name = "ggm",
    class = GgmComparator,

    # Add a set of example arguments used to automatically test your implementation.
    example.args = list(argument.1 = 1, argument.2 = 2)
)



# End of file.
