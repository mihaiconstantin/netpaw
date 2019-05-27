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
#   - this file contains specific sampler classes that inherit from the abstract class `Sampler`                                          #
#                                                                                                                                         #
# Classes/ functions/ methods:                                                                                                            #
#   - IsingSampler (R6 class)                                                                                                             #
#   - GgmSampler (R6 class)                                                                                                               #
#                                                                                                                                         #
# Additional information:                                                                                                                 #
#   - steps to implement a sampler:                                                                                                       #
#       1. add a new R6 class that inherits from the `Sampler` abstract class                                                             #
#       2. overwrite the public `sampler` method and add the data generating algorithm                                                    #
#           a. ensure that the algorithm returns a standard `R` matrix object                                                             #
#       3. add an alias for the R6 class to the `Sampler$..ALIASES..` list                                                                #
#           a. the list of aliases must contain the following named elements:                                                             #
#               i.   `name` -> the name of the implemented sampler in lowercase as a string                                               #
#               ii.  `class` -> the name of the implemented class without quotation marks (i.e., the R6 generator)                        #
#               iii. `example.args` -> a list of named arguments and arbitrary values as in the signature of the `sampler`                #
#                                                                                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# Start implementing below this line.



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Ising model sampler child class -----------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

IsingSampler <- R6::R6Class("IsingSampler",
    inherit = Sampler,
    
    
    public = list(
        sampler = function(n, nIter = 100, method = "MH") {
            # Sample data.
            data <- IsingSampler::IsingSampler(n, self$model$model$weights, self$model$model$thresholds, nIter = nIter, method = method)

            return(data)
        }
    )
)

# Register alias.

Sampler$..ALIASES..$ising <- list(
    name = "ising",
    class = IsingSampler,
    example.args = list(
        n = 100, nIter = 100, method = "MH"
    ) 
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Ggm model sampler child class -------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


GgmSampler <- R6::R6Class("GgmSampler",
    inherit = Sampler,
    
    
    public = list(
        sampler = function(n, levels = 5) {
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

# Register alias.

Sampler$..ALIASES..$ggm <- list(
    name = "ggm",
    class = GgmSampler,
    example.args = list(
        n = 100, levels = 5
    ) 
)



# Stop implementing at this line.
