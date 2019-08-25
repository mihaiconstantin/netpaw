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
#   - contains a class that defines a model comparison result             #
#   - meant for internal use only                                         #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Result class ------------------------------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

Result <- R6::R6Class("Result",

    private = list(
        ..true = NULL,
        ..estimated = NULL,
        ..result = NULL,


        # This way of computing the simulation result should be applicable to all types of simulations.
        ..compute.basic.result = function() {
            # Get the true and estimated edges.
            true <- private$..true$weights[upper.tri(private$..true$weights)]
            esti <- private$..estimated$weights[upper.tri(private$..estimated$weights)]

            # Check if perfect recovery in terms of presence/ absence of an edge.
            perfect <- all((true == 0) == (esti == 0))

            # Check the size of both graphs (i.e., useful of nodes were dropped due to restamping issues).
            equal.size <- dim(private$..true$weights)[1] == dim(private$..estimated$weights)[1]

            # True/ false | positive/ negative rates.
            TP <- sum(true != 0 & esti != 0)
            FP <- sum(true == 0 & esti != 0)
            TN <- sum(true == 0 & esti == 0)
            FN <- sum(true != 0 & esti == 0)

            # Compound indicators based on the rates.
            sensitivity <- TP / (TP + FN) # Aka power.
            specificity <- TN / (TN + FP)
            type.one    <- FP / (FP + TN)
            type.two    <- FN / (TP + FN)

            # Compute type S error rate.
            # TODO: Implement type S errors.
            type.s <- "not yet implemented"

            # Compute type M error rate.
            # TODO: Implement type M errors.
            type.m <- "not yet implemented"

            # Edge weights correlation.
            correlation <- ifelse(equal.size && ((var(true) != 0) && (var(esti) != 0)), cor(true, esti), NA)

            # Density for true and estimated graphs.
            density.true = sum(true != 0) / length(true)
            density.esti = sum(esti != 0) / length(esti)

            # Return as a list.
            return(list(
                perfect                 = perfect,
                true.positive           = TP, 
                false.positive          = FP, 
                true.negative           = TN, 
                false.negative          = FN,
                sensitivity             = sensitivity,
                specificity             = specificity,
                type.one                = type.one,
                type.two                = type.two,
                type.s                  = type.s,
                type.m                  = type.m,
                edge.correlation        = correlation,
                density.true.model      = density.true,
                density.estimated.model = density.esti,
                equal.size              = equal.size
            ))
        },


        # Can be overwritten later if advanced computations are necessary.
        ..compute.result = function() {
            return(private$..compute.basic.result())
        }
    ),


    public = list(
        # Constructor.
        initialize = function(true, estimated) {
            # Set the fields. Note that true` and `estimated` are expected to be of `Model` type.
            # Since this is not an exported class (i.e., API) we do not perform type checks.
            private$..true <- true
            private$..estimated <- estimated

            # Compute the result which will be known as the simulation outcome.
            private$..result <- private$..compute.result()
        }
    ),


    active = list(
        result = function() {
            return(private$..result)
        }
    )
)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Example of extending the class for specialized results ------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# SpecialResult <- R6::R6Class("SpecialResult",
#     inherit = Result,
# 
#     private = list(
#         ..compute.results = function() {
#             return(paste("from", private$..compute.basic.results(), "to special results"))
#         }
#     )
# )



# End of file.
