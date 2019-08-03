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
            return("To be added.")
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
