# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                              _                                           #
#                             | |                                          #
#                 _ __    ___ | |_  _ __    __ _ __      __                #
#                | '_ \  / _ \| __|| '_ \  / _` |\ \ /\ / /                #
#                | | | ||  __/| |_ | |_) || (_| | \ V  V /                 #
#                |_| |_| \___| \__|| .__/  \__,_|  \_/\_/                  #
#                                  | |                                     #
#                                  |_|                                     #
# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                          #
# File contributors:                                                       #
#   - M.A. Constantin                                                      #
#                                                                          #
# File description:                                                        #
#   - contains an abstract R6 class that extends the generic Generator     #
#     defines an abstract cross-sectional generator                        #
#                                                                          #
# # # # #  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Specialized cross-sectional generator class -----------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

CrossSectionalGenerator <- R6::R6Class("CrossSectionalGenerator",
    inherit = Generator,


    private = list(
        ..graph = NULL,


        # Get a vector of negative and positive integers (i.e., 1 and -1) matching a specified sign proportion.
        ..sample.positive.parameter.ratio = function(flip = FALSE) {
            if(flip) {
                positive.ratio <- sample(c(-1, 1), self$number.parameters, TRUE, prob = c(private$..options$values$positive.edge.ratio, 1 - private$..options$values$positive.edge.ratio))
            } else {
                positive.ratio <- sample(c(-1, 1), self$number.parameters, TRUE, prob = c(1 - private$..options$values$positive.edge.ratio, private$..options$values$positive.edge.ratio))
            }

            return(positive.ratio)
        }
    ),


    public = list(
        initialize = function(graph, ...) {
            # Type check.
            assert.condition("Graph" %in% class(graph), ..ERRORS..$incorrect.object.type)

            # Set the graph via dependency injection.
            private$..graph <- graph

            # Call the parent constructor.
            super$initialize(...)
        }
    ),


    active = list(
        # Compute the number of parameters based on the injected graph object.
        number.parameters = function() {
            number.parameters <- (private$..graph$nodes * (private$..graph$nodes - 1)) / 2

            return(number.parameters)
        }
    )
)



# End of file.
