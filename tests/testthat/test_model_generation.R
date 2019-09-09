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
#   - unit tests for generating true model parameters                     #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# Testing that the outputs of newly added models match the expectations of the framework.
context("Model generation")



# Test that all generators correctly construct models.
test_that("generators can construct models", {
    for(gen in Generator$..ALIASES..) {
        # Build the arguments.
        args <- c(type = gen$name, gen$example.args)

        # Add a graph if the current generator is a cross-sectional generator.
        if(gen$class$get_inherit()$classname == "CrossSectionalGenerator") {
            # Create a graph.
            graph <- generate.graph(type = "random", nodes = 10, p = .3)

            # Append the graph to the arguments.
            args <- c(args, graph = graph)
        }

        # Call the generator.
        result <- do.call("generate.model", args)

        # Expect that the model weights is a matrix.
        expect_equal(is.matrix(result$model$weights), TRUE)

        # Expect that the model details is a list or null.
        expect_equal(is.list(result$model$details) || is.null(result$model$details), TRUE)

        # Expectations about the cross-sectional models.
        if(gen$class$get_inherit()$classname == "CrossSectionalGenerator") {
            # Expect that all cross-sectional generators output symmetrical weights.
            expect_equal(isSymmetric(result$model$weights), TRUE)

            # Expect that all cross-sectional generators output weights with 0 on main diagonal.
            expect_equal(all(diag(result$model$weights) == 0), TRUE)
        }
    }
})



# Test the model generator wrapper.
test_that("generator wrapper returns correct type", {
    # Generate a `ggm` model.
    result <- generate.model(type = "ggm", graph = RandomGraph$new(nodes = 10, p = .1))

    # Expect that the model is of correct type.
    expect_equal(result$options$meta$type == "GgmGenerator", TRUE)

    # Generate an unregistered model.
    expect_error(generate.model("unknown"), ..ERRORS..$unsupported.type)
})



# End of file.
