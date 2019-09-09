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
#   - unit tests for estimating model parameters                          #
#                                                                         #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



# Testing that the outputs of newly added models match the expectations of the framework.
context("Model estimation")



# Test that all estimators correctly construct models.
test_that("estimators can construct models", {
    for(est in Estimator$..ALIASES..) {
        # Generate a true model.
        true <- generate.model(type = est$name, graph = RandomGraph$new(5, .3))

        # Generate data.
        data <- generate.data(type = est$name, model = true$model, n = 300)

        # Build the arguments.
        args <- c(type = est$name, data = data$data, est$example.args$frequentist, thinking = "frequentist")

        # Call the frequentist estimator.
        result <- do.call("estimate", args)

        # Expect that the model weights is a matrix.
        expect_equal(is.matrix(result$model$weights), TRUE)

        # Expect that the model details is a list or null.
        expect_equal(is.list(result$model$details) || is.null(result$model$details), TRUE)
    }
})



# Test the model generator wrapper.
test_that("estimator wrapper returns correct type", {
    # Generate a `ggm` model.
    true <- generate.model(type = "ggm", graph = RandomGraph$new(nodes = 10, p = .1))

    # Generate data.
    data <- generate.data(type = "ggm", model = true$model, n = 100)

    # Build the arguments.
    args <- c(type = Estimator$..ALIASES..$ggm$name, data = data$data, Estimator$..ALIASES..$ggm$example.args$frequentist, thinking = "frequentist")

    # Call the frequentist estimator.
    result <- do.call("estimate", args)

    # Expect that the model is of correct type.
    expect_equal(result$options$meta$type == "GgmEstimator", TRUE)

    # Generate an unregistered model.
    expect_error(estimate("unknown"), ..ERRORS..$unsupported.type)
})



# End of file.
