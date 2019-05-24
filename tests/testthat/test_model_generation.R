context("True model generation")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Tests related to the `Model` class --------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

test_that("`Model` class is not instantiable", {
    # Expectation.
    expect_error(Model$new(), ..ERRORS..$non.instantiable.class)
})



test_that("model aliases inherit from `Model` class", {
    # Preparation.
    for(alias in Model$..ALIASES..) {
        test <- "Model" %in% alias$class$get_inherit()$get_inherit()$classname

        # Expectation.
        expect_equal(test, TRUE)
    }
})



test_that("model aliases have implementation", {
    # Preparation.
    for(alias in Model$..ALIASES..) {
        test <- is.null(alias$class$public_methods$generator)

        # Expectation.
        expect_equal(test, FALSE)
    }
})



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Tests related to the `ModelCrossSectional` class ------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

test_that("`ModelCrossSectional` class is not instantiable", {
    # Preparation.
    graph <- do.call(Graph$..ALIASES..[[1]]$class$new, Graph$..ALIASES..[[1]]$example.args)

    # Expectation.
    expect_error(ModelCrossSectional$new(graph), ..ERRORS..$non.instantiable.class)
})



test_that("cross-sectional model aliases return a symmetric matrix with 0 on main diagonal", {
    # Preparation.
    for(model.alias in Model$..ALIASES..) {
        # Check that the model alias is indeed of cross-sectional type.
        if("ModelCrossSectional" %in% model.alias$class$get_inherit()$classname) {
            for(graph.alias in Graph$..ALIASES..) {
                # If present ensure that the `directed` argument is set to `FALSE`.
                if(!is.null(graph.alias$example.args$directed)) {
                    graph.alias$example.args$directed <- FALSE
                }

                # Generate the true graph.
                graph <- do.call(graph.alias$class$new, graph.alias$example.args)

                # Generate the true model.
                model <- do.call(model.alias$class$new, c(graph = graph, model.alias$example.args))

                # Expectations.
                expect_equal(isSymmetric(model$model$weights), TRUE)
                expect_equal(sum(diag(graph$graph)), 0)
            }
        }
    }
})



test_that("`gen.model` can start the factory for all cross-sectional model aliases", {
    # Preparation.
    for(model.alias in Model$..ALIASES..) {
        # Check that the model alias is indeed of cross-sectional type.
        if("ModelCrossSectional" %in% model.alias$class$get_inherit()$classname) {
            for(graph.alias in Graph$..ALIASES..) {
                # Generate the true graph.
                graph <- do.call(graph.alias$class$new, graph.alias$example.args)

                # Generate the true model.
                factory <- do.call(gen.model, c(model.type = model.alias$name, graph = graph, model.alias$example.args))

                # Expectation.
                expect_equal(is.matrix(factory$first$model$weights), TRUE)
            }
        }
    }
})



test_that("`gen.model` fails for all unsupported cross-sectional model aliases", {
    # Preparation.
    for(model.alias in Model$..ALIASES..) {
        # Check that the model alias is indeed of cross-sectional type.
        if("ModelCrossSectional" %in% model.alias$class$get_inherit()$classname) {
            for(graph.alias in Graph$..ALIASES..) {
                # Generate the true graph.
                graph <- do.call(graph.alias$class$new, graph.alias$example.args)

                # Expectation.
                expect_error(do.call(gen.model, c(model.type = "unknown", graph = graph, model.alias$example.args)), ..ERRORS..$unsupported.type)
            }
        }
    }   
})



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Tests related to the `ModelTimeSeries` class ----------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

test_that("`ModelTimeSeries` class is not instantiable", {
    # Preparation.
    graph <- do.call(Graph$..ALIASES..[[1]]$class$new, Graph$..ALIASES..[[1]]$example.args)

    # Expectation.
    expect_error(ModelTimeSeries$new(graph), ..ERRORS..$not.implemented)
})



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Very specialized tests for edge cases -----------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

test_that("`GgmModel` class `generator` matches `bootnet::genGGM`", {
    # Seed number.
    seed <- 1993

    # Generate via `bootnet`.
    set.seed(seed)
    model.bootnet.random <- bootnet::genGGM(10, graph = "random", p = .3, propPositive = .5)
    model.bootnet.smallworld <- bootnet::genGGM(10, graph = "smallworld", nei = 2, p = .3, propPositive = .5)
    
    # Generate via `netpaw`.
    set.seed(seed)
    model.netpaw.random <- GgmModel$new(graph = RandomGraph$new(10, .3), positive.edge.ratio = .5)
    model.netpaw.smallworld <- GgmModel$new(graph = SmallWorldGraph$new(10, 2, .3), positive.edge.ratio = .5)
    
    # Expectations.
    expect_equal(model.bootnet.random, model.netpaw.random$model$weights)
    expect_equal(model.bootnet.smallworld, model.netpaw.smallworld$model$weights)
})
