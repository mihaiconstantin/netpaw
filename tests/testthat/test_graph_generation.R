context("True graph generation")



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Tests related to the `Graph` class --------------------------------------
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

test_that("`Graph` class is not instantiable", {
    # Expectation.
    expect_error(Graph$new(), ..ERRORS..$non.instantiable.class)
})



test_that("graph aliases inherit from `Graph` class", {
    # Preparation.
    for(alias in Graph$..ALIASES..) {
        test <- "Graph" %in% alias$class$get_inherit()$classname

        # Expectation.
        expect_equal(test, TRUE)
    }
})



test_that("graph aliases have implementation", {
    # Preparation.
    for(alias in Graph$..ALIASES..) {
        test <- is.null(alias$class$public_methods$generator)

        # Expectation.
        expect_equal(test, FALSE)
    }
})



test_that("undirected graph aliases return a symmetric matrix", {
    # Preparation.
    for(alias in Graph$..ALIASES..) {
        # If present ensure that the `directed` argument is set to `FALSE`.
        if(!is.null(alias$example.args$directed)) {
            alias$example.args$directed <- FALSE
        }
        
        # Generate true graph.
        graph <- do.call(alias$class$new, alias$example.args)
        
        test <- isSymmetric(graph$graph)

        # Expectation.
        expect_equal(test, TRUE)
    }
})



test_that("graph aliases have 0 on main diagonal", {
    # Preparation.
    for(alias in Graph$..ALIASES..) {
        # Generate true graph.
        graph <- do.call(alias$class$new, alias$example.args)
        
        test <- sum(diag(graph$graph))

        # Expectation.
        expect_equal(test, 0)
    }
})



test_that("`gen.graph` can start the factory for all graph aliases", {
    # Preparation.
    for(alias in Graph$..ALIASES..) {
        # Prepare arguments.
        args <- c(graph.type = alias$name, alias$example.args)

        # True graph.
        factory <- do.call(gen.graph, args)

        test <- is.matrix(factory$first$graph)

        # Expectation.
        expect_equal(test, TRUE)
    }
})



test_that("`gen.graph` fails on unsupported graph type", {
    # Expectation.
    expect_error(gen.graph("unknown"), ..ERRORS..$unsupported.type)
})
