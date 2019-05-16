context("True graph generation (001_graph.R)")



test_that("`gen.graph` result inherits from the correct class", {
    # Preparation.
    true.graph <- gen.graph("random", nodes = 5, p = .5)
    
    # Expectation.
    expect_is(true.graph, "npgraph")
})



test_that("all implemented graphs are supported", {
    # Preparation.
    for(graph in UNDIRECTED.UNWEIGHTED.GRAPHS[-1]) {
        test <- graph$name %in% UNDIRECTED.UNWEIGHTED.GRAPHS$supported

        # Expectation.
        expect_equal(test, TRUE)
    }
})



test_that("all supported graphs are implemented", {
    # Preparation.
    for(graph.type in UNDIRECTED.UNWEIGHTED.GRAPHS$supported) {
        test <- is.null(UNDIRECTED.UNWEIGHTED.GRAPHS[[graph.type]])

        # Expectation.
        expect_equal(test, FALSE)
    }
})



test_that("all registered graphs return a symmetric matrix", {
    # Preparation.
    for(graph in UNDIRECTED.UNWEIGHTED.GRAPHS[-1]) {
        # Generate some suitable parameters.
        parameters <- generate.arguments(graph$args)

        # Generate true graph.
        true.graph <- do.call(graph$generator, parameters)
        
        test <- isSymmetric(true.graph)

        # Expectation.
        expect_equal(test, TRUE)
    }
})



test_that("all registered graphs have 0 on main diagonal", {
    # Preparation.
    for(graph in UNDIRECTED.UNWEIGHTED.GRAPHS[-1]) {
        # Generate some suitable parameters.
        parameters <- generate.arguments(graph$args)

        # Generate true graph.
        true.graph <- do.call(graph$generator, parameters)
        
        test <- sum(diag(true.graph))

        # Expectation.
        expect_equal(test, 0)
    }
})



test_that("`gen.graph` uses the correct graph generator", {
    expect_equal(gen.graph("random", nodes = 5, p = .5)$type, "random")

    expect_equal(gen.graph("smallworld", nodes = 5, neighborhood = 2, p.rewire = .5)$type, "smallworld")
    
    expect_equal(gen.graph("scalefree", nodes = 5, edges = 2, attachment = 2)$type, "scalefree")
    
    expect_error(gen.graph("unknown", nodes = 5, p = .5)$type, "Unsupported graph type. Please request it at `m.a.constantin@uvt.nl`.")
})



test_that("`gen.graph` catches non-conformable argument(s)", {
    # With missing argument.
    expect_error(gen.graph("random", p = .5), "Non-conformable argument\\(s\\) provided. See the documentation.")

    # With argument out of bounds.
    expect_error(gen.graph("random", nodes = 5, p = -1), "Non-conformable argument\\(s\\) provided. See the documentation.")
})
