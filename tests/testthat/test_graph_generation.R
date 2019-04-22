context("True graph generation")



test_that("`gen.graph` inherits from the correct class", {
    true.graph <- gen.graph("random", 5, p = .5)
    
    expect_is(true.graph, "npgraph")
})



test_that("`gen.graph` uses the correct graph generator", {
    expect_equal(gen.graph("random", 5, p = .5)$type, "random")

    expect_equal(gen.graph("smallworld", 5, neighborhood = 2, p = .5)$type, "smallworld")
    
    expect_equal(gen.graph("scalefree", 5, edges = 2, attachment = 2)$type, "scalefree")
    
    expect_error(gen.graph("unknown", "random", 5, p = .5)$type, "Unsupported graph type. Please request it at `m.a.constantin@uvt.nl`.")
})
