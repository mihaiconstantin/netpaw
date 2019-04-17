context("True graph generation")



test_that("`gen.graph` inherits from the correct class", {
    true.graph <- gen.graph("random", 5, p = .5)
    
    expect_is(true.graph, "npgraph")
})
