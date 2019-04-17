context("True graph generation")



test_that("`get.graph` inherits from the correct class", {
    true.graph <- get.graph("random", 5, p = .5)
    
    expect_is(true.graph, "netpowerGraph")
})
