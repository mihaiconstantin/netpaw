context("True model generation")



test_that("`get.model` inherits from the correct class", {
    true.model <- get.model("ggm", "random", 5, p = .5)
    
    expect_is(true.model, "netpowerTrueModel")
})



test_that("`get.model` uses the correct model generator", {
    expect_equal(get.model("ggm", "random", 5, p = .5)$model, "ggm")
    
    expect_equal(get.model("ising", "random", 5, p = .5)$model, "ising")
    
    expect_error(get.model("unknown", "random", 5, p = .5)$model, "Unsupported model type. Please request it at `m.a.constantin@uvt.nl`.")
})



test_that("`model.ggm` matches `bootnet::genGGM`", {
    seed <- 1993
    
    # For random graphs.
    set.seed(seed)
    model.bootnet <- bootnet::genGGM(10, graph = "random", p = .3, propPositive = .5)
    
    set.seed(seed)
    model.netpower <- model.ggm("random", 10, p = .3, positive.edge.ratio = .5)
    
    expect_equal(model.bootnet, model.netpower$weights)
    
    # For small worlds.
    set.seed(seed)
    model.bootnet <- bootnet::genGGM(10, graph = "smallworld", nei = 2, p = .3, propPositive = .5)
    
    set.seed(seed)
    model.netpower <- model.ggm("smallworld", 10, neighborhood = 2, p = .3, positive.edge.ratio = .5)
    
    expect_equal(model.bootnet, model.netpower$weights)
})



test_that("`get.model` sets the correct thresholds", {
    
    expect_equal(get.model("ggm", "random", 5, p = .5)$thresholds, "n.a.")
    
    expect_gte(length(get.model("ising", "random", 5, p = .5)$thresholds), 1)
})
