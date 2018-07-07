g <- function(n) {
    rrr = runif(n^2)
    graph=matrix(1,n,n)
    graph <- graph * abs(rrr)
    graph[lower.tri(graph)] <- t(graph)[lower.tri(graph)]
    diag(graph) <- 0
    return(graph)
}

# library(netTinker)
library(mvtnorm)

# graph = build_true_network(10, 1, operationalize_connectedness(1, 3))$graph

# ggmSampler = ggmGenerator(TRUE, 5)
# ggmSampler(100, graph)
# ggmSampler(100, genGGM(10))

ggmSampler <- function(n, input) {
    graph <- input
    intercepts <- rep(0, ncol(graph))

    diag(graph) <- 0

    Sigma <- cov2cor(solve(diag(ncol(graph)) - graph))
    Data <- mvtnorm::rmvnorm(n, sigma = Sigma)

    for (i in 1:ncol(Data)){
        Data[,i] <- as.numeric(cut(Data[,i],sort(c(-Inf,rnorm(5.000000-1),Inf))))
    }

    return(Data)
}
# ggmSampler(100, graph)
# ggmSampler(100, genGGM(10))


graph = genGGM(10, propPositive = 1, p = 1, nei = 2)
graph = isingDepr$graph


graph = g(10)
x = diag(ncol(graph))
x = x - graph
x = solve(x)
sigma = cov2cor(x)
sigma

rmvnorm(10, sigma = sigma)

data <- rmvnorm(500, sigma = sigma)

for (i in 1:ncol(data)){
    data[,i] <- as.numeric(cut(data[,i],sort(c(-Inf,rnorm(5.000000-1),Inf))))
}




#
ggmSampler(10, graph)



gtest <- function(Nvar, p = 0, nei = 1, parRange = c(0.5, 1), constant = 1.5,
          propPositive = 0.5)
{
    trueKappa <- as.matrix(igraph::get.adjacency(igraph::watts.strogatz.game(1,
                                                                             Nvar, nei, p)))
    trueKappa[upper.tri(trueKappa)] <- trueKappa[upper.tri(trueKappa)] *
        sample(c(-1, 1), sum(upper.tri(trueKappa)), TRUE, prob = c(propPositive,
                                                                   1 - propPositive)) * runif(sum(upper.tri(trueKappa)),
                                                                                              min(parRange), max(parRange))
    trueKappa[lower.tri(trueKappa)] <- t(trueKappa)[lower.tri(trueKappa)]
    diag(trueKappa) <- constant * rowSums(abs(trueKappa))
    diag(trueKappa) <- ifelse(diag(trueKappa) == 0, 1, diag(trueKappa))
    trueKappa <- trueKappa/diag(trueKappa)[row(trueKappa)]
    trueKappa <- (trueKappa + t(trueKappa))/2
    return(as.matrix(qgraph::wi2net(trueKappa)))
}


qgraph::wi2net()


#















