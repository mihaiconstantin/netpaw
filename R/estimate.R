# This file contains functions related to estimating the model.



estimator.ising <- function(data) {
    # Estimate the model.
    result = IsingFitEssential(data)

    return(result)
}



estimator.ggm <- function(data) {
    # Estimate the model.
    result = bootnet::estimateNetwork(data, default = 'EBICglasso', verbose = FALSE, memorysaver = TRUE)
    
    # Remove the names, not needed.
    rownames(result$graph) <- colnames(result$graph) <- NULL

    return(result)
}



#' @title Estimated the model based on the type of data provided.
#' @export
estimate.model <- function(data) {
    # Determine which estimator to use based on the model type.
    if(data$model == "ising") 
        estimator.fun = estimator.ising
    else if(data$model == "ggm")
        estimator.fun = estimator.ggm
    else
        stop("Unrecognized estimator type.")

    # Fit the model.
    model.fit = estimator.fun(data$data)

    return(model.fit)
}



# # # 
# External functions.
# # #

#' @title .
#' Copyright: https://github.com/cvborkulo/IsingFit
#' @export
IsingFitEssential <- function(x, AND = TRUE, gamma = 0.25, lowerbound.lambda = NA) {
    nvar <- ncol(x)
    p <- nvar - 1
    intercepts <- betas <- lambdas <- list(vector, nvar)
    nlambdas <- rep(0, nvar)

    for (i in 1:nvar) {
        a <- glmnet::glmnet(x[, -i], x[, i], family = "binomial")
        intercepts[[i]] <- a$a0
        betas[[i]] <- a$beta
        lambdas[[i]] <- a$lambda
        nlambdas[i] <- length(lambdas[[i]])
    }

    P <- logl <- sumlogl <- J <- matrix(0, max(nlambdas), nvar)

    for (i in 1:nvar) {
        J[1:ncol(betas[[i]]), i] <- colSums(betas[[i]] != 0)
    }

    logl_M <- P_M <- array(0, dim = c(nrow(x), max(nlambdas), nvar))
    N <- nrow(x)

    for (i in 1:nvar) {
        betas.ii <- as.matrix(betas[[i]])
        int.ii <- intercepts[[i]]
        y <- matrix(0, nrow = N, ncol = ncol(betas.ii))
        xi <- x[, -i]
        NB <- nrow(betas.ii)

        for (bb in 1:NB) {
            y <- y + betas.ii[rep(bb, N), ] * xi[, bb]
        }

        y <- matrix(int.ii, nrow = N, ncol = ncol(y), byrow = TRUE) + y
        n_NA <- max(nlambdas) - ncol(y)

        if (n_NA > 0) {
            for (vv in 1:n_NA) {
                y <- cbind(y, NA)
            }
        }

        P_M[, , i] <- exp(y * x[, i])/(1 + exp(y))
        logl_M[, , i] <- log(P_M[, , i])
    }

    logl_Msum <- colSums(logl_M, 1, na.rm = FALSE)
    sumlogl <- logl_Msum
    sumlogl[sumlogl == 0] = NA
    penalty <- J * log(nrow(x)) + 2 * gamma * J * log(p)
    EBIC <- -2 * sumlogl + penalty
    lambda.mat <- matrix(NA, nrow(EBIC), ncol(EBIC))

    for (i in 1:nvar) {
        lambda.mat[, i] <- c(lambdas[[i]], rep(NA, nrow(EBIC) - length(lambdas[[i]])))
    }

    if (!is.na(lowerbound.lambda)) {
        EBIC <- EBIC/(lambda.mat >= lowerbound.lambda) * 1
    }

    lambda.opt <- apply(EBIC, 2, which.min)
    lambda.val <- rep(NA, nvar)
    thresholds <- 0

    for (i in 1:length(lambda.opt)) {
        lambda.val[i] <- lambda.mat[lambda.opt[i], i]
        thresholds[i] <- intercepts[[i]][lambda.opt[i]]
    }

    weights.opt <- matrix(, nvar, nvar)

    for (i in 1:nvar) {
        weights.opt[i, -i] <- betas[[i]][, lambda.opt[i]]
    }

    asymm.weights <- weights.opt
    diag(asymm.weights) = 0

    if (AND == TRUE) {
        adj <- weights.opt
        adj <- (adj != 0) * 1
        EN.weights <- adj * t(adj)
        EN.weights <- EN.weights * weights.opt
        meanweights.opt <- (EN.weights + t(EN.weights))/2
        diag(meanweights.opt) <- 0
    }
    else {
        meanweights.opt <- (weights.opt + t(weights.opt))/2
        diag(meanweights.opt) <- 0
    }

    # Return the main results.
    return(
        list(
            weiadj = meanweights.opt,
            thresholds = thresholds
        )
    )
}
