library(lattice)

sampler <- function(mu, pcount = 10, lambda = 0, ncaller = 1) {
    n <- length(mu)
    alpha <- mu * pcount
    beta <- (1 - mu) * pcount
    prob <- rbeta(n, shape1 = alpha, shape2 = beta, ncp = lambda)
    iscalled <- as.logical(rbinom(n, 1, prob))
    marker <- rep(ncaller, n)
    marker[! iscalled] <- NaN
    return(marker)
}

caller.par <-
    data.frame(A = c(10, 0),
               B = c(10, 10),
               C = c(100, 0),
               D = c(100, 10))
rownames(caller.par) <- c("pcount", "lambda")

ncalls <- 1e1
df <- data.frame(call.loc = sort(runif(ncalls)))
rownames(df) <- paste0("c", seq_len(ncalls))
df$prob <- runif(ncalls)
s <- data.frame(lapply(caller.par, function(y) sampler(df$prob, y[1], y[2])))
df <- cbind(df, t(t(as.matrix(s)) * 1:4))
