set.seed(666)

N <- 1E4
u1 <- runif(N)
u2 <- runif(N)
u3 <- runif(N)

z <- -log(u1*u2)

X <- z*u2
Y <- z - X

hist(X+Y, freq = F)
curve(dgamma(x, 2, 1), add = T)

gera_chi2 <- function(v, N=1E4){
  replicate(N, -2*log(prod(runif(v/2))))
}

hist(gera_chi2(10), freq=F, breaks = 20)
curve(dchisq(x, 10), add=T)

library(microbenchmark)
microbenchmark(gera_chi2(10), rchisq(N, 10))
microbenchmark(gera_chi2(50), rchisq(N, 50))
microbenchmark(gera_chi2(50), rchisq(N, 50))
#--------------------------------------------------- #Beta ----------------------------------

curve(dbeta(x, 2, 5))
curve(dbeta(x, 5, 2), add = T)

curve(dbeta(x, 2, 2))
curve(dbeta(x, 5, 5))
curve(dbeta(x, 10, 10))
curve(dbeta(x, 0.1, 0.1))
curve(dbeta(x, 0.5, 0.5), add=T)
curve(dbeta(x, 0.9, 0.9), add=T)


gera_beta <- function(alpha, beta, lambda = 1, N=1E4){
  X <- rgamma(N, alpha, lambda)
  Y <- rgamma(N, beta, lambda)
  X/(X+Y)
}

hist(gera_beta(5,5), freq = F)
curve(dbeta(x, 5, 5), add=T)


hist(gera_beta(2,5), freq = F)
curve(dbeta(x, 2, 5), add=T)

microbenchmark(gera_beta(2, 5), rbeta(N, 2, 5))

