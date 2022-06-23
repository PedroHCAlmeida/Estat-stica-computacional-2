set.seed(666)
gera_mistura_normais <- function(n, mu, sigma, p){
  
  dim <- length(mu)
  stopifnot(length(sigma) == dim, length(p) == dim)
  X <- lapply(1:dim,
              function(i) rnorm(n, mu[i], sigma[i]))
  U <- runif(n)
  p <- cumsum(c(0,p))
  X <- unlist(
         lapply(1:dim,
              function(i){
                ifelse(U > p[i]  & U < p[i+1], X[[i]], NA)
              }
              )
         )
  X[!is.na(X)]
}

mu <- c(-2, 2)
sigma <- c(1, 1)

x_mistura <- gera_mistura_normais(1E4, mu, sigma, c(0.3, 0.7)) 

hist(x_mistura, freq = F, ylim = c(0,0.3))
lines(density(x_mistura), col = "red")
curve(0.3*dnorm(x, -2, 1)+0.7*dnorm(x, 2, 1), col = "blue", add = T)

mu_2 <- c(1, 2)

x_mistura_2 <- gera_mistura_normais(1E4, mu_2, sigma, c(0.3, 0.7)) 

hist(x_mistura_2, freq = F)
lines(density(x_mistura_2), col = "red")
curve(0.3*dnorm(x, mu_2[1], sigma[1])+0.7*dnorm(x, mu_2[2], sigma[2]), col = "blue", add = T)


x_mistura_3 <- gera_mistura_normais(1E4, mu_2, sigma, c(0.5, 0.5)) 

hist(x_mistura_3, freq = F)
lines(density(x_mistura_3), col = "red")
curve(0.5*dnorm(x, mu_2[1], sigma[1])+0.5*dnorm(x, mu_2[2], sigma[2]), col = "blue", add = T)


mu_4 <- c(-1, 0, 2)
sigma_4 <- c(1, 1, 1)
p_4 <- c(0.3, 0.3, 0.4)

x_mistura_4 <- gera_mistura_normais(1E4, mu_4, sigma_4, p_4) 

hist(x_mistura_4, freq = F)
lines(density(x_mistura_4), col = "red")
curve(0.3*dnorm(x, mu_4[1], sigma_4[1])+0.3*dnorm(x, mu_4[2], sigma_4[2])+0.4*dnorm(x, mu_4[3], sigma_4[3]), 
      col = "blue", add = T)

library(microbenchmark)

microbenchmark(gera_mistura_normais(1E4, c(-2, 2, 9), c(1,1,1), c(0.3,0.3,0.4)))

#-----------------------------------Integracao Numerica --------------------------------------#

# Gama

f_gama <- function(a){
  integrate(function(x) x**(a-1)*exp(-x), 0, Inf)$val
  }

alpha <- seq(0.1, 10, le = 100)
plot(lgamma(alpha), log(sapply(alpha, f_gama)))

# Normal

F_norm <- function(a) integrate(dnorm, -a, a)$val
F_norm(1)

library(MASS)
F_norm_mass <- function(a) area(dnorm, -a, a)
F_norm_mass(1)

# Cauchy 

integrate(dcauchy, -Inf, Inf)$value
