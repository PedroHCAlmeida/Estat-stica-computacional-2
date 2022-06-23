set.seed(666)

gera_normal_box_muller <- function(N = 1E4){
  
  N <- N/2
  U1 <- runif(N)
  U2 <- runif(N)
  
  d <- -2*log(U1)
  teta <- 2*pi*U2
  
  Z1 <- sqrt(d)*cos(teta)
  Z2 <- sqrt(d)*sin(teta)
  
  c(Z1, Z2)
}

gera_normal_bivariada_condicional <- function(N, mu = c(0,0), sigma = matrix(c(1,0.8,0.8,2), ncol = 2)){
  X1 <- rnorm(N, mu[1], sigma[1, 1])
  X2_X1 <- rnorm(N, mean = mu[2] + (sigma[1, 2]*sqrt(sigma[2,2]/sigma[1,1])*(X1-mu[1])), 
                 sd = sqrt(sigma[2,2]*(1-(sigma[1,2]**2))))
  
  return(list(X1 = X1, X2_X1 = X2_X1))
}

gera_normal_bivariada_matriz_A <- function(n, 
                                           mu = c(0,0), 
                                           sigma = matrix(c(1, 0.8*sqrt(2), 
                                                    0.8*sqrt(2), 2), 
                                                    ncol = 2)){
  
  Z <- replicate(2, gera_normal_box_muller(n))
  ro <- sigma[1,2]/(sqrt(sigma[1, 1]*sigma[2,2]))
  X1 <- sqrt(sigma[1, 1])*Z[[1]]+mu[1]
  X2 <- (ro*sqrt(sigma[2, 2])*Z[[1]]) + (sqrt(sigma[2, 2])*sqrt(1-(ro**2))*Z[[2]]) + mu[2]
  
  return(list(x1 = X1, x2 = X2))
}

x_matriz_A <- gera_normal_bivariada_matriz_A(1E3)

hist(x_matriz_A$x1, freq = F)
hist(x_matriz_A$x2, freq = F)

cor(x_matriz_A$x1, x_matriz_A$x2)

plot(x_matriz_A$x1, x_matriz_A$x2)

lapply(x_matriz_A, shapiro.test)

lapply(x_matriz_A, mean)

lapply(x_matriz_A, var)

library(microbenchmark)

microbenchmark(gera_normal_bivariada_condicional(1E4),
               gera_normal_bivariada_matriz_A(1E4))

teste_chi2_normal_bivariada <- function(x1, x2, mu = c(0,0), sigma = matrix(c(1, 0.8*sqrt(2), 
                                                                         0.8*sqrt(2), 2), 
                                                                       ncol = 2)){
  
  n <- length(x1)
  X_2 <- lapply(1:n,
                function(i){
                  matrix(c(x1[i] - mu[1], x2[i] - mu[1]), ncol = 2)%*%solve(sigma, matrix(c(x1[i] - mu[1], x2[i] - mu[1]), ncol = 1))
                })
  unlist(X_2)
}

X_2_teste <- teste_chi2_normal_bivariada(x_matriz_A$x1, x_matriz_A$x2)
hist(X_2_teste, freq = F)
curve(dchisq(x, 2), add = T, col = "blue")

qchisq(0.95, 2)

summary(X_2_teste)
sum(X_2_teste > qchisq(0.95, 2))
quantile(X_2_teste, 0.95)

microbenchmark(teste_chi2_normal_bivariada(x_matriz_A$x1, x_matriz_A$x2))

A_linha <- chol(matrix(c(9, 4, 2, 4, 8, 3, 2, 3, 7), ncol = 3, byrow = T))

A <- t(A_linha)

A%*%A_linha

gera_normal_multi_matriz_A <- function(n, 
                                       dim = 3,
                                       mu = c(0, 0, 0), 
                                       sigma = matrix(c(9, 4, 2, 4, 8, 3, 2, 3, 7), ncol = 3, byrow = T)){
  
  Z <- t(replicate(dim, gera_normal_box_muller(n)))
  A <- t(chol(sigma))
  X <- lapply(1:n, function(i){
    A%*%Z[,i]+mu
    })
  X <- do.call(cbind, X)
  return(X)
}

X_tri <- gera_normal_multi_matriz_A(10000)
plot(X_tri[1,], X_tri[2,])
plot(X_tri[1,], X_tri[3,])
plot(X_tri[2,], X_tri[3,])




                                                                                