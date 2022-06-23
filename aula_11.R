set.seed(666)

gera_normal_box_muller <- function(N = 1E4){
  
  N <- N/2
  U1 <- runif(N)
  U2 <- runif(N)
  
  d <- -2*log(U1)
  teta <- 2*pi*U2
  
  X <- sqrt(d)*cos(teta)
  Y <- sqrt(d)*sin(teta)
  
  c(X, Y)
}

x_normal <- gera_normal_box_muller()
hist(x_normal, freq = F)

library(microbenchmark)
microbenchmark(gera_normal_box_muller(1E4),
               rnorm(1E4))

#---------------------------- Indireta -----------------------------------------

# gera_normal_box_muller_indireta <- function(N = 1E4){
#   
#   N <- N/2
#   U1 <- runif(N)
#   U2 <- runif(N)
#   U3 <- runif(N)
#   
#   V1 <- 2*U1 - 1
#   V2 <- 2*U2 - 1
#   
#   d <- -2*log(U3)
#   cos_teta <- V2/sqrt(V1**2 + V2**2)
#   sin_teta <- V1/sqrt(V1**2 + V2**2)
#   
#   X <- sqrt(d)*cos_teta
#   Y <- sqrt(d)*sin_teta
#   
#   c(X, Y)
# }

gera_normal_box_muller_indireta <- function(N = 1E4){

  faltantes <- N/2
  X <- c()
  
  while(faltantes > 0){
    U1 <- runif(N)
    U2 <- runif(N)
  
    V1 <- 2*U1 - 1
    V2 <- 2*U2 - 1
    S <- V1**2 + V2**2
    
    S <- ifelse(S < 1, S, NA)
    na_i <- is.na(S)
    faltantes <- sum(na_i)
    S <- S[!na_i]
    
    X_i <- sqrt(-2*log(S)/S) * V1[!na_i]
    Y_i <- sqrt(-2*log(S)/S) * V2[!na_i]

    X <- c(X_i, Y_i)
    break
  }
  X
}

x_normal_indireta <- gera_normal_box_muller_indireta()
hist(x_normal_indireta, freq = F)
curve(dnorm(x), col = "blue", add = T)

microbenchmark(gera_normal_box_muller(1E4),
               gera_normal_box_muller_indireta(1E4),
               rnorm(1E4))

