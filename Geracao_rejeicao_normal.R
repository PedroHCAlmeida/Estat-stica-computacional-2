# Geracao Normal

 # Definindo semente
set.seed(666)

 # Definindo fx, gy e M
fx <- function(x)  2*exp(-(x**2)/2)/sqrt(2*pi)
gy <- function(x) exp(-x)
M <- 2*exp(0.5)/sqrt(2*pi)

gera_normal_faltantes <- function(fx, gy, M, N = 1E4, ...){
  
  faltantes <- N
  x <- c()
  gerados <- 0
  
  while(faltantes > 0){
    
    gerados <- gerados + faltantes
    
    U1 <- runif(faltantes)
    Y <- -log(U1)
    U <- runif(faltantes)
    
    x_i <- ifelse(U <= fx(Y)/(M*gy(Y)), Y, NA)
    na_i <- is.na(x_i)
    x_i_aceito <- x_i[!na_i]
    
    faltantes <- sum(na_i)
    x <- c(x, x_i_aceito)
  }
  
  U_sinal <- runif(N)
  x <- ifelse(U_sinal < 0.5, -x, x)
  
  return(list(x = x, rate = N/gerados))
}


gera_normal_replicate <- function(fx, gy, M, N = 1E4, ...){
  
  rejeita_funcao <- function(){
    
    U1 <- runif(1)
    Y <- -log(U1)
    U <- runif(1)
    
    gerados <- 1
    
    while(U > fx(Y)/(M*gy(Y))){
      
      gerados <- gerados + 1
      
      U1 <- runif(1)
      Y <- -log(U1)
      U <- runif(1)
    }
    return(list(x = Y, rate = 1/gerados))
  }
  
  x <- replicate(N, rejeita_funcao())
  
  X <- list(x = unlist(x[1, ]), rate = mean(unlist(x[2, ])))
  
  U_sinal <- runif(N)
  X$x <- ifelse(U_sinal < 0.5, -X$x, X$x)
  
  return(x)
}

par(mfrow = c(2,1))
x_normal_falt <- gera_normal_faltantes(fx, gy, M)
hist(x_normal_falt$x, freq = F)
curve(dnorm(x), col = 'blue', add= T)

x_normal_rep <- gera_normal_replicate(fx, gy, M)
hist(x_normal_rep$x, freq = F)
curve(dnorm(x), col = 'blue', add= T)

sprintf("Proporção de aceitos faltantes: %f0.4", x_normal_falt$rate)
sprintf("Proporção de aceitos replicate: %f0.4", x_normal_rep$rate)

library(microbenchmark)
microbenchmark(gera_normal_faltantes(fx, gy, M),
               gera_normal_replicate(fx, gy, M))
