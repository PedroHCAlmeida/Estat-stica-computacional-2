set.seed(666)
fx_beta <- function(x) 6*x*(1-x)

gera_beta <- function(fx, N = 1E4){
  faltantes <- N
  x <- c()
  gerados <- 0
  
  while(faltantes > 0){
    
    gerados <- gerados + faltantes
    U1 <- runif(faltantes)
    U2 <- runif(faltantes)
    
    beta_i <- ifelse(fx(U1) >= 1.5*U2, U1, NA)
    na_beta_i <- is.na(beta_i)
    
    faltantes <- sum(na_beta_i)
    x <- c(x, beta_i[!na_beta_i])
  }
  return(list(x = x, rate = N/gerados))
}
x_beta <- gera_beta(fx_beta)
hist(x_beta$x, freq = F)
curve(dbeta(x,2,2), add=T)

# ------------------ Metodo rejeicao -----------------


fx <- function(x){
  2*exp(-(x**2)/2)/sqrt(2*pi)
  }

curve(fx(x), from = 0, to = 5)

gy <- function(x) exp(-x)

M <- 2*exp(0.5)/sqrt(2*pi)

gera_rejeicao_exp <- function(fx, gy, M, gerador_Y, N = 1E4, ...){
  faltantes <- N
  x <- c()
  gerados <- 0
  
  while(faltantes > 0){
    
    gerados <- gerados + faltantes
    Y <- gerador_Y(faltantes, ...)
    U <- runif(faltantes)
    
    x_i <- ifelse(U <= fx(Y)/(M*gy(Y)), Y, NA)
    na_i <- is.na(x_i)

    faltantes <- sum(na_i)
    x <- c(x, x_i[!na_i])
  }
  return(list(x = x, rate = N/gerados))
}

fx <- function(x)  2*exp(-(x**2)/2)/sqrt(2*pi)
gy <- function(x) exp(-x)
M <- 2*exp(0.5)/sqrt(2*pi)
gerador_Y <- function(N){
  U <- runif(N)
  -log(U)
}

gera_rejeicao_exp(fx, gy, M, gerador_Y)

gy_M <- function(x) M*gy(x)
x_1 <- gera_rejeicao_exp(fx, gy, M, gerador_Y)

curve(gy_M, from = 0, to = 5, col = "blue")
hist(x_1$x, freq = F, add = T)
curve(fx(x), from = 0, to = 5, add = T, col = "red")

# --------------- Normal ---------------------------

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
  
  return(X)
}

x_normal_falt <- gera_normal_faltantes(fx, gy, M)
hist(x_normal_falt$x, freq = F)

x_normal_rep <- gera_normal_replicate(fx, gy, M)
hist(x_normal_rep$x, freq = F)

x_normal_falt$rate
x_normal_rep$rate

library(microbenchmark)
microbenchmark(gera_normal_faltantes(fx, gy, M),
               gera_normal_replicate(fx, gy, M))
