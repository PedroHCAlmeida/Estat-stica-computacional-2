
fx <- function(x) dbeta(x, 2.7, 6.3)

M <- optimize(f = fx, interval = c(0,1), maximum = T)$objective

rejeicao <- function(fx, gy, M, gerador_y, N = 1E4, ...){
  
  faltantes <- N
  x <- c()
  x_nao_aceito <- c()
  y <- c()
  y_nao_aceito <- c()
  gerados <- 0
  
  while(faltantes > 0){
    
    gerados <- gerados + faltantes
    
    Y <- gerador_y(faltantes, ...)
    U <- runif(faltantes)
    
    x_i <- ifelse(U <= fx(Y)/(M*gy(Y)), Y, NA)

    na_i <- is.na(x_i)
    x_i_aceito <- x_i[!na_i]
    y_i_aceito <- (U*M*gy(Y))[!na_i]
    x_i_nao_aceito <- Y[na_i]
    y_i_nao_aceito <- (U*M*gy(Y))[na_i]
    
    faltantes <- sum(na_i)
    x <- c(x, x_i_aceito)
    y <- c(y, y_i_aceito)
    x_nao_aceito <- c(x_nao_aceito, x_i_nao_aceito)
    y_nao_aceito <- c(y_nao_aceito, y_i_nao_aceito)
  }
  
  return(list(x = x, y = y, rate = N/gerados, iteracoes = gerados,
              x_nao_aceitos = x_nao_aceito, y_nao_aceitos = y_nao_aceito))
}

x_beta <- rejeicao(fx, dunif, M, runif)

hist(x_beta$x, freq = F)
curve(dbeta(x, 2.7, 6.3), add = T, col = "blue")

x_beta$x_nao_aceitos

plot(x_beta$x_nao_aceitos, x_beta$y_nao_aceitos, col = "red")
points(x_beta$x, x_beta$y, col = "blue")

x_beta_2 <- rejeicao(fx, dunif, M, runif, N = 200)
plot(x_beta_2$x_nao_aceitos, x_beta_2$y_nao_aceitos, col = "red")
points(x_beta_2$x, x_beta_2$y, col = "blue")
curve(fx, col = "blue", add = T)

# -------------------- Exemplo 2 ---------------------------

gy_2 <- function(x) dbeta(x, 2, 6)

M_2 <- optimize(f = function(x) fx(x)/gy_2(x), interval = c(0,1), maximum =T)$objective
M_2

curve(M*gy_2(x))
curve(dbeta(x, 2.7, 6.3), add = T)

x_beta_3 <- rejeicao(fx, gy_2, M_2, rbeta, shape1 = 2, shape2 = 6, N = 1000)
x_beta_3$rate

hist(x_beta_3$x, freq = F)
curve(dbeta(x, 2.7, 6.3), add = T, col = "blue")

plot(x_beta_3$x_nao_aceitos, x_beta_3$y_nao_aceitos, col = "red")
points(x_beta_3$x, x_beta_3$y, col = "blue")
curve(fx, col = "blue", add = T)

iteracoes <- replicate(1000, 
                rejeicao(fx, gy_2, M_2, rbeta, shape1 = 2, shape2 = 6, N = 1000)$iteracoes)

hist(iteracoes/1000)
mean(iteracoes/1000)

