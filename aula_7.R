newton_rapson <- function(x0, fx, f_x, max_iter = 10, erro_min = 1E-6){
  # Calculando erro inicial
  erro <- fx(x0)
  # Variável para controlar as iterações
  i <- 1
  # Definindo os criterios de parada quando o erro menor que o erro mínimo e i
  # for maior que o máximo de iterações
  while((abs(erro) >= erro_min) & (i < max_iter)){
    # Avançando uma iteração
    i <- i + 1
    # calculando a derivada
    derivada <- f_x(x0[i-1])
    # Retornando um erro quando a derivada for zero
    if(derivada == 0) return("Erro - derivada zerada")
    # Calculando o próximo valor
    x0[i] <- x0[i-1] - (fx(x0[i-1])/derivada)
    # calculando o erro atual
    erro <- fx(x0[i])
  }
  return(list("Resultado" = x0[i],
              "Erro" = erro,
              "Raiz" = fx(x0[i]),
              "Qte_Iteracoes" = i-1))
}
qfunc <- function(p, x0, Fx, fx){
  unlist(lapply(p, function(pi) 
    newton_rapson(x0, function(x) Fx(x) - pi, function(x) fx(x))$Resultado))
}

qfunc_opt <- function(p, Fx = pnorm, intervalo = c(-4, 4), erro = 1E-6){
  
  func_opt_norm <- function(x) (Fx(x) - p)**2
  optimize(func_opt_norm, intervalo, tol = erro)$minimum
}

qfunc_opt_vec <- Vectorize(qfunc_opt)

diff(qfunc_opt_vec(c(0.25, 0.75)))

diff(qnorm(c(0.25, 0.75)))
  
diff(qcauchy(c(0.25, 0.75)))

diff(qt(c(0.25, 0.75), 2))
diff(qt(c(0.25, 0.75), 3))
diff(qt(c(0.25, 0.75), 30))
diff(qt(c(0.25, 0.75), 50))
diff(qt(c(0.25, 0.75), Inf))

# gera_gamma_1 <- function(n, alpha, lambda){
#   ui <- replicate(alpha, runif(n))
#   sapply(1:n, function(i) (-1/lambda) * log(prod(ui[i,])))
# }

gera_gamma <- function(n, alpha, lambda){
  replicate(n, (-1/lambda) * log(prod(runif(alpha))))
}

gera_gamma(10, 5, 1)

gera_gamma(10, 5, 1)

par(mfrow = c(2,1))
hist(gera_gamma(10000, 5, 1))
hist(rgamma(10000, shape = 5, rate = 1))

library(microbenchmark)
microbenchmark(gera_gamma(10000, 100, 1), rgamma(10000, 100, 1))

microbenchmark(gera_gamma(1000, 100, 1), rgamma(1000, 100, 1))






