set.seed(666)

curve(dnorm, from = -5, to = 5)
curve(dcauchy, from = -5, to = 5, add = T, col = "blue")

qcauchy(0.75) - qcauchy(0.25)
qnorm(0.75) - qnorm(0.25)

amostra <- rcauchy(10000)
mean(amostra)
median(amostra)


newton_rapson <- function(x0, fx, f_x, max_iter = 10, erro_min = 1E-6){
  erro <- fx(x0)
  i <- 1
  while((erro >= erro_min) & (i < max_iter)){
    i <- i + 1
    derivada <- f_x(x0[i-1])
    if(derivada == 0) return("Erro - derivada zerada")
    x0[i] <- x0[i-1] - (fx(x0[i-1])/derivada) 
    erro <- fx(x0[i])
  }
  return(list("Resultado" = x0[i],
              "Erro" = erro,
              "Raiz" = fx(x0[i]),
              "Qte_Iteracoes" = i))
}

newton_rapson(3, function(x) (x**2) - 10, function(x) 2*x, erro_min = 1E-3)
