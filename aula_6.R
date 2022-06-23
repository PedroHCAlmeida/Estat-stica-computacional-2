f <- expression(x**2 - 4)
df <- D(f, "x")
x <- -4:4
eval(f)
eval(df)

func <- function(x) eval(f)
func_linha <- function(x) eval(df)

func(c(-4,-2,0,2,4))
func_linha(c(-4,-2,0,2,4))

curve(func, from = -4, to = 4)
curve(func_linha, from = -4, to = 4, add = T)

genD(x**2 - 4, 2)$D[1]
genD(pnorm, 0)$D

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

newton_rapson(5, function(x) x**3 - (2*x) - 5, function(x) 3*(x**2) - 2, max_iter = 100)
newton_rapson(1000, function(x) x**3 - (2*x) - 5, function(x) 3*(x**2) - 2, max_iter = 100)

newton_rapson(5, function(x) exp(2*x) - x - 6, function(x) 2*exp(2*x) - 1, max_iter = 1000)

newton_rapson(2, function(x) exp(2*x) - x - 6, function(x) 2*exp(2*x) - 1, max_iter = 1000)

uniroot(function(x) exp(2*x) - x - 6, c(-2, 2), tol = 1E-6)

qfunc <- function(p, x0, Fx, fx){
  unlist(lapply(p, function(pi) 
    newton_rapson(x0, function(x) Fx(x) - pi, function(x) fx(x))$Resultado))
}

