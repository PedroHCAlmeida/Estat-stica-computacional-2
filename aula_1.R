hist(rock$area, freq = T)

fn <- ecdf(rock$area)

str(fn)

plot(fn)

fn(8000)

fn(c(4000, 6000, 8000))

summary(fn)

media <- mean(rock$area)
desvio <- sd(rock$area)

curve(pnorm(x, mean = media, sd = desvio),
      lwd = 2, col = "blue", add = T)

quantis <- c(4000, 6000, 8000)

soma_quadratica <- function(quantis){
  empiricos <- fn(quantis)
  teoricos <- pnorm(quantis, mean = media, sd = desvio)
  sum((teoricos - empiricos)**2)
}

soma_quadratica(quantis)
soma_quadratica(rock$area)

soma_quadratica(quantis)/3
soma_quadratica(rock$area)/48

# -------------------------------------------------------------- #
amostra_10 <- runif(48)
fn_amostra_unif <- ecdf(amostra_10)

soma_quadratica_unif <- function(quantis){
  empiricos <- fn_amostra_unif(quantis)
  teoricos <- punif(quantis)
  sum((teoricos - empiricos)**2)
}
soma_quadratica_unif(amostra_10)/48

#------------------------------------------------------------------------

mqse_unif <- function(tamanho_amostra = 48){
  
  amostra <- runif(tamanho_amostra)
  fn_amostra_unif <- ecdf(amostra)
  
  soma_quadratica_unif <- function(quantis){
    empiricos <- fn_amostra_unif(quantis)
    teoricos <- punif(quantis)
    sum((teoricos - empiricos)**2)
  }
  return(soma_quadratica_unif(amostra)/tamanho_amostra)
}

mqse_unif()
