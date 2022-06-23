set.seed(666)

beta_function <- function(t, a, b) (1-t)**(b-1)*(t**(a-1))

beta_monte_carlo <- function(k, a, b){
  U <- runif(k)
  teta <- mean(beta_function(U, a, b))
  teta
}

beta_monte_carlo(1E3, 2, 3)

testa_estimador <- function(k, a = 2, b = 3, n = 1E3){
  
  medias_amostrais <- replicate(n, beta_monte_carlo(k, a, b))
  hist(medias_amostrais, freq = F)
  media <- mean(medias_amostrais)
  variancia <- var(medias_amostrais)
  erro <- sqrt(variancia)
  
  lim <- quantile(medias_amostrais, probs = c(0.025, 0.975))
  intervalo_teorico <- qnorm(0.975)*erro 
  lim_teoricos <- c(media - intervalo_teorico,
                    media + intervalo_teorico)
    
  mtext(paste("Média", round(media, 8)), at = 0.088)
  mtext(paste("Variância", round(variancia, 8)), line = -1, at = 0.088)
  
  
  abline(v = lim[1], col = "blue")
  abline(v = lim[2], col = "blue")
  
  abline(v = lim_teoricos[1], col = "red")
  abline(v = lim_teoricos[2], col = "red")
  
  EQM <- mean((medias_amostrais - beta(a, b))**2)
  
  list(media = media,
       variancia = variancia,
       limites = lim, 
       limites_teoricos = lim_teoricos,
       erro_padrao = erro, 
       EQM = EQM,
       log_eqm = log10(EQM))
  
}

resultados <- testa_estimador(1E3)
log(resultados$EQM)

n <- seq(1000, 50000, by = 5000)
log_eqm_estim <- sapply(n, 
                   function(n) testa_estimador(n)$log_eqm)

plot(n, log_eqm_estim)


library(microbenchmark)

microbenchmark(beta_monte_carlo(1E3, 2, 3),
               beta_monte_carlo(5E3, 2, 3),
               beta_monte_carlo(10E3, 2, 3),
               beta(2,3))
                                                                                                      