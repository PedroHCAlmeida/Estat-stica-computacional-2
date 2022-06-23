set.seed(666)
amostra <- runif(1E4)

media <- mean(amostra)
variancia <- var(amostra)

t.test(x = amostra, mu = 0.5)

teste_var_chi_2 <- function(x, sigma_0 = 1/12, conf.level = 0.95){
  
  n <- length(x)
  alpha <- 1 - conf.level
  sigma <- var(x)
  
  t <- (n-1) * (sigma/sigma_0)
  F_chi2_t <- pchisq(t, df = n-1)
  p <- 2 * min(F_chi2_t, (1-F_chi2_t))
  
  IC <- ((n-1) * sigma) / qchisq(c(1-(alpha/2), alpha/2), df = n-1)
  return(list("Estatística de teste" = t, 
              "p-valor" = p,
              "Intervalo de confiança" = IC))
}
teste_var_chi_2(amostra)

teste_intervalos <- function(x, k = 10){
  
  n <- length(x)
  e <- n/k
  fi <- hist(x, breaks = k)$counts
  t = sum(((fi - e) ** 2) / e)
  p = 1 - pchisq(t, df = k-1)
  return(list("Estatística de teste" = t, 
              "p-valor" = p))
}

teste_intervalos(amostra)