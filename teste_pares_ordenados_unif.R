set.seed(666)
amostra <- runif(1E4)

teste_pares_ord_unif <- function(x, a = 0, b = 1, k=10){

   # Calculando o tamanho da amostra
  n <- length(x)
   # Verificando se o tamanho é um número par para gerar os pares ordenados
  stopifnot(n %% 2 == 0)
   # Valor esperado de pares ordenados em cada região
  e <- (n/(k**2))/2
  
   # Gerando um data.frame com pares ordenados dos indices impares com pares
  par_ordenado <- lapply(1:(n/2), function(i) c(x[i], x[i+1]))
  
   # Calculando a amplitude de valores
  amplitude <- (b - a)/ k
   # Calculando os intervalos a serem divididos
  intervalos <- seq(a, b, amplitude)
  
   # Definindo os intervalos pertencentes de cada valor 
  par_ord_intervalo <- lapply(par_ordenado,
                              function(x) cut(x, intervalos, 
                                              include.lowest = T))
  
   # Juntando a lista de pares de intervalos em um data.freame 
  par_ord_intervalo_df <- do.call(rbind, par_ord_intervalo)
  
   # Criando uma tabela para contar quantos pares pertencem ao mesmo intervalo 
   # em x e em y
  tabela_pares_ord <- aggregate(par_ord_intervalo_df, 
                                list(par_ord_intervalo_df[, 1], par_ord_intervalo_df[, 2]), 
                                length)
  
   # Definindo os nomes das colunas da tabela
  colnames(tabela_pares_ord) <- c("Intervalo_x", "Intervalo_y", "Quantidade_impares", "Quantidade_pares")
   # Somando as quantidades impares e pares para saber quantos valores estão naquela região
  tabela_pares_ord$fi <-tabela_pares_ord$Quantidade_impares + tabela_pares_ord$Quantidade_pares
  
   # Plotando os pares ordenados
  plot(do.call(rbind,(par_ordenado)), col = "grey")
   # Plotando os intervalos
  abline(v = intervalos, col = "blue")
  abline(h = intervalos, col = "blue")
   # Percorrendo os intervalos em x e em y para plotar a quantidade de valores
  lapply(1:k, function(j){
    lapply(1:k, function(i){
      text((intervalos[i] + intervalos[i+1])/2, (intervalos[j] + intervalos[j+1])/2,
           tabela_pares_ord$fi[tabela_pares_ord$Intervalo_x == i & tabela_pares_ord$Intervalo_y == j], col = "Red", cex = 1.5)
      
    }
    )
  }
  )
  
   # Retornando esta tabela
  return(tabela_pares_ord)
}

 # Chamando a função para a amostra
tabela <- teste_pares_ord_unif(amostra)
 # Somando a quantidade observado para ver se da o tamanho da amostra
sum(tabela$fi)

