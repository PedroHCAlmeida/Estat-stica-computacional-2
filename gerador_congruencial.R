set.seed(666)
N <- 1E4
amostra <- runif(N)

gerador_congruencial_for <- function(x0 = 2, N = 10, m = 10, A = 103, B = 17){
  xn <- x0
  for(i in 1:(N+1)){
    xn[i+1] <- (A*xn[i] + B) %% m
  }
  
  un <- xn[2:(N+1)]/m
  
  return(list('Xn' = xn[2:(N+1)], 'Un' = un))
}

gerador_congruencial_for()

gerador_congruencial_rec <- function(x0 = 2, N = 10, m = 10, A = 103, B = 17){
  
  rec_xn <- function(x0_ = x0, N_ = N+1, m_ = m, A_ = A, B_ = B){
    if(N_ == 1){
      return((A_*x0_ + B_) %% m_)
    }
    return(c(x0_, (A_*rec_xn(x0_, N_-1, m_, A_, B_) + B_) %% m_))
  }
  
  xn <- rec_xn()
  un <- xn[2:N+1]/m
  
  return(un)
}

library(microbenchmark)

microbenchmark(gerador_congruencial_rec = gerador_congruencial_rec(x0 = 4, N = 1E3, A= 1644525, B = 1013904223, m=2E32),
               gerador_congruencial_for = gerador_congruencial_for(x0 = 4, N = 1E3, A= 1644525, B = 1013904223, m=2E32))


gerador_congruencial_for()

gerador_congruencial_for(x0 = 4, N = 10000, A= 1644525, B = 1013904223, m=2E32)
