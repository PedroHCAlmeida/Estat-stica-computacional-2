set.seed(666)
N <- 1E4
amostra <- runif(N)

plot(amostra[2:N], amostra[1:N-1])

objeto <- acf(amostra)$acf
objeto2 <- acf(amostra, type = 'covariance')$acf

# gerador_congruencial <- function(x0 = 2, n = 3, m = 10, A = 103, B = 17){
#   xn <- x0
#   for(i in 1:(n-1)){
#     xn[i+1] <- (A*xn[i] + B) %% m
#   }
#   un = xn/m
#   return(list('Xn' = xn, 'Un' = un))
# }

gerador_congruencial <- function(x0 = 2, N = 10, m = 10, A = 103, B = 17){
  rec_xn <- function(x0_ = x0, N_ = N, m_ = m, A_ = A, B_ = B){
    if(N_ == 1){
      return((A_*x0_ + B_) %% m_)
    }
    return(c(x0_, (A_*rec_xn(x0_, N_-1, m_, A_, B_) + B_) %% m_))
  }
  xn <- rec_xn()
  un <- xn/m
  return(un)
}
gerador_congruencial(N = 750)

gerador_congruencial(x0 = 4, N = 1E3, A= 1644525, B = 1013904223, m=2E32)






