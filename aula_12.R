#--------------------------- Normais Bivariadas -------------------------------#
set.seed(666)

gera_normal_bivariada <- function(N, mu = c(0,0), sigma = matrix(c(1,0.8,0.8,2), ncol = 2)){
  X1 <- rnorm(N, mu[1], sigma[1, 1])
  X2_X1 <- rnorm(N, mean = mu[2] + (sigma[1, 2]*sqrt(sigma[2,2]/sigma[1,1])*(X1-mu[1])), 
                 sd = sqrt(sigma[2,2]*(1-(sigma[1,2]**2))))
  
  return(list(X1 = X1, X2_X1 = X2_X1, 
              mu_1 = mu[1],))
} 

X <- gera_normal_bivariada(100)

hist(X$X1)
hist(X$X2_X1)
plot(X$X1, X$X2_X1)
points(mean(X$X1), mean(X$X2_X1), col = "red", cex = 1.5, pch=20)
points(0, 0, col = "blue", cex = 1.5, pch=20)

cor(X$X1, X$X2_X1)

shapiro.test(X$X1)
shapiro.test(X$X2_X1)

mean(X$X1)
mean(X$X2_X1)

sd(X$X1)
sd(X$X2_X1)

S <- matrix(c(1, 0.8*sqrt(2), 0.8*sqrt(2), 2), 
            ncol = 2)

auto_vet <- eigen(S)$vectors

plot(X$X1, X$X2_X1)
points(mean(X$X1), mean(X$X2_X1), 
       col = "red", cex = 1.5, pch=20)
points(0, 0, col = "blue", cex = 1.5, pch=20)
arrows(0, 0, auto_vet[1,1], auto_vet[2,1])
arrows(0, 0, auto_vet[1,2], auto_vet[2,2])

X_replicate <- replicate(100, 
                           list(rnorm(1), rnorm(1, mean = 0.8*sqrt(2)*(X1), 
                                                sd = sqrt(2*(1-(0.8**2)))))
                         )

library(microbenchmark)

microbenchmark(replicate(100, 
                         list(rnorm(1), rnorm(1, mean = 0.8*sqrt(2)*(X1), 
                                              sd = sqrt(2*(1-(0.8**2)))))),
               gera_normal_bivariada(100)
)

microbenchmark(
               gera_normal_bivariada(100)
)

# X_neg <- gera_normal_bivariada(100, sigma = matrix(c(1, -0.8, -0.8, 2), ncol=2))
# 
# hist(X_neg$X1)
# hist(X_neg$X2_X1)
# plot(X_neg$X1, X_neg$X2_X1)
# 
# cor(X_neg$X1, X_neg$X2_X1)
# 
# shapiro.test(X_neg$X1)
# shapiro.test(X_neg$X2_X1)





