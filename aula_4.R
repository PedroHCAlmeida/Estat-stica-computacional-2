set.seed(666)

random_exp <- function(N, lambda = 1){
  U <- runif(N)
  return(-1*log(U)/lambda)
}
N <- 1E4
amostra_exp <- random_exp(N)

mean(amostra_exp)
var(amostra_exp)

hist(amostra_exp, freq = F)
curve(dexp, add = T, col = "blue")

fn_exp <- ecdf(amostra_exp)
plot(fn_exp, ylim = c(0.45, 0.55), xlim = c(0.5,1))
curve(pexp, add = T, col = 'red')

plot(fn_exp, ylim = c(0.8, 0.9), xlim = c(1.5,2.5))
curve(pexp, add = T, col = 'red')

qexp(c(0.95, 0.99, 0.995, 0.999))

quantile(amostra_exp, probs = c(0.95, 0.99, 0.995, 0.999))

quantis <- qexp((1:9)*0.1)
quantile(amostra_exp, probs = (1:9)*0.1)

vet_breaks <- c(0, quantis, max(amostra_exp))
# vet_breaks_log <- c(0, log(vet_breaks[2:11]))
# hist(log(amostra_exp), breaks = vet_breaks_log, freq = F)
fi <- hist(amostra_exp, breaks = vet_breaks, freq = F)$counts
ei <- 1000
k <- 10

qui2 <- ((fi - ei) ** 2)/ei
qui2/sum(qui2)
p <- 1 - pchisq(qui2, df = k-1)

acf(amostra_exp, type = 'covariance')
acf(amostra_exp)

plot(amostra_exp[2:N], amostra_exp[1:N-1])
