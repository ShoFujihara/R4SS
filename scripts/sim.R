

library(confintr)


n <- 10
num_simulations <- 10000
count <- 0
p <- 0.9

for (i in 1:num_simulations) {
  x <- as.numeric(runif(n) < p)
  prop <- mean(x)
  ll <- prop + qnorm(0.025) * sqrt(prop * (1 - prop) / n)
  ul <- prop + qnorm(0.975) * sqrt(prop * (1 - prop) / n)
  if (ll < p && ul > p) {
    count <- count + 1
  }
}
probability <- count / num_simulations
print(probability)

count <- 0
for (i in 1:num_simulations) {
  x <- c(as.numeric(runif(n) < p),1,1,0,0)
  prop <- mean(x)
  ll <- prop + qnorm(0.025) * sqrt(prop * (1 - prop) / (n + 4))
  ul <- prop + qnorm(0.975) * sqrt(prop * (1 - prop) / (n + 4))
  if (ll < p && ul > p) {
    count <- count + 1
  }
}
probability <- count / num_simulations
print(probability)


count <- 0
for (i in 1:num_simulations) {
  x <- as.numeric(runif(n) < p)
  prop <- ci_proportion(x, type = "Wilson")$estimate
  ll <- ci_proportion(x, type = "Wilson")$interval[1]
  ul <- ci_proportion(x, type = "Wilson")$interval[2]
  if (ll < p && ul > p) {
    count <- count + 1
  }
}
probability <- count / num_simulations
print(probability)

count <- 0
for (i in 1:num_simulations) {
  x <- as.numeric(runif(n) < p)
  prop <- ci_proportion(x, type = "Agresti-Coull")$estimate
  ll <- ci_proportion(x, type = "Agresti-Coull")$interval[1]
  ul <- ci_proportion(x, type = "Agresti-Coull")$interval[2]
  if (ll < p && ul > p) {
    count <- count + 1
  }
}
probability <- count / num_simulations
print(probability)

