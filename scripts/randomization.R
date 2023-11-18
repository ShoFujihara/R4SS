permutation <- function(n, n1) {
  M <- choose(n, n1)
  treat.index <- combn(n, n1)
  Z <- matrix(0, n, M)
  for (m in 1:M) {
    treat <- treat.index[, m]
    Z[treat, m] <- 1
  }
  Z
}


# 処置の場合の数
choose(5, 3)
matrix(NA, 5, 10)

# 何番目が処置されたか
permutation(5, 3)
permutation(5, 2)

fisher_randomization_test <- function(group1, group2) {
  combined <- c(group1, group2)
  n <- length(combined)
  n1 <- length(group1)
  
  # Get permutations
  Z <- permutation(n, n1)
  M <- ncol(Z)
  
  # Calculate test statistic for actual data
  observed_statistic <- abs(mean(group1) - mean(group2))
  
  # Calculate test statistics for all permutations
  permuted_statistics <- numeric(M)
  for (m in 1:M) {
    permuted_group1 <- combined[Z[, m] == 1]
    permuted_group2 <- combined[Z[, m] == 0]
    permuted_statistics[m] <- abs(mean(permuted_group1) - mean(permuted_group2))
  }
  
  # Calculate p-value
  p_value <- mean(permuted_statistics >= observed_statistic)
  
  list(observed_statistic = observed_statistic, p_value = p_value)
}

group1 <- rnorm(10)
group2 <- rnorm(10) + .5

result <- fisher_randomization_test(group1, group2)
print(result)


# Agresti (2019) Fisher's Exact Test, p.48
library(tidyverse)
prob <- function(n, n1c, nr1, n11){
  choose(n1c, n11) * choose(n - n1c, nr1 - n11) / choose(n, nr1)
}

chisq <- function(n, n1c, nr1, n11){
  matrix(c(n11, n1c - n11,  nr1 - n11, n - n1c - nr1 + n11), nrow = 2, ncol = 2)
}

dplyr::tibble(n11 = 0:4) |> 
  mutate(prob = purrr::map_dbl(n11, ~prob(8,4,4,.)),
         p =  1 - cumsum(lag(prob, default = 0)),
         X2 = purrr::map_dbl(n11, ~chisq(8,4,4,.) |> 
                               chisq.test(correct = FALSE) |> 
                               pluck("statistic") |> 
                               unname()))

fisher <- function(n, n1c, nr1, n11, ...){
  tab <- matrix(c(n11, n1c - n11,  nr1 - n11, n - n1c - nr1 + n11), 
                nrow = 2, 
                ncol = 2)
  print(tab)
  fisher.test(tab,...)
}
map(0:4, ~fisher(8,4,4,.))
map(0:4, ~fisher(8,4,4,., alternative = "greater"))

map(92, ~fisher(885,173,439,., alternative = "greater"))

matrix(c(92, 173 - 92,  439 - 92, 885 - 173 - 439 + 92), 
       nrow = 2, 
       ncol = 2) |> fisher.test(alternative = "greater")

