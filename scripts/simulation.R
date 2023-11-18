
# simulation --------------------------------------------------------------

library(fabricatr)
library(estimatr)

set.seed(123456)
N <- 10e5
n <- 100
b0 <- 4
b1 <- 2
b2 <- - 3
b3 <- 1.5
b4 <- - 0.5

pop_data <- 
  fabricate(
  N = N,
  ID_label = "id",
  X1 = rnorm(N),
  X2 = rnorm(N),
  X3 = rnorm(N),
  X4 = rnorm(N),
  Y = rnorm(N, mean = b0 + b1 * X1 + b2 * X2 + b3 * X3 + b4 * X4, sd = 10)
  ) |> 
  relocate(id, Y) |> 
  tibble()
pop_data


pop_data <- 
  fabricate(
    pref = add_level(N = 47,
                     Z = rnorm(N)),
    city = add_level(N = 100,
                     W = rnorm(N)),
    id = add_level(N = 10,  
                     X1 = rnorm(N),
                     X2 = rnorm(N),
                     X3 = rnorm(N),
                     X4 = rnorm(N),
                     Y = rnorm(N, mean = b0 + b1 * X1 + b2 * X2 + b3 * X3 + b4 * X4, sd = 10))) |> 
  relocate(id, pref, city, Y, X1:X4, W, Z) |> 
  tibble()
pop_data |> 
  arrange(pref, city) |> 
  print(n = 50)

pop_data |> 
  fixest::feols(Y ~ X1 + X2 + X3 + X4, data = _)

pop_data |> 
  fixest::feols(Y ~ X1 + X2 + X3 + X4 | pref + city, data = _)

pop_data |> 
  fixest::feols(Y ~ X1 + X2 + X3 + X4 + Z + W, cluster = ~ pref + city, data = _)
