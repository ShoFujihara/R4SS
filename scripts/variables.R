pacman::p_load(tidyverse,
               haven,
               janitor,
               here)
d <- read_dta(here("data","raw","u001.dta"))

## ybirth

d$ybirth

d |> count(ybirth)

d |> tabyl(ybirth)

d |> tabyl(ybirth) |> tibble()

library(dplyr)

d |> mutate(age_2006 = 2006 - ybirth)
d |> mutate(age_2006 = 2006 - ybirth, .before = 1)
d <- d |> mutate(age_2006 = 2006 - ybirth)
d |> count(ybirth, age_2006)

d |> select(ybirth)

d |> pull(ybirth)

d |> filter(ybirth == 1981)
