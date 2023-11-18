# twoway_categorical_plot

# install ggplot2 package
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

# install DescTools package
if (!requireNamespace("DescTools", quietly = TRUE)) {
  install.packages("DescTools")
}

# load packages
library(tidyverse)
library(ggplot2)
library(DescTools)

# function
threeway_categorical_plot <- function(data, row, col, layer,
                        varname_row = "Row",
                        varname_col = "Column",
                        varname_layer = "Layer",
                        label_col = "Column",
                        label_layer = "Layer",
                        digits = 3) {
tab <- data |> 
    mutate(row = {{row}},
           col = {{col}},
           layer = {{layer}}) |> 
  drop_na(row, col, layer) |> 
  xtabs(~ row + col + layer, data = _) 

N <- X2test <- X2 <- df <- p <- CV <- list()
L <- sample_data$layer |> unique() |> length()
for (l in 1:2) {
N[[l]] <- sum(tab[,,l])
X2test[[l]] <- tab[,,l] |> chisq.test() 
X2[[l]]  <- X2test[[l]] |> pluck("statistic")
df[[l]]  <- X2test[[l]] |> pluck("parameter")
p[[l]]  <- X2test[[l]] |> pluck("p.value")
CV[[l]]  <- DescTools::CramerV(tab[,,l], conf.level = .95)[1]
}
print(tibble(L = 1:L,
             N = unlist(N), 
             X2 = unlist(X2),
             df = unlist(df),
             p = unlist(p),
             CV = unlist(CV[1])))
}

# 関数にサンプルデータを渡す
threeway_categorical_plot(sample_data, row, col, layer)

# サンプルデータを作成
set.seed(123)  # 再現性のための乱数シード設定
sample_data <- data.frame(
  row = factor(sample(1:3, 1000, replace = TRUE)),
  col = factor(sample(1:4, 1000, replace = TRUE)),
  layer = factor(sample(1:2, 1000, replace = TRUE))
)
DescTools::CramerV(tab[,,1], conf.level = .95)[1]
tab <- sample_data |> 
  drop_na(row, col, layer) |> 
  xtabs(~ row + col + layer, data = _) 

threeway_categorical_plot(sample_data, row, col, layer)



data |> 
    count({{row}}, {{col}}) |> 
    drop_na() |> 
    group_by({{row}}) |> 
    mutate(sub_total = sum(n),
           prop = n / sub_total) |>
    ungroup() |>
    mutate(row = haven::as_factor({{row}}),
           col = haven::as_factor({{col}})) |>
    ggplot(aes(x = forcats::fct_rev(row), 
               y = prop, 
               fill = forcats::fct_rev(col))) + 
    geom_col() + 
    geom_text(aes(label = scales::percent(round(prop, 3))),
              position = position_stack(vjust = .5)) +
    theme_minimal(base_family = "HiraKakuPro-W3") +
    coord_flip()  + 
    guides(fill = guide_legend(reverse = TRUE)) + 
    theme(legend.position = "top") + 
    scale_y_continuous(labels = scales::percent) + 
    labs(x = varname_row, y = varname_col,
         fill = label_col,
         caption = paste0("N = ",N ,
                          "\nX-squared = ", X2 |> round(digits) |> format(nsmall = digits), 
                          ", d.f. = ", df, 
                          ", p = ", p |> round(digits) |> format(nsmall = digits),
                          "\nCramer's V = ", CV[1] |> round(digits) |> format(nsmall = digits),
                          " (", CV[2] |> round(digits) |> format(nsmall = digits),
                          ", ", CV[3] |> round(digits) |> format(nsmall = digits),
                          ")")) + 
    scale_colour_viridis_d()
  }
