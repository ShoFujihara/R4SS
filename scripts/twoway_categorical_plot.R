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
library(ggplot2)
library(DescTools)

# function
twoway_categorical_plot <- function(data, row, col,
                        varname_row = "Row",
                        varname_col = "Column",
                        label_col = "Column",
                        digits = 3) {
tab <- data |> 
    mutate(row = {{row}},
           col = {{col}}) |> 
  drop_na(row, col) |> 
  xtabs(~ row + col, data = _) 
N <- sum(tab)
X2 <- tab |> chisq.test() |> pluck("statistic")
df <- tab |> chisq.test() |> pluck("parameter")
p <- tab |> chisq.test() |> pluck("p.value")
CV <- DescTools::CramerV(tab, conf.level = .95)

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