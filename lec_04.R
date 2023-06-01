# データ
library(tidyverse)
d <- read_csv("data/PJ010.csv")
dim(d)

# パッケージの読み込み
library(janitor)

# 度数分布を確認
d |> tabyl(c1_19) |> adorn_pct_formatting()
d |> tabyl(m1_25m_b) |> adorn_pct_formatting()

# 数値にラベルを与え，ラベルの水準を定める．
d <- d |> mutate(
  asp3 = case_match(c1_19,
                    1:2 ~ "JH/HS",
                    3:5 ~ "JC/PTC",
                    6:7 ~ "UNIV",
                    .default = NA_character_) |>
    fct_relevel("JH/HS", "JC/PTC", "UNIV"), 
  medu3 = case_match(m1_25m_b,
                    1:3 ~ "JH/HS",
                    4:6 ~ "JC/PTC",
                    7:8 ~ "UNIV",
                    .default = NA_character_) |>
    fct_relevel("JH/HS", "JC/PTC", "UNIV")
)

# 確認
d |> count(c1_19, asp3)
d |> count(m1_25m_b, medu3)

# 度数分布の確認
d |> tabyl(asp3) |> adorn_pct_formatting()
d |> tabyl(medu3) |> adorn_pct_formatting()

# install.packages("gtsummary", dependencies = TRUE)
library(gtsummary)
d |> 
  dplyr::select(medu3, asp3) |>  # クロス表をみたい変数のみを選択する
  tbl_summary()

## library(gt)
## d |>
##   dplyr::select(medu3,asp3) |>
##   tbl_summary() |>
##   as_gt() |>
##   gtsave(filename = "tables/table1.png")

# 基本的なクロス表
tab <- table(d$medu3,d$asp3)
tab
# 次のようにしてもよい
tab <- with(d, table(medu3, asp3))
tab
# xtabsを使う
tab <- xtabs(~ medu3 + asp3, data = d)
tab

# 慣れたら次のようにする．
tab <- d |> xtabs(~ medu3 + asp3, data = _)
tab

d |> 
  tabyl(medu3, asp3, show_na = FALSE)

d |> 
  tabyl(medu3, asp3, show_na = FALSE) |>
  adorn_percentages("row")

d |> 
  tabyl(medu3, asp3, show_na = FALSE) |>  # naのないクロス表
  adorn_percentages("row") |>  # 行％を表示
  adorn_pct_formatting(digits = 1) |>  # %で表示
  adorn_ns() |>  # 度数も表示
  adorn_title(placement = "combined") # 列の変数名

# install.packages("knitr", dependencies = TRUE)
library(knitr)
d |> 
  tabyl(medu3, asp3, show_na = FALSE) |>  # naのないクロス表
  adorn_percentages("row") |>  # 行％を表示
  adorn_pct_formatting(digits = 1) |>  # %で表示
  adorn_ns() |>  # 度数も表示
  adorn_title(placement = "combined") |>  # 列の変数名
  knitr::kable()  # 値を表示

d |> 
  tabyl(medu3, asp3, show_na = FALSE) |>  # naのないクロス表
  adorn_percentages("row") |>  # 行％を表示
  adorn_pct_formatting(digits = 1) |>  # %で表示
  adorn_ns() |>  # 度数も表示
  adorn_title(placement = "combined") |>  # 列の変数名
  knitr::kable(format = "simple")  # 値を表示

# 論文に掲載するクロス表(度数のみ)
d |> 
  tbl_cross(row = medu3,
            col = asp3, 
            label = list(medu3 ~ "Mother's Education", 
                         asp3 ~ "Child's Aspiration"),
            missing = "no",
            margin_text = "Total")

# 行パーセント
d |> 
  tbl_cross(row = medu3,
            col = asp3, 
            label = list(medu3 ~ "Mother's Education", 
                         asp3 ~ "Child's Aspiration"),
            missing = "no",
            margin_text = "Total",
            percent = "row")

# 列パーセント
d |> 
  tbl_cross(row = medu3,
            col = asp3, 
            label = list(medu3 ~ "Mother's Education", 
                         asp3 ~ "Child's Aspiration"),
            missing = "no",
            margin_text = "Total",
            percent = "col")

library(gt)
# 行パーセント
d |>
  tbl_cross(row = medu3,
            col = asp3, 
            label = list(medu3 ~ "Mother's Education", 
                         asp3 ~ "Child's Aspiration"),
            missing = "no",
            margin_text = "Total",
            percent = "row") |>
  as_gt() |> 
  gtsave(filename = "tables/table2.png")

library(vcd)
mosaic(~ medu3 + asp3, 
       data = d,
       main = "Mother's Education and Child's Aspiration",
       shade = TRUE)

png("figures/mosaic.png")
mosaic(~ medu3 + asp3, data = d,
       main = "Mother's Education and Child's Aspiration",
       shade = TRUE)
dev.off()

640 * 246 / 1709  # 1行1列
640 * 294 / 1709  # 1行2列
640 * 1169 / 1709  # 1行3列

797 * 246 / 1709  # 2行1列
797 * 294 / 1709  # 2行2列
797 * 1169 / 1709  # 2行3列

272 * 246 / 1709  # 3行1列
272 * 294 / 1709  # 3行2列
272 * 1169 / 1709  # 3行3列

# outer関数の基礎
x <- 1:3
y <- 1:5
outer(x, y, "*")

# クロス表のまま期待度数を計算
tab_exp <- outer(rowSums(tab), colSums(tab), "*") / sum(tab)
tab_exp

tab_std_res <- (tab - tab_exp) / sqrt(tab_exp)
tab_std_res

tab_std_res2 <- tab_std_res^2
tab_std_res2
X2 <- sum(tab_std_res2)
X2

# 引数が表
chisq.test(tab)
# 引数が2つの変数
chisq.test(d$medu3, d$asp3)

# カイ二乗検定の結果
chisq.test(tab)
# カイ二乗統計量を取り出し，オブジェクトとして保存
X2 <- chisq.test(tab)$statistic
X2  # そのままだと名前が残る
X2 <- X2 |> unname()  # unnameで名前を取る
X2  # 名前が消えている．

phi <- sqrt(X2/sum(tab))
phi

V <- sqrt(X2/sum(tab) / min(dim(tab) - 1)) 
V

summary(assocstats(tab))

# install.packages("DescTools", dependencies = TRUE)
library(DescTools)
Desc(tab)
Assocs(tab, conf.level = 0.95)

# データの準備
library(magrittr)
Condition <- rep(c(1,2), times = c(20,32))
Type <- rep(c(1,2,1,2,1,2,1,2), times = c(4,8,3,5,2,12,3,15))
Colour <- rep(c(1,2,1,2), times = c(12,8,14,18))
d <- tibble(Colour, Type, Condition)
d <- d |> mutate(Condition = factor(Condition, 
                                     levels = 1:2, 
                                     labels = c("Dirty","Clean")),
              Type = factor(Type, 
                            levels = 1:2, 
                            labels = c("Court","Plain")),
              Colour = factor(Colour, 
                              levels = 1:2, 
                              labels = c("Red","Black")))

# Dirty (Table 2)
d |> filter(Condition == "Dirty") |> 
  tbl_cross(row = Colour,
            col = Type, 
            percent = "col")
# Clean (Table 2)
d |> filter(Condition == "Clean") |> 
  tbl_cross(row = Colour,
            col = Type,
            percent = "col")

# Total (Table 3)
d |> tbl_cross(row = Colour,
               col = Type,
               percent = "col")

# データの準備
Gender <- rep(c(1,2), times = c(20,32))
Treatment <- rep(c(1,2,1,2,1,2,1,2), times = c(4,8,3,5,2,12,3,15))
Outcome <- rep(c(1,2,1,2), times = c(12,8,14,18))
d <- tibble(Outcome, Treatment, Gender)
d <- d |> mutate(Gender = factor(Gender, 
                                  levels = 1:2, 
                                  labels = c("Male","Female")),
              Treatment = factor(Treatment, 
                                 levels = 1:2, 
                                 labels = c("Untreated","Treated")),
              Outcome = factor(Outcome, 
                               levels = 1:2, 
                               labels = c("Alive","Dead")))

# Dirty (Table 2)
d |> filter(Gender == "Male") |> 
  tbl_cross(row = Outcome,
            col = Treatment,
            percent = "col")
# Clean (Table 2)
d |> filter(Gender == "Female") |> 
  tbl_cross(row = Outcome,
            col = Treatment,
            percent = "col")

# Total (Table 3)
d |> 
  tbl_cross(row = Outcome,
            col = Treatment,
            percent = "col")

library(dagitty)
g1 <- dagitty('dag{ 
  X [pos="0,1"]
  Y [pos="1,1"]
  Z [pos="0.5,0.5"]
  X -> Y
  Z -> X
  Z -> Y
  }')
plot(g1)

g2 <- dagitty('dag{ 
  X [pos="0,1"]
  Y [pos="1,1"]
  Z [pos="0.5,0.5"]
  X -> Y
  X -> Z
  Y -> Z
  }')
plot(g2)

g3 <- dagitty('dag{ 
  X [pos="0,1"]
  Y [pos="1,1"]
  Z [pos="0.5,0.5"]
  X -> Y
  X -> Z
  Z -> Y
  }')
plot(g3)

# データの準備
Univ <- rep(c(1,2), times = c(1100,500))
Exam <- rep(c(1,2,1,2,1,2,1,2), times = c(550,450,80,20,50,150,120,180))
Gender <- rep(c(1,2,1,2), times = c(1000,100,200,300))
d <- tibble(Exam, Gender, Univ)
d <- d |> mutate(Univ = factor(Univ, 
                            levels = 1:2, 
                            labels = c("Univ A","Univ B")),
              Exam = factor(Exam, 
                            levels = 1:2, 
                            labels = c("Pass","Fail")),
              Gender = factor(Gender, 
                              levels = 1:2, 
                              labels = c("Male","Female")),)

# 全体受験生
d |> tbl_cross(row = Exam,
               col = Gender,
               percent = "col")

# うちA大学
d |> filter(Univ == "Univ A") |>
  tbl_cross(row = Exam,
            col = Gender, percent = "col")
# うちB大学
d |> filter(Univ == "Univ B") |> 
  tbl_cross(row = Exam,
            col = Gender, percent = "col")

d <- d |>  dplyr::select(Exam, Univ, Gender)
# 全体受験生
d |> tbl_cross(row = Exam,
               col = Univ,
               percent = "col")
# うちA大学
d |> filter(Gender == "Male") |> 
  tbl_cross(row = Exam,
            col = Univ,
            percent = "col")
# うちB大学
d |> filter(Gender == "Female") |>
  tbl_cross(row = Exam,
            col = Univ,
            percent = "col")
