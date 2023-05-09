# パッケージの呼び出し
pacman::p_load(knitr,
               here)

# qmdファイルからRスクリプトのみを抜き出す
purl(here("intro.qmd"), output = here("scripts", "intro.R"), documentation = 0)
purl(here("data.qmd"), output = here("scripts", "data.R"), documentation = 0)
purl(here("variables.qmd"), output = here("scripts", "variables.R"), documentation = 0)
