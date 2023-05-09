pacman::p_load(knitr,
               here)
purl(here("intro.qmd"), output = here("scripts", "intro.R"), documentation = 0)
purl(here("data.qmd"), output = here("scripts", "data.R"), documentation = 0)
purl(here("variables.qmd"), output = here("scripts", "variables.R"), documentation = 0)
