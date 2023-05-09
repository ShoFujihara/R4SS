# パッケージの呼び出し
pacman::p_load(
  here,
  readr,   # csvファイルを読み込む．tidyverseとしてもよい．
  haven   # sav, dta, sasファイルなどを読み込む．
  )

## u001 <- read.csv("~/GitHub/R4SS/data/raw/u001.csv")

library(readr)
u001 <- read_csv("data/raw/u001.csv")

# read.csvでcsvデータを読み込む
d_csv_1 <- read.csv("data/raw/u001.csv")
# 確認
head(d_csv_1)
# データのクラス
class(d_csv_1)

d_csv_1 <- read.csv(here("data","raw","u001.csv"))

# read_csvでcsvデータを読み込む
d_csv_2 <- read_csv("data/raw/u001.csv")
# 確認
head(d_csv_2)
# データのクラス
class(d_csv_2)

d_csv_2 <- read_csv(here("data","raw","u001.csv"))

# read_dtaでdtaデータを読み込む
d_dta <- read_dta("data/raw/u001.dta")
# 確認
head(d_dta)
# データのクラス
class(d_dta)

d_dta <- read_dta(here("data","raw","u001.dta"))

# read_savでsavデータを読み込む
d_sav <- read_sav("data/raw/u001.sav")
# 確認
head(d_sav)
# データのクラス
class(d_sav)

d_sav <- read_sav(here("data","raw","u001.sav"))

d_sav_nolab <- d_sav |> zap_label() |> zap_labels()
d_sav_nolab
