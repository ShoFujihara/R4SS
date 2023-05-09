## # 足し算
## 1 + 1
## # 引き算
## 2 - 100
## # 掛け算
## 7 * 8
## # 割り算
## 123456 / 3
## # 累乗
## 2^3

# 足し算
1 + 1
# 引き算
2 - 100
# 掛け算
7 * 8
# 割り算
123456 / 3
# 累乗
2^3

# 足し算
1 + 2
1 + 2   # 足し算

# --------------------------
# 2023年5月9日
# --------------------------
# Rの基礎

# 足し算
1 + 2

# 平方根
sqrt(8)
# 底が2の対数
log2(8)
# 底が2の対数 log()関数の引数にbase = 2 を加える
log(8, base = 2)
# 自然対数 log関数のデフォルトのbase = exp(1)
log(8)
# 指数関数
exp(8)

# ベクトル
c(1,2,2,3)

# 連続した値
1:18

# 等差数列
seq(0,10,2) # 0から10まで2つずつ増加

# 繰り返し
rep(1,10) # 1を10個

# aというオブジェクトに4を代入
a <- 4

# 結果を表示
a

# bというオブジェクトに [1,2,3,4,5,5] というベクトルを代入
b <- c(1,2,3,4,5,5)
# 結果を表示
b

# piは3.141593
pi

# 平均50，標準偏差10の正規分布からランダムに100個のデータを取り出す
# 再現のため乱数を指定
set.seed(123456)
x <- rnorm(n = 100, mean = 50, sd = 10)
x

# ヒストグラム
hist(x)

# 総和
sum(x)

# 平均値
mean(x)

# 平均値を求めmean_xというオブジェクトに代入する
mean_x <- mean(x)
# mean_xというオブジェクトに対してround()関数を適用する．
round(mean_x, digits = 1)

# まとめて実行する
round(mean(x), digits = 1)

# 中央値
median(x)

# 不偏分散
var(x)

# 標準偏差
sd(x)

# 最大値
max(x)
# 最小値
min(x)

# 大きさ
length(x)

# xをコピー
x_mis <- x
# 10から20番目の要素をNAとする
x_mis[10:20] <- NA
# 欠損を含むデータの表示
x_mis
# 欠損値を含むサイズ
length(x_mis)
# 欠損値を除いたサイズ
sum(!is.na(x_mis))
# 欠損値をた観察ケースサイズを求める関数を作成する
complete_obs <- function(x) sum(!is.na(x))
complete_obs(x)

# 四分位数
quantile(x)

# 要約
summary(x)

## # 以下をコンソールに貼り付ければ，インストールされる（ただし時間がかかる）
## install.packages("tidyverse", dependencies = TRUE)  # データの整理
## install.packages("haven", dependencies = TRUE)  # データの読み込み
## install.packages("janitor", dependencies = TRUE)  # データの変換
## install.packages("here", dependencies = TRUE)   # プロジェクト内のファイルを表示
## install.packages("fs", dependencies = TRUE)  # フォルダの作成

library(tidyverse)  # データの整理
library(haven)  # データの読み込み
library(janitor)  # 度数分布表
library(here)  # プロジェクト内のファイルを表示
library(fs)  # ファイルの作成

# pacmanパッケージがインストールされていない場合は以下からインストールを実行
#install.packages("pacman")
# パッケージのインストール（もしインストールされていなければ）と呼び出し
pacman::p_load(tidyverse,
               haven,
               janitor)

# 総和
x |> sum()
# 平均値
x |> mean()
# 平均値を丸める
x |> mean() |> round(digits = 1)
# 中央値
x |> median()
# 不偏分散
x |> var()
# 標準偏差
x |> sd()
# 最大値
x |> max()
# 最小値
x |> min()
# 大きさ
x |> length()
# 四分位数
x |> quantile()
# 要約
x |> summary()

library(magrittr)
x %>% mean()
x |> mean()

getwd()

# 作業ディレクトリの確認
getwd()

# 作業ディレクトリの確認
here()

dir_create(here(c("scripts",           # Rスクリプトを保存
                  "figures",           # 図を保存
                  "tables",           # 表を保存
                  "documents",        # 先行研究や資料を保存するフォルダ
                  "manuscripts"      # 原稿を保存
                  )))

here("data", "raw")
here("data", "processed")

dir_create(here("data", "raw"))        # データを保存（生データ）
dir_create(here("data", "processed"))  # データを保存（処理したデータ）

# u001.csvの場所の表示
here("data", "raw", "u001.csv")
