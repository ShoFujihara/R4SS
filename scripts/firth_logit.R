# 必要なパッケージの読み込み
library(brglm)
library(purrr)
library(broom)

# シミュレーションの設定
set.seed(42) # 乱数の再現性のためにseedを設定
n <- 50000 # サンプルサイズ
p <- 4   # パラメータ数
simulations <- 500 # シミュレーション回数

# 関数を定義して1回のシミュレーションを行う（通常のロジスティック回帰）
run_glm <- function() {
  X <- matrix(rnorm(n * p), nrow = n)
  beta_true <- c(1, -2, 0.5, -0.5)
  prob <- plogis(X %*% beta_true)
  y <- rbinom(n, 1, prob)
  fit <- glm(y ~ X, family = binomial(link = "logit"))
  tidy(fit) # 結果を整形して返す
}

# 関数を定義して1回のシミュレーションを行う（Firth logit）
run_firth_logit <- function() {
  X <- matrix(rnorm(n * p), nrow = n)
  beta_true <- c(1, -2, 0.5, -0.5)
  prob <- plogis(X %*% beta_true)
  y <- rbinom(n, 1, prob)
  fit <- brglm(y ~ X, family = binomial(link = "logit"))
  tidy(fit) # 結果を整形して返す
}

# 100回のシミュレーションを実行
glm_results <- map_df(1:simulations, ~run_glm())
firth_logit_results <- map_df(1:simulations, ~run_firth_logit())

# 結果の表示
print("通常のロジスティック回帰の結果:")
print(glm_results)

print("Firth logitの結果:")
print(firth_logit_results)

fit_data <- 
  bind_rows(glm_results, firth_logit_results, .id = "Method") |> 
  mutate(Method = factor(Method, levels = 1:2, labels = c("logit", "firth logit"))) 

fit_data |> 
  ggplot(aes(x = estimate, linetype = Method, color = Method)) + 
  geom_density() + 
  geom_vline(data = fit_data |> filter(term == "X1"), aes(xintercept = 1)) + 
  geom_vline(data = fit_data |> filter(term == "X2"), aes(xintercept = -2)) + 
  geom_vline(data = fit_data |> filter(term == "X3"), aes(xintercept = .5)) + 
  geom_vline(data = fit_data |> filter(term == "X4"), aes(xintercept = -.5)) + 
  facet_wrap(~term) + 
  theme_minimal()

# バイアスの計算
bias_data <- fit_data |>
  mutate(true_estimate = case_match(term,
                                    "X1" ~ 1,
                                    "X2" ~ -2,
                                    "X3" ~ .5,
                                    "X4" ~ -.5)) |> 
  summarise(bias = mean(estimate - true_estimate), 
            variance = mean((estimate - true_estimate)^2),
            .by = c(Method, term)) 
bias_data
