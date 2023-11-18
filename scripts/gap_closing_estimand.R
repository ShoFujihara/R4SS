# R Scripts for the Methods of Jackson and Vanderweele (2018) and Lundberg (2022)

# packages
library(gapclosing)
library(tidyverse)
library(broom)
library(estimatr)
library(marginaleffects)
library(ggeffects)

# simulate data
set.seed(123456)
simulated_data <- 
  generate_simulated_data(n = 10000) |> 
  mutate(id = 1:n(), 
         .before = category)
head(simulated_data)

# model 1
fit_1 <- lm(outcome ~ category,
            data = simulated_data)
tidy(fit_1)

# factual means
avg_predictions(fit_1, by = "category") |> 
  arrange(category)

# counterfactual means
avg_predictions(fit_1, variable = "category") |> 
  arrange(category)

# model 2
fit_2 <- lm(outcome ~ category * treatment * confounder,
            data = simulated_data)
tidy(fit_2)

# factual means
## method 1
avg_predictions(fit_2, by = "category") |> 
  arrange(category)

## method 2
augment(fit_2) |> 
  group_by(category) |> 
  summarise(mean = mean(.fitted))

# counterfactual marginal means
avg_predictions(fit_2, variable = "category") |> 
  arrange(category)

# adjusted mean
## method 1
ggeffects::ggpredict(fit_2, terms = "category",
                     condition = c(treatment = 0, confounder = 0)) |> 
  knitr::kable(format = "simple", digits = 5)

## method 2
augment(fit_2, newdata = simulated_data |> 
          mutate(treatment = 0,
                 confounder = 0)) |> 
  group_by(category) |> 
  summarise(mean = mean(.fitted)) |> 
  knitr::kable(format = "simple", digits = 5)

## method 3
avg_predictions(fit_2, 
                by = "category",
                newdata = simulated_data |> 
                  mutate(treatment = 0,
                         confounder = 0)) |> 
  arrange(category)

# outcome model -----------------------------------------------------------

# post-intervention means
## method 1
avg_predictions(fit_2, by = "category",
                                 newdata = simulated_data |> mutate(treatment = 1)) |> 
  arrange(category)

## method 2
avg_predictions(fit_2, variable = list(treatment = 1), by = "category")

## method 3
simulated_data |> 
  mutate(outcome_under_treatment_1 = predict(fit_2,
                                             newdata = simulated_data |> 
                                               mutate(treatment = 1))) |> 
  group_by(category) |> 
  summarize(factual = mean(outcome),
            counterfactual = mean(outcome_under_treatment_1))

## method 4
# gapclosing function
fit_gapclosing <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = 1,
  outcome_formula = formula(outcome ~ category * treatment * confounder),
  category_name = "category",
  treatment_name = "treatment")
estimate_tab_gapclosing_reg <- 
  tibble(fit_gapclosing$factual_means) |> 
  rename(factual = estimate) |> 
  mutate(counterfactual = fit_gapclosing$counterfactual_means["estimate"] |> pull())
estimate_tab_gapclosing_reg

# treatment model -----------------------------------------------------------

treatment_model <- glm(treatment ~ category * confounder,
                       family = binomial(link = "logit"),
                       data = simulated_data)
simulated_data <- simulated_data |> 
  mutate(counterfactual_treatment = 1,
         schatostic = dplyr::case_when(treatment == 1 ~ counterfactual_treatment,
                              treatment == 0 ~ 1 - counterfactual_treatment),
         ps = predict(treatment_model, type = "response"),
         ipw = treatment / ps + (1 - treatment) / (1 - ps),
         w = schatostic * ipw)

fit_3 <- 
  lm_robust(outcome ~ category, weights = w, data = simulated_data)

# same results
avg_predictions(fit_3, variable = "category") |> 
  arrange(category)
avg_predictions(fit_3, by = "category",
                newdata = simulated_data |> mutate(treatment = 1)) |> 
  arrange(category)
avg_predictions(fit_3, by = "category")

factual_means <- simulated_data %>%
  group_by(category) %>%
  summarize(mean_outcome = mean(outcome))
counterfactual_means <- simulated_data %>%
  group_by(category) %>%
  summarize(mean_outcome = weighted.mean(outcome, w))

estimate_tab_weighting <- tibble(factual_means) |> 
  rename(factual = mean_outcome) |> 
  mutate(counterfactual = counterfactual_means["mean_outcome"] |> pull())

fit_gapclosing_m <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = 1,
  treatment_formula = formula(treatment ~ category * confounder),
  outcome_name = "outcome",
  category_name = "category",
  treatment_name = "treatment",
  treatment_algorithm = "glm")

estimate_tab_gapclosing_weighting <- 
  tibble(fit_gapclosing_m$factual_means) |> 
  rename(factual = estimate) |> 
  mutate(counterfactual = fit_gapclosing_m$counterfactual_means["estimate"] |> pull())

estimate_tab_weighting
estimate_tab_gapclosing_weighting


# doubly robust -----------------------------------------------------------

fit_gapclosing_dr <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = 1,
  outcome_formula = formula(outcome ~  category * treatment * confounder),
  treatment_formula = formula(treatment ~  category * confounder),
  category_name = "category",
  treatment_name = "treatment")

estimate_tab_gapclosing_dr <- 
  tibble(fit_gapclosing_dr$factual_means) |> 
  rename(factual = estimate) |> 
  mutate(counterfactual = fit_gapclosing_dr$counterfactual_means["estimate"] |> pull())

estimate_tab_gapclosing_reg
estimate_tab_gapclosing_weighting
estimate_tab_gapclosing_dr


# Jackson and Vanderweele -------------------------------------------------
# Jackson, John W., and Tyler J. VanderWeele. 2018. 
# Decomposition Analysis to Identify Intervention Targets for Reducing Disparities:” 
# Epidemiology 29(6):825–35. doi: 10.1097/EDE.0000000000000901.

# データ生成
set.seed(1234567)  # 乱数の再現性を確保するためのシード設定

# シミュレーションデータのフレーム作成
simulated_data <- tibble(
  Race = numeric(),
  Age = numeric(),
  AFQT = numeric(),
  MotherHighestGrade = numeric(),
  ChildhoodPoverty = numeric(),
  ChildhoodIncome = numeric(),
  Wage = numeric()
)

# white
n_white <- 1010
simulated_data <- simulated_data |>
  add_row(Race = 0,
          Age =  rnorm(n_white, mean = 43.1, sd = 0.8),
          AFQT = rnorm(n_white, mean = 0.45, sd = 1.0),
          MotherHighestGrade = rnorm(n_white, mean = 11.9, sd = 2.4),
          ChildhoodPoverty = rnorm(n_white, mean = 9.6, sd = 29.5),
          ChildhoodIncome = rnorm(n_white, mean = 21466, sd = 12854))

# black
n_black <- 597
simulated_data <- simulated_data |>
  add_row(Race = 1,
          Age =  rnorm(n_black, mean = 43.1, sd = 0.8),
          AFQT = rnorm(n_black, mean = -0.58, sd = 0.8),
          MotherHighestGrade = rnorm(n_black, mean = 10.9, sd = 2.5),
          ChildhoodPoverty = rnorm(n_black, mean = 48.7, sd = 50.0),
          ChildhoodIncome = rnorm(n_black, mean = 10835, sd = 7799))

# データに対するWageの関数を定義
generate_wage <- function(data) {
  with(data, {
    wage <- -16.6 + 0.5 * Age + 2 * AFQT + 1.5 * MotherHighestGrade - 
      0.8 * ChildhoodPoverty + 0.0005 * ChildhoodIncome + 8.7 * Race
    
    # wageが負の値を取らないように調整
    wage <- pmax(wage, 0)
    
    ifelse(Race == 0, wage <- wage + (26.1 - mean(wage[Race == 0])), wage <- wage + (17.4 - mean(wage[Race == 1])))
    
    # 標準偏差を調整
    sd_correction <- sd(wage) / ifelse(Race == 0, 17.4, 17.4)
    wage <- wage / sd_correction
    
    return(wage)
  })
}

# Wageを生成しデータフレームに追加
simulated_data$Wage <- generate_wage(simulated_data)

# データフレームを出力
simulated_data <-
  simulated_data |> 
  mutate(log_wage = log(Wage)) |> 
  drop_na()
head(simulated_data)

# 集計
simulated_data |> 
  group_by(Race) |> 
  skimr::skim()

# analysis
fit_1 <- simulated_data |> 
  lm_robust(log_wage ~ Race, data = _)

fit_2 <- simulated_data |> 
  lm_robust(log_wage ~ Race + MotherHighestGrade, data = _)

fit_3 <- simulated_data |> 
  lm_robust(log_wage ~ Race + AFQT + MotherHighestGrade, data = _)

phi_1 <- coef(fit_1)["Race"]
gamma_1 <- coef(fit_2)["Race"]
theta_1 <- coef(fit_3)["Race"]

gamma_2 <- coef(fit_2)["MotherHighestGrade"]
theta_2 <- coef(fit_3)["MotherHighestGrade"]

fit_1 |> 
  avg_predictions(variable = "Race") |> 
  tidy()
fit_2 |> 
  avg_predictions(variable = "Race") |> 
  tidy()

# scenario 1
scenario_1 <-
  tibble(scenario = 1,
         initial_disparity = phi_1,
         residual_disparity = gamma_1,
         reduction = initial_disparity - residual_disparity,
         reduction_percent = reduction / initial_disparity)

# scenario 2
scenario_2 <-
  tibble(scenario = 2,
         initial_disparity = gamma_1,
         residual_disparity = theta_1,
         reduction = initial_disparity - residual_disparity,
         reduction_percent = reduction / initial_disparity)

# scenario 3
scenario_3 <-
  tibble(scenario = 3,
         initial_disparity = phi_1,
         residual_disparity = theta_1,
         reduction = initial_disparity - residual_disparity,
         reduction_percent = reduction / initial_disparity)

# scenario 4
scenario_4 <-
  tibble(scenario = 4,
         initial_disparity = phi_1,
         residual_disparity =  (theta_1 + (theta_2 / gamma_2) * (phi_1 - gamma_1)),
         reduction = initial_disparity - residual_disparity,
         reduction_percent = reduction / initial_disparity)

bind_rows(scenario_1, scenario_2, scenario_3, scenario_4)

# equalize AFQT to mean(simulated_data$AFQT)
simulated_data <- simulated_data |> 
  mutate(outcome_under_treatment = predict(fit_3,
                                           newdata = simulated_data |> 
                                             mutate(AFQT = mean(simulated_data$AFQT))))
# estimate counterfactual mean
estimate_tab_imp <- 
  simulated_data |> 
  group_by(Race) |> 
  summarize(factual = mean(log_wage),
            counterfactual = mean(outcome_under_treatment)) 
estimate_tab_imp

scenario_4_imp <-
  tibble(scenario = 4,
         initial_disparity = (estimate_tab_imp[2,2] - estimate_tab_imp[1,2]) |> as.double(),
         residual_disparity = (estimate_tab_imp[2,3] - estimate_tab_imp[1,3]) |> as.double(),
         reduction = initial_disparity - residual_disparity,
         reduction_percent = reduction / initial_disparity)

# gapclosing function
fit_gapclosing <- gapclosing(
  data = simulated_data,
  counterfactual_assignments = mean(simulated_data$AFQT),
  outcome_formula = formula(log_wage ~  Race + AFQT + MotherHighestGrade),
  category_name = "Race",
  treatment_name = "AFQT")

estimate_tab_gapclosing <- tibble(fit_gapclosing$factual_means) |> 
  rename(factual = estimate) |> 
  mutate(counterfactual = fit_gapclosing$counterfactual_means["estimate"] |> pull())

scenario_4_gapclosing <-
  tibble(scenario = 4,
         initial_disparity = (estimate_tab_gapclosing[2,2] - estimate_tab_gapclosing[1,2]) |> as.double(),
         residual_disparity = (estimate_tab_gapclosing[2,3] - estimate_tab_gapclosing[1,3]) |> as.double(),
         reduction = initial_disparity - residual_disparity,
         reduction_percent = reduction / initial_disparity)

# In Scenario 4, the coefficients-based method, imputation method, and the gap-closing function yield consistent results.
bind_rows(scenario_1, scenario_2, scenario_3, scenario_4, scenario_4_imp, scenario_4_gapclosing)



