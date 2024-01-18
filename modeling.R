# load libraries, dataset ####

# libraries
library(broom)
library(car)
library(gridExtra)
library(glmnet)
library(selectiveInference)
library(tidyverse)

# county data
county_data = read_csv('final-data.csv')

# set seed
set.seed(2023)

# initial linear regression ####

# model fit
ols_fit = county_data |> 
  dplyr::select(-fips, -state, -county) |> 
  mutate_all(scale) |> 
  lm(formula = perc_days_mentally_unhealthy ~ .)

# extract mse
ols_mse = summary(ols_fit)$sigma^2
# extract adjusted r squared
adj_r_squared = summary(ols_fit)$adj.r.squared

# ols visualizations ####

# regression table
ols_table = tidy(ols_fit) |> 
  arrange(desc(estimate)) |> 
  mutate(sig = case_when(p.value < 0.001 ~ 'Yes', T ~ 'No')) |> 
  rename(Term = term,
         Estimate = estimate,
         `Standard Error` = std.error,
         `Test Statistic` = statistic,
         `P-Value` = p.value,
         `Significant?*` = sig)

# significant predictors
ols_significant = ols_table |> 
  filter(`Significant?*` == 'Yes') |> 
  dplyr::select(-`Significant?*`)

# ols residuals vs fitted values

# split covariates, outcome
covariates = county_data |> 
  dplyr::select(-fips, - state, -county, -perc_days_mentally_unhealthy) |> 
  mutate_all(scale) |> 
  as.matrix()
outcome = county_data |> 
  dplyr::select(perc_days_mentally_unhealthy) |> 
  mutate_all(scale) |> 
  as.matrix()
# ols fitted values
ols_fitted = predict(ols_fit, newx = covariates)
# ols residuals
ols_residuals = outcome - ols_fitted
# plot
ols_residual_plot = ols_fitted |> 
  bind_cols(ols_residuals) |> 
  ggplot(aes(x = ols_fitted, y = ols_residuals)) +
  labs(x = 'Fitted Vallues', y = 'Residuals') +
  geom_point(pch = 20) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# vif table

# calculate vif
vif = vif(ols_fit)
# compile results in table
vif_table = tibble(Variables = names(vif), VIF = vif) |> 
  arrange(desc(vif))

# ols coefficient plot
ols_coef_plot = tidy(ols_fit) |> 
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) |> 
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  labs(x = 'Term', y = 'OLS Estimate', title = 'Coefficient Estimates by Term') +
  geom_col(color = 'white') +
  scale_fill_manual(values = c('darkred', 'darkblue'), 
                    guide = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# lasso regression ####

# model fit w/ 10-fold cv
lasso_fit = cv.glmnet(covariates, outcome, alpha = 1)
# extract coefficients
lasso_coef = tidy(lasso_fit$glmnet.fit)

# optimal lambda, 1se criteria
cv_lambda = lasso_fit$lambda.1se
# optimal coefficients
cv_coef = tidy(lasso_fit$glmnet.fit) |> 
  filter(lambda == cv_lambda) |> 
  dplyr::select(term, estimate) |> 
  mutate(
    # coefficient standard error
    se = c(NA, fixedLassoInf(x = covariates, y = outcome, beta = as.vector(coef(lasso_fit, x = covariates, y = outcome, s = cv_lambda))[-1], lambda = cv_lambda, alpha = 0.001)$sd),
    # test statistic
    test_statistic = estimate / se,
    # p value
    p_value = pnorm(abs(test_statistic), lower.tail = F),
    )

# extract mse
lasso_mse = lasso_fit$cvm[which(lasso_fit$lambda == cv_lambda)]

# lasso visualizations ####

# regression table
lambda_table = cv_coef |> 
  filter(term != '(Intercept)') |> 
  arrange(desc(estimate)) |> 
  mutate(sig = case_when(p_value < 0.001 ~ 'Yes', T ~ 'No')) |> 
  rename(Term = term,
         Estimate = estimate,
         `Standard Error` = se,
         `Test Statistic` = test_statistic,
         `P-Value` = p_value,
         `Significant?*` = sig)

# significant predictors
lasso_significant = lambda_table |> 
  filter(`Significant?*` == 'Yes') |> 
  dplyr::select(-`Significant?*`)

# coefficient estimates vs lambda
coef_vs_lambda = lasso_coef |>
  ggplot(aes(x = lambda, y = estimate, group = term)) +
  labs(x = expression(lambda), y = 'Coefficient Estimate', title = expression(paste('Coefficient Estimates vs ', lambda))) +
  scale_x_log10() +
  geom_line(alpha = 0.75) +
  geom_vline(xintercept = lasso_fit$lambda.min) +
  geom_vline(xintercept = cv_lambda, 
             linetype = 'dashed', color = 'red') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# number of non-zero predictors vs lambda
nonzero_vs_lambda = lasso_fit |>
  tidy() |> 
  ggplot(aes(x = lambda, y = nzero)) +
  labs(x = expression(lambda), y = 'Non-Zero Predictors', title = expression(paste('Number of Non-Zero Predictors vs ', lambda))) +
  geom_line() +
  geom_vline(xintercept = cv_lambda, 
             linetype = 'dashed', color = 'red') +
  scale_x_log10() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# LASSO coefficient plot
lasso_coef_plot = cv_coef |> 
  mutate(coef_sign = as.factor(sign(estimate)),
         term = fct_reorder(term, estimate)) %>%
  ggplot(aes(x = term, y = estimate, fill = coef_sign)) +
  labs(x = 'Term', y = 'LASSO Estimate') +
  geom_bar(stat = 'identity', color = 'white') +
  scale_fill_manual(values = c('darkred', 'darkblue'), 
                    guide = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# residuals vs fitted values

# lasso fitted
lasso_fitted = predict(lasso_fit, newx = covariates, s = cv_lambda)
# lasso residuals
lasso_residuals = outcome - lasso_fitted
# plot
lasso_residual_plot = lasso_fitted |> 
  bind_cols(lasso_residuals) |> 
  ggplot(aes(x = lasso_fitted, y = lasso_residuals)) +
  labs(x = 'Fitted Vallues', y = 'Residuals', title = 'Residuals vs Fitted Values') +
  geom_point(pch = 20) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))