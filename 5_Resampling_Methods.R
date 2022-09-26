library(ISLR2)
library(tidyverse)

# Ex. 5.
default_ex <- Default

# a)
default_ex %>%
  glm(
    default ~ income + balance,
    data = .,
    family = binomial
  ) -> glm_default_1

# b)
# i.
set.seed(1)
default_ex$id <- 1:nrow(default_ex)
default_ex %>%
  sample_frac(.8) -> default_ex_train
anti_join(default_ex, default_ex_train, by = 'id') -> default_ex_test

# ii.
default_ex_train %>%
  glm(
    default ~ income + balance,
    data = .,
    family = binomial
  ) -> glm_default_2

# iii.
glm_default_pred_2 <- predict.glm(
  glm_default_2,
  newdata = default_ex_test,
  type = 'response'
)
glm_default_pred_2 <- ifelse(glm_default_pred_2 > .5, 'Yes', 'No')

# iv.
# A little function would be helpful...
conf_mat_fun <- function(conf_matrix) {
  print('Confusion matrix:')
  print(conf_matrix)
  print(paste(
    'Prediction accuracy (% of correctly predicted observations):',
    (sum(diag(conf_matrix)) / sum(conf_matrix) * 100) %>% round(., 2),
    '%'
  ))
  print(paste(
    'Classification error (% of incorrectly predicted observations):',
    (100 - (sum(diag(conf_matrix)) / sum(conf_matrix) * 100)) %>% round(., 2),
    '%'
  ))
  print(paste(
    'Sensitivity  (% of correctly predicted',
    rownames(conf_matrix)[2],
    'observations):',
    (conf_matrix[2, 2] / sum(conf_matrix[, 2]) * 100) %>% round(., 2),
    '%'
  ))
  print(paste(
    'Specificity  (% of correctly predicted',
    rownames(conf_matrix)[1],
    'observations):',
    (conf_matrix[1, 1] / sum(conf_matrix[, 1]) * 100) %>% round(., 2),
    '%'
  ))
}

glm_default_tab_2 <- table(glm_default_pred_2, default_ex_test$default)
conf_mat_fun(glm_default_tab_2)
# Prediction accuracy = 97.4%
# Classification error = 2.6%
# Sensitivity = 28.57% (correctly predicted Yes observations)
# Specificity = 99.9% (correctly predicted No observations)

# c)
# Three runs with different set.seed() values.
# 1st repetition (10)
# Prediction accuracy = 97.35%
# Classification error = 2.65%
# Sensitivity = 27.87% (correctly predicted Yes observations)
# Specificity = 99.54% (correctly predicted No observations)
# 2nd repetition (50)
# Prediction accuracy = 98.25%
# Classification error = 1.75%
# Sensitivity = 48.78% (correctly predicted Yes observations)
# Specificity = 99.29% (correctly predicted No observations)
# 3rd repetition (123)
# Prediction accuracy = 97.4%
# Classification error = 2.6%
# Sensitivity = 30.3% (correctly predicted Yes observations)
# Specificity = 99.69% (correctly predicted No observations)

# d)
# Note: go back to line 17, set seed to 1 and run lines 17:21 once again.
default_ex_train %>%
  glm(
    default ~ income + balance + student,
    data = .,
    family = binomial
  ) -> glm_default_3
glm_default_pred_3 <- predict.glm(
  glm_default_3,
  newdata = default_ex_test,
  type = 'response'
)
glm_default_pred_3 <- ifelse(glm_default_pred_3 > .5, 'Yes', 'No')
glm_default_tab_3 <- table(glm_default_pred_3, default_ex_test$default)
conf_mat_fun(glm_default_tab_3)
# Prediction accuracy = 97.25%
# Classification error = 2.75%
# Sensitivity = 24.29% (correctly predicted Yes observations)
# Specificity = 99.9% (correctly predicted No observations)
# Including dummy variable didn't change result in meaningfully. Classification error is even slightly higher.

# Ex. 6.
# a)
summary(glm_default_1)$coef[2:3, 'Std. Error']

# b)
boot.fn <- function(dataset, index) {
  dataset[index, ] %>%
    glm(
      default ~ income + balance,
      data = .,
      family = binomial
    ) %>% coef()
}

# c)
boot::boot(default_ex, boot.fn, 1000)

# d)
# Standard error obtained by bootstrap for income coefficient is slightly lower (0.00000487 vs 0.00000499), while for balance coefficient is a bit higher (0.000229 vs 0.000227) than error from logistic regression summary.

# Ex. 7.
# a)
weekly_ex %>%
  glm(
    Direction ~ Lag1 + Lag2,
    data = .,
    family = binomial
  ) -> glm_weekly_3

# b)
weekly_ex[-1, ] %>%
  glm(
    Direction ~ Lag1 + Lag2,
    data = .,
    family = binomial
  ) -> glm_weekly_4

# c)
glm_weekly_pred_4 <- predict.glm(
  glm_weekly_4,
  newdata = weekly_ex[1, ],
  type = 'response'
)
glm_weekly_pred_4 <- ifelse(glm_weekly_pred_4 > .5, 'Up', 'Down')
# Prediction is 'Up', while true value is 'Down'

# d)
error_int <- 0
for (i in 1:nrow(weekly_ex)) {
  weekly_ex[-i, ] %>%
    glm(
      Direction ~ Lag1 + Lag2,
      data = .,
      family = binomial
    ) -> temp_model
  temp_pred <- predict.glm(
    temp_model,
    newdata = weekly_ex[i, ],
    type = 'response'
  )
  temp_pred <- ifelse(temp_pred > .5, 'Up', 'Down')
  if (temp_pred != weekly_ex$Direction[i]) {
    error_int = error_int + 1
  }
}

# e)
error_int / nrow(weekly_ex)
# 0.45
# Logistic regression correctly predict 45% of observations.

# Ex. 8.
# a)
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x ^ 2 + rnorm(100)
# p = 2 (x, x ^ 2), n = 100
# y = x - 2 * x ^ 2 + e

# b)
tibble(x = x, y = y) %>%
  ggplot(., aes(x = x, y = y)) + 
  geom_point() +
  labs(
    title = 'y vs x',
    subtitle = 'y = x - 2 * x ^ 2 + e',
    caption = 'Source: own study'
  ) +
  theme_bw()
# There is quadratic relationship between y and x.

# c)
set.seed(123)
# First set.seed = 1, second = 123

# i.
cv_err_tib <- tibble(
  degree = 1:4,
  cv_error = rep(NA, 4)
)
xy_tib = tibble(x = x, y = y)
for (i in 1:4) {
  xy_tib %>%
    glm(
      y ~ poly(x, i),
      data = .
    ) -> temp_model

  boot::cv.glm(
    xy_tib,
    temp_model
  )$delta[2] -> cv_err_tib[i, 2]
}
cv_err_tib

# d)
# Run lines 209-230 with different set.seed() value.
# The results are identical regardless of seed set. This is because we use LOOCV, so in both cases we use the same sets for training and testing.

# e)
# The lowest LOOCV error is provided by quadratic fit. It's not a surprise, since there is quadratic relationship between y and x.

# f)
xy_tib %>%
  glm(
    y ~ poly(x, 1),
    data = .
  ) %>%
  summary()
# B0 and B1 are statistically significant.
xy_tib %>%
  glm(
    y ~ poly(x, 2),
    data = .
  ) %>%
  summary()
# B0 B1, and B2 are statistically significant.
xy_tib %>%
  glm(
    y ~ poly(x, 3),
    data = .
  ) %>%
  summary()
# B0 B1, and B2 are statistically significant.
xy_tib %>%
  glm(
    y ~ poly(x, 4),
    data = .
  ) %>%
  summary()
# B0 B1, and B2 are statistically significant.
# All results agree with the conslusions drawn based on the cross-validation - the best fit is provided by quadratic model.

# Ex. 9.
# a)
boston_ex$medv %>% mean() -> mu

# b)
sd(boston_ex$medv) / sqrt(length(boston_ex$medv))
# 0.41 - it means that sample means would not differ from population mean (22.53) more than this value.

# c)
boot_mean_fun <- function(dataset, index) {
  dataset[index] %>%
    mean()
}
mu_est <- boot::boot(boston_ex$medv, boot_mean_fun, 1000)
# Bootstrap estimate for standard error is 0.42 - a bit higher than estimate from b).

# d) 
mu_est$t0 - 2 * sd(mu_est$t) # 21.69
mu_est$t0 + 2 * sd(mu_est$t) # 23.37

boston_ex$medv %>% t.test()
# 95% confidence interval is [21.73, 23.34] - very close to obtained by bootstrap.

# e)
boston_ex$medv %>% median() -> mu_med

# f)
boot_med_fun <- function(dataset, index) {
  dataset[index] %>%
    median()
}
mu_med_est <- boot::boot(boston_ex$medv, boot_med_fun, 1000)
# SE = 0.39, which means that sample medians would not differ from population median (21.2) more than this value.

# g)
boston_ex$medv %>% quantile(probs = .1) -> mu_perc

# h)
boot_perc_fun <- function(dataset, index) {
  dataset[index] %>%
    quantile(probs = .1)
}
mu_perc_est <- boot::boot(boston_ex$medv, boot_perc_fun, 1000)
# SE = 0.5, which means that samples 10th percentiles would not differ from population 10th percentile (12.75) more than this value.



