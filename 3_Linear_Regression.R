library(ISLR2)
library(tidyverse)
library(ggfortify)

# Ex. 8.
# a)
auto_ex <- Auto
auto_ex %>% 
  lm(mpg ~ horsepower, data = .) -> lm_auto_1
lm_auto_1 %>% summary()
# We can use F-Statistic to check significance of model. P-value very close to 0 suggests that model is statistically significant.
# R2 is considered below.
# Percentage error is 20.92%.
(summary(lm_auto_1)$sigma / mean(auto_ex$mpg) * 100) %>% round(., 2) # Percentage error.
# We can also check whether autocorrelation of errors using Durbin-Watson test:
car::dwt(lm_auto_1)$dw
# Test statistic is 0.92, which suggests positive autocorrelation (this is bad!)
# Another test: this time Breusch-Pagan test for heteroscedasticity:
car::ncvTest(lm_auto_1)$p
# P-value is 0.002 - we can assume, that error variance is constant.

# i.
# Yes, there is relationship between mpg and horsepower. According to p-value coefficient for horsepower significantly differs from 0.

# ii.
# R2 = 0.61, which means that 61% of variance in mpg is explained by horsepower. We can safely asume strong relationship between response and predictor.

# iii.
# Relationship between response and predictor is negative. Increase of horsepower by 1 unit would decrease mpg by 0.16.

# iv.
lm_auto_1 %>%
  predict(., newdata = data.frame(horsepower = 98), interval = 'prediction')
# Model predicts that mpg for engine with 98hp is 24.47. Confidence interval for this prediction is (23.97, 24.96)

# b)
auto_ex %>%
  ggplot(., aes(x = horsepower, y = mpg)) +
  geom_point() +
  geom_smooth(method = 'lm', color = 'blue', fill = 'blue') +
  labs(
    title = 'Miles per gallon vs engine horsepower',
    subtitle = 'with least squares regression line',
    x = 'Engine horsepower',
    y = 'Miles per gallon',
    caption = 'Source: Auto dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# c)
autoplot(lm_auto_1)
# Residuals vs Fitted plot suggests non-linear relationship between mpg and horsepower. Polynomial or logistic regression may provide better fit.
# Normal Q-Q and Scale-Location plots suggests outliers (observations no 323, 330 and 334).
auto_ex %>%
  slice(c(323, 330, 334))
# We can check that using Bonferroni outlier test from package car:
out_test <- car::outlierTest(lm_auto_1)
out_test
# According to the test only observation no 323 is an outlier.
auto_ex %>%
  slice(out_test$p %>% names() %>% as.numeric())
# Observation no 323 has horsepower = 65 with mpg = 46.6. Let's see if there is other similar observations:
auto_ex %>%
  filter(horsepower == 65)
# There is other 9 cars with hp = 65. 8 of them has mpg below 40.

# Residuals vs Leverage plot suggests observations with high leverage.
# Let's wrap it in a function:
high_lev_fun <- function(model, dataset) {
  tibble(
    leverage = hatvalues(model),
    row_num = seq(1, nrow(dataset), 1)
  ) %>%
    filter(
      leverage > 3 * length(model$coef) / nrow(dataset)
    ) -> high_lev_obs
  dataset %>%
    slice(high_lev_obs$row_num)
}

high_lev_fun(lm_auto_1, auto_ex)
# There are 14 observations identified as high leverage points. They have relatively low range (all values in 1st quantile of population) and high horsepower value (all values in 4th quantile of Auto dataset).

# Ex. 9.
# a)
auto_ex %>%
  GGally::ggpairs(., columns = 1:8)

# b)
auto_ex[, 1:8] %>% cor() %>% round(., 4)

# c)
auto_ex %>%
  lm(mpg ~ . -name, data = .) -> lm_auto_2
lm_auto_2 %>% summary()
# R2 is 0.82, which means that 82% of variance in mpg is explained by predictors.
# Percentage error is 14.19%.
(summary(lm_auto_2)$sigma / mean(auto_ex$mpg) * 100) %>% round(., 2) # Percentage error.
# We can also check whether autocorrelation of errors using Durbin-Watson test:
car::dwt(lm_auto_2)$dw
# Test statistic is 1.31, which suggests positive autocorrelation (this is bad!)
# Another test: this time Breusch-Pagan test for heteroscedasticity:
car::ncvTest(lm_auto_2)$p
# P-value is almost 0 - we can assume, that error variance is constant.
# We should also check multicollinearity:
car::vif(lm_auto_2)
# Number of cylinders, vehicle weight, engine displacement and horsepower have high variance inflation factor. However, cylinders and horsepower are not significant variables in this model, so we could consider removing them - or combining them with engine displacement.

# i.
# Number of cylinders, engine horsepower and acceleration are not statistically significant in this model.

# ii.
# Engine displacement, vehicle weight, model year and origin of car have a statistically significant relationship to the response.

# iii.
# Coefficient for the year variable is positive. It suggests that newer cars have higher range, i.e. increase of year by 1 would increase miles per gallon by 0.75.

# d)
autoplot(lm_auto_2)
# Residuals vs Fitted plot suggests non-linear relationships between mpg and other variables.
# Normal Q-Q and Scale-Location plots suggests outliers.
out_test <- car::outlierTest(lm_auto_2)
out_test
# According to the test only observation no 323 is an outlier - the same one as in ex. 8c).
# Residuals vs Leverage plot suggests observations with high leverage.

high_lev_fun(lm_auto_2, auto_ex)
# There are 5 observations identified as high leverage points.

# e)
auto_ex %>%
  lm(
    mpg ~ . -name + displacement * cylinders + displacement * horsepower,
    data = .
  ) -> lm_auto_3
lm_auto_3 %>% summary()
# Cylinders and cylinders * displacement are not statistically significant. However, acceleration and horsepower became significant (they weren't in previous model), as well as displacement * horsepower.

# f)
auto_ex %>%
  lm(
    mpg ~ cylinders + exp(displacement) + 
      log(horsepower) + poly(weight, 2) + 
      acceleration + year + origin,
    data = .
  ) -> lm_auto_4
lm_auto_4 %>% summary()
# Cylinders and acceleration are insignificant variables.

# Ex. 10.
carseats_ex <- Carseats
# a)
carseats_ex %>%
  lm(Sales ~ Price + Urban + US, data = .) -> lm_carseats_1
# b)
lm_carseats_1 %>% summary()
# Intercept is statistically significant. Stores would sell 13 043 car seats assuming the other variables are zero. 
# Price is statistically significant and negative. Increase in price by 1$ would decrease sales by 54 seats (assuming the other variables did not change).
# Urban is statistically insignificant and negative. If store is in an urban location, it would decrease sales by 22 seats (assuming the other variables did not change).
# US is statistically significant and positive If store is in the USA, it would increase sales by 1 201 seats (assuming the other variables did not change).

# c)
# Sales = 13.04 - 0.05 * Price - 0.02 * Urban (Yes - 1, No - 0) + 1.2 * US (Yes - 1, No - 0)

# d)
carseats_ex %>%
  lm(Sales ~ ., data = .) -> lm_carseats_2
lm_carseats_2 %>% summary()
# CompPrice, Income, Advertising, Price, ShelveLoc and Age are statistically significant.

# e)
carseats_ex %>%
  lm(
    Sales ~ CompPrice + Income + 
      Advertising + Price + 
      ShelveLoc + Age,
    data = .
  ) -> lm_carseats_3

# f)
lm_carseats_3 %>% summary()
# First model has R2 = 0.24, while last model has R2 = 0.87. The latter much better fits the data.

# g)
lm_carseats_3 %>% confint()

# h)
car::outlierTest(lm_carseats_3)
# Test suggests that observation 358 is an outlier.

high_lev_fun(lm_carseats_3, carseats_ex)
# There is one observation which could be considered as high leverage point: 311.

# Ex. 11
set.seed(1)
x <- rnorm(100)
y <- 2 * x + rnorm(100)

# a)
lm(y ~ x + 0) -> lm_xy_1
lm_xy_1 %>% summary()
# x is statistically significant. It's coefficient is 1.99 with standard error 0.11, t-statistic 18.73 and p-value 0, which means that we can reject null hypothesis.

# b)
lm(x ~ y + 0) -> lm_xy_2
lm_xy_2 %>% summary()
# y is statistically significant. It's coefficient is 0.39 with standard error 0.02, t-statistic 18.73 and p-value 0, which means that we can reject null hypothesis.

# c)
# Both models have exactly the same t-statistics and p-value.

# d)
beta <- sum(x * y) / sum(x ^ 2)
first_eq <- 
  beta / sqrt(
    sum((y - x * beta) ^ 2) /
      ((length(y) - 1) * sum(x ^ 2))
  )

second_eq <-
  sqrt(length(y) - 1) * sum(x * y) / sqrt(
    sum(x ^ 2) * sum(y ^ 2) - sum(x * y) ^ 2
  )
first_eq == second_eq
# TRUE

# e)
# Sections a-c) have already shown that t-statistic remains the same, regardless of which variable is response/predictor.

# f)
lm(y ~ x) -> lm_xy_3
lm(x ~ y) -> lm_xy_4
summary(lm_xy_3)$coef[, 't value'][2] # 18.5556
summary(lm_xy_4)$coef[, 't value'][2] # 18.5556

# Ex. 12.
# a)
# The coefficient estimate remains the same if X = Y.

# b)
# Ex. 11. is exactly about that.

# c)
x <- rnorm(100)
x %>% sample() -> y
lm(y ~ x + 0) -> lm_xy_5
lm(x ~ y + 0) -> lm_xy_6
summary(lm_xy_5)$coef[, 'Estimate'] # 0.1403
summary(lm_xy_6)$coef[, 'Estimate'] # 0.1403


# Ex. 13.
# a)
set.seed(1)
x <- rnorm(100)

# b)
eps <- rnorm(100, 0, sqrt(.25))

# c)
y <- -1 + .5 * x + eps
# Length of y: 100, B0: -1, B1: 0.5

# d)
tibble(x = x, y = y) %>%
  ggplot(., aes(x = x, y = y)) +
  geom_point() +
  labs(
    title = 'y vs x',
    subtitle = 'y = -1 + 0.5 * x + e',
    caption = 'Source: random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw() -> plot_xy_1
plot_xy_1
# There is a positive linear relationship between y and x.

# e)
lm(y ~ x) -> lm_xy_7
lm_xy_7 %>% summary()
# If x = 0, then y = -1.01. If x increase by 1 unit, then y would increase by 0.49.
# Estimated coefficients are very close to real coefficients (-1.01885 vs -1 and 0.49947 vs 0.5)

# f)
plot_xy_1 +
  geom_smooth(method = 'lm', color = 'blue', fill = 'blue')

# g)
lm(y ~ poly(x, 2)) -> lm_xy_8
lm_xy_8 %>% summary()
# Quadratic term doesn't improve the model significantly - R2 for linear model is 0.47, while R2 for polynomial model is 0.48. Moreover, coefficient for x^2 is not statistically significant.

# h)
eps <- rnorm(100, 0, sqrt(.1))
y <- -1 + .5 * x + eps

tibble(x = x, y = y) %>%
  ggplot(., aes(x = x, y = y)) +
  geom_point() +
  labs(
    title = 'y vs x',
    subtitle = 'y = -1 + 0.5 * x + e',
    caption = 'Source: random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw() -> plot_xy_2
plot_xy_2

lm(y ~ x) -> lm_xy_9
lm_xy_9 %>% summary()
# If x = 0, then y = -0.99 If x increase by 1 unit, then y would increase by 0.51.
# Estimated coefficients are very close to real coefficients (-0.99135 vs -1 and 0.50669 vs 0.5)

plot_xy_2 +
  geom_smooth(method = 'lm', color = 'blue', fill = 'blue')

# Reduction of noise resulted in higher R2 (0.66) and lower RSE (0.33).

# i)
eps <- rnorm(100)
y <- -1 + .5 * x + eps

tibble(x = x, y = y) %>%
  ggplot(., aes(x = x, y = y)) +
  geom_point() +
  labs(
    title = 'y vs x',
    subtitle = 'y = -1 + 0.5 * x + e',
    caption = 'Source: random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw() -> plot_xy_3
plot_xy_3

lm(y ~ x) -> lm_xy_10
lm_xy_10 %>% summary()
# If x = 0, then y = -0.94 If x increase by 1 unit, then y would increase by 0.44.
# Estimated coefficients differ more from real coefficients than in previous models (-0.9423 vs -1 and 0.4443 vs 0.5)

plot_xy_3 +
  geom_smooth(method = 'lm', color = 'blue', fill = 'blue')

# Increase of noise resulted in much lower R2 (0.14) and higher RSE (0.99) than in previous models.

# j)
lm_xy_7 %>% confint() # Original
lm_xy_9 %>% confint() # Less noisy
lm_xy_10 %>% confint() # Noisier
# The less noisy dataset is, the narrower confidence interval is. 

# Ex. 14
# a)
set.seed(1)
x1 <- runif(100)
x2 <- .5 * x1 + rnorm(100) / 10
y <- 2 + 2 * x1 + .3 * x2 + rnorm(100)
# y = 2 + 2 * x1 + 0.3 * x2 + e
# B0: 2, B1: 2, B2: 0.3

# b)
cor(x1, x2) # 0.84

tibble(x1 = x1, x2 = x2) %>%
  ggplot(., aes(x = x2, y = x1)) +
  geom_point() +
  labs(
    title = 'x1 vs x2',
    caption = 'Source: random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw() 

# c)
lm(y ~ x1 + x2) -> lm_xy_11
lm_xy_11 %>% summary()
# While intercept is relatively close to B0 (2.13 vs 2), the coefficients for x1 and x2 differ more from real coefficients (1.44 vs 2 and 1.01 vs 0.3 respectively). 
# P-value for x1 coefficient is 0.0487, however we can reject the null hypothesis. On the other hand, coefficient for x2 is not statistically significant (p-value = 0.3754).

# d)
lm(y ~ x1) -> lm_xy_12
lm_xy_12 %>% summary()
# This model only slightly differs from the previous one in terms of R2 (~0.2 in both cases) and RSE (~1.06). This time p-value for x1 coefficient is close to 0, so we can safely reject the null hypothesis.

# e)
lm(y ~ x2) -> lm_xy_13
lm_xy_13 %>% summary()
# This model has slightly lower R2 (0.18) and a little higher RSE (1.07) than previous ones. Coefficient for x2 is statistically significant.

# f)
# Not at all. In the first model the x2 variable is redundant, as it's function of x1. These variables are highly collinear, so using only one of them makes more sense.

# g)
x1 <- c(x1, .1)
x2 <- c(x2, .8)
y <- c(y, 6)
lm(y ~ x1 + x2) -> lm_xy_14
lm_xy_14 %>% summary()
# This model has a bit higher R2 (0.22) and RSE (1.08) than previous y ~ x1 + x2 model. This time x1 coefficient is not statistically significant, while x2 coefficient is.
car::outlierTest(lm_xy_14)
high_lev_fun(lm_xy_14, tibble(y, x1, x2))
# New observation is not considered as outlier, but is a high leverage point.

lm(y ~ x1) -> lm_xy_15
lm_xy_15 %>% summary()
# This model has much lower R2 (0.16) and higher RSE (1.11) than previous y ~ x1 model. Coefficient for x1 is statistically significant.
car::outlierTest(lm_xy_15)
high_lev_fun(lm_xy_15, tibble(y, x1))
# New observation is considered as outlier, but not as high leverage point.

lm(y ~ x2) -> lm_xy_16
lm_xy_16 %>% summary()
# This model has higher R2 (0.21) and similar RSE (1.07) than previous y ~ x2 model. Coefficient for x2 is statistically significant.
car::outlierTest(lm_xy_16)
high_lev_fun(lm_xy_16, tibble(y, x2))
# New observation is not considered as outlier, but is a high leverage point.

# Ex. 15.
# a)
boston_res_tib <- tibble(
  Predictor = colnames(boston_ex)[-1],
  R2 = rep(NA, ncol(boston_ex) - 1),
  RSE = rep(NA, ncol(boston_ex) - 1),
  Coef = rep(NA, ncol(boston_ex) - 1),
  Coef_p_val = rep(NA, ncol(boston_ex) - 1)
)

for (i in 2:ncol(boston_ex)) {
  lm(crim ~ boston_ex[, i], boston_ex) %>%
    summary() -> temp_model_sum
  boston_res_tib[i - 1, 2] <- temp_model_sum$r.squared %>% round(., 4)
  boston_res_tib[i - 1, 3] <- temp_model_sum$sigma %>% round(., 4)
  boston_res_tib[i - 1, 4] <- temp_model_sum$coef[, 'Estimate'][2] %>% round(., 4)
  boston_res_tib[i - 1, 5] <- temp_model_sum$coef[, 'Pr(>|t|)'][2] %>% round(., 4)
}
boston_res_tib

# Coefficients for predictors are statistically significant in all models except one with chas (determining whether tract bounds Charles River). This one model has also the lowest R2 (0.0031) and the highest RSE (8.6). It seems that the best models are those involving rad (index of accessibility to radial highways) and tax (full-value property-tax rate). This coincides with findings from Ex. 10 form chapter 2. Both predictors have positive coefficients.

boston_ex %>%
  GGally::ggpairs()

# b)
boston_ex %>%
  lm(crim ~ ., .) -> lm_boston_1
summary(lm_boston_1)
# Multiple regression model has R2 = 0.45 and RSE = 6.46. This model shows, that only zn (proportion of residential land zoned for lots over 25,000 sq.ft), dis (weighted mean of distances to five Boston employment centres), rad and medv (median value of owner-occupied homes) are statistically significant. crim has positive relationship with zn and rad, while dis and medv have negative relationship with the response.

# c)
tibble(
  simple_coef = boston_res_tib$Coef,
  multi_coef = summary(lm_boston_1)$coef[-1, 'Estimate']
) %>% ggplot(
    .,
    aes(
      x = simple_coef,
      y = multi_coef,
      label = colnames(boston_ex[, -1])
    )
  ) +
  geom_point() +
  geom_text(vjust = 1) +
  labs(
    title = 'Coefficients from simple and multiple regression models',
    x = 'Coefficients from simple regression models',
    y = 'Coefficients from multiple regression model',
    caption = 'Source: Boston dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# There are several changes in sign of coefficients. The most glaring case is nox (31.2 in simple regression, -9.96 in multiple regression), followed by rm (-2.68 vs 0.63) and ptratio (1.15 vs -0.3).

# d)
boston_ex_2 <- boston_ex[, -4] # Dummy variable cannot be used in polynomial regression
boston_poly_tib <- tibble(
  Predictor = colnames(boston_ex_2)[-1],
  R2 = rep(NA, ncol(boston_ex_2) - 1),
  RSE = rep(NA, ncol(boston_ex_2) - 1),
  Coef_1_p_val = rep(NA, ncol(boston_ex_2) - 1),
  Coef_2_p_val = rep(NA, ncol(boston_ex_2) - 1),
  Coef_3_p_val = rep(NA, ncol(boston_ex_2) - 1)
)

for (i in 2:ncol(boston_ex_2)) {
  lm(crim ~ poly(boston_ex_2[, i], 3), boston_ex_2) %>%
    summary() -> temp_model_sum
  boston_poly_tib[i - 1, 2] <- temp_model_sum$r.squared %>% round(., 4)
  boston_poly_tib[i - 1, 3] <- temp_model_sum$sigma %>% round(., 4)
  boston_poly_tib[i - 1, 4] <- temp_model_sum$coef[, 'Pr(>|t|)'][2] %>% round(., 4)
  boston_poly_tib[i - 1, 5] <- temp_model_sum$coef[, 'Pr(>|t|)'][3] %>% round(., 4)
  boston_poly_tib[i - 1, 6] <- temp_model_sum$coef[, 'Pr(>|t|)'][4] %>% round(., 4)
}
boston_poly_tib

# There is evidence for non-linear relationship in 3rd degree polynomials between crim and the following predictors: indus, nox, age, dis, ptratio and medv.
# There is also evidence for non-linear relationship in 2nd degree polynomials between crim and all other variables.












