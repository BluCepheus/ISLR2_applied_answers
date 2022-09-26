library(ISLR2)
library(tidyverse)

# Ex. 13.
weekly_ex <- Weekly

# a)
weekly_ex %>% summary()
# Lags and Today have very similar distribution. 
weekly_ex %>%
  GGally::ggpairs()
# Only Volume and Year are significantly correlated (0.842). Volume tends to be higher in later years (esp. in 2007-10)

# b)
weekly_ex %>%
  glm(Direction ~ . - Today, data = ., family = binomial) -> glm_weekly_1
glm_weekly_1 %>% summary()
# Only Lag2 coefficient is statistically significant.

# c)
glm_weekly_pred_1 <- predict.glm(glm_weekly_1, type = 'response')
glm_weekly_pred_1 <- ifelse(glm_weekly_pred_1 > .5, 'Up', 'Down')
glm_weekly_tab_1 <- table(glm_weekly_pred_1, weekly_ex$Direction)

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
conf_mat_fun(glm_weekly_tab_1)
# This logistic regression handled 'Up' observations properly (92% of them were correctly predicted), however performed poorly with 'Down' observations - less than 12% of them were correctly predicted. All in all, regression was able to correctly predict 56% of observations.

# d)
weekly_ex %>%
  filter(Year <= 2008) -> weekly_ex_train
weekly_ex %>%
  filter(Year > 2008) -> weekly_ex_test

weekly_ex_train %>%
  glm(Direction ~ Lag2, data = ., family = binomial) -> glm_weekly_2

glm_weekly_pred_2 <- predict.glm(
  glm_weekly_2,
  newdata = weekly_ex_test,
  type = 'response'
)
glm_weekly_pred_2 <- ifelse(glm_weekly_pred_2 > .5, 'Up', 'Down')
glm_weekly_tab_2 <- table(glm_weekly_pred_2, weekly_ex_test$Direction)
conf_mat_fun(glm_weekly_tab_2)
# Logistic regression still performs poorly on 'Down' observations - only 21% of them were correctly predicted. However, it maintained its performance with 'Up' observations (92% of them were correctly predicted). 62.5% of observations from test subset were correctly predicted by this model.

# e)
weekly_ex_train %>%
  MASS::lda(Direction ~ Lag2, data = .) -> lda_weekly_1
lda_weekly_pred_1 <- predict(lda_weekly_1, weekly_ex_test)
lda_weekly_tab_1 <- table(lda_weekly_pred_1$class, weekly_ex_test$Direction)
conf_mat_fun(lda_weekly_tab_1)
# LDA provided exactly the same results as logistic regression.

# f)
weekly_ex_train %>%
  MASS::qda(Direction ~ Lag2, data = .) -> qda_weekly_1
qda_weekly_pred_1 <- predict(qda_weekly_1, weekly_ex_test)
qda_weekly_tab_1 <- table(qda_weekly_pred_1$class, weekly_ex_test$Direction)
conf_mat_fun(qda_weekly_tab_1)
# QDA classified all observations as 'Up'.

# g)
set.seed(1)
knn_weekly_1 <- class::knn(
  weekly_ex_train['Lag2'],
  weekly_ex_test['Lag2'],
  weekly_ex_train$Direction,
  k = 1
)
knn_weekly_tab_1 <- table(knn_weekly_1, weekly_ex_test$Direction)
conf_mat_fun(knn_weekly_tab_1)
# KNN classified correctly 49% of 'Down' observations and 51% of 'Up' observations. This method was able to correctly classify 50% of all observations.

# h)
nb_weekly_1 <- e1071::naiveBayes(Direction ~ Lag2, weekly_ex_train)
nb_weekly_tab_1 <- table(
  predict(nb_weekly_1, weekly_ex_test),
  weekly_ex_test$Direction
)
conf_mat_fun(nb_weekly_tab_1)
# Naive Bayes classified all observations as 'Up'.

# i)
# Logistic regression and LDA gave the best results, nevertheless they performed poorly with classification of 'Down' observations.

# j)
# We can check KNN with different values of K:
set.seed(1)
knn_weekly_tib <- tibble(
  K = seq(1, 10, 1),
  correct_down = rep(NA, 10),
  correct_up = rep(NA, 10),
  correct_all = rep(NA, 10)
)
for (i in 1:10) {
  temp_model <- class::knn(
    weekly_ex_train['Lag2'],
    weekly_ex_test['Lag2'],
    weekly_ex_train$Direction,
    k = i
  )
  temp_tab <- table(temp_model, weekly_ex_test$Direction)
  (temp_tab[1, 1] / sum(temp_tab[, 1])) %>%
    round(., 2) -> knn_weekly_tib[i, 2]
  (temp_tab[2, 2] / sum(temp_tab[, 2])) %>%
    round(., 2) -> knn_weekly_tib[i, 3]
  (sum(diag(temp_tab)) / sum(temp_tab)) %>%
    round(., 2) -> knn_weekly_tib[i, 4]
}
knn_weekly_tib %>%
  gather(Prediction, Percentage, -K) %>%
  ggplot(., aes(x = K, y = Percentage, color = Prediction)) +
  geom_line() +
  labs(
    title = 'Percentage of correctly predicted observations',
    subtitle = 'Using KNN method',
    x = 'Number of nearest neigbours',
    y = 'Percentage',
    caption = 'Source: Weekly dataset, own study'
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    name = 'Prediction of:',
    values = c('blue', 'red', 'green'),
    labels = c('All', 'Down', 'Up')
  ) +
  theme_bw()

# KNN with K = 10 gives best results. 70% of 'Up' and 44% of 'Down' observations were correctly predicted, while overally KNN was right in 60% of cases.

# We can check also Direction vs Lag2 ^ 2.

weekly_ex_train %>%
  glm(Direction ~ poly(Lag2, 2), data = ., family = binomial) -> glm_weekly_3

glm_weekly_pred_3 <- predict.glm(
  glm_weekly_3,
  newdata = weekly_ex_test,
  type = 'response'
)
glm_weekly_pred_3 <- ifelse(glm_weekly_pred_3 > .5, 'Up', 'Down')
glm_weekly_tab_3 <- table(glm_weekly_pred_3, weekly_ex_test$Direction)
conf_mat_fun(glm_weekly_tab_3)
# Comparing to logistic regression with simply Lag2, it performs even worse on 'Down' observations - only 19% of them were correctly predicted. However, it increase effectiveness with 'Up' observations (93% of them were correctly predicted). 62.5% of observations from test subset were correctly predicted by this model, same as previous logistic regression.

weekly_ex_train %>%
  MASS::lda(Direction ~ poly(Lag2, 2), data = .) -> lda_weekly_2
lda_weekly_pred_2 <- predict(lda_weekly_2, weekly_ex_test)
lda_weekly_tab_2 <- table(lda_weekly_pred_2$class, weekly_ex_test$Direction)
conf_mat_fun(lda_weekly_tab_2)
# LDA gave a bit worse results: it correctly predicted only 16% of 'Down' observations, but was right about 93% of 'Up' observations, with overall correctness at 62%.

weekly_ex_train %>%
  MASS::qda(Direction ~ poly(Lag2, 2), data = .) -> qda_weekly_2
qda_weekly_pred_2 <- predict(qda_weekly_2, weekly_ex_test)
qda_weekly_tab_2 <- table(qda_weekly_pred_2$class, weekly_ex_test$Direction)
conf_mat_fun(qda_weekly_tab_2)
# QDA provided slightly better results than LDA. It still correctly predicted 16% of 'Down' observations, however it was right about 95% of 'Up'observations. 62.5% of observations from test subset were correctly predicted by this model.

nb_weekly_2 <- e1071::naiveBayes(Direction ~ Lag2 + I(Lag2^2), weekly_ex_train)
nb_weekly_tab_2 <- table(
  predict(nb_weekly_2, weekly_ex_test),
  weekly_ex_test$Direction
)
conf_mat_fun(nb_weekly_tab_2)
# Naive Bayes classified all observations as 'Up'.

# Ex. 14.
# a)
ifelse(auto_ex$mpg > median(auto_ex$mpg), 1, 0) -> auto_ex$mpg01

# b)
auto_ex[, -c(1, 9)] %>%
  GGally::ggpairs()
# mpg01 is negatively correlated with 'engine' variables (cylinders, displacement, horsepower) and vehicle weight.

# c)
set.seed(1)
auto_ex$id <- 1:nrow(auto_ex)
auto_ex %>%
  sample_frac(.8) -> auto_ex_train
anti_join(auto_ex, auto_ex_train, by = 'id') -> auto_ex_test

# d)
auto_ex_train %>%
  MASS::lda(
    mpg01 ~ cylinders + displacement + horsepower + weight,
    data = .
  ) -> lda_auto_1
lda_auto_pred_1 <- predict(lda_auto_1, auto_ex_test)
lda_auto_tab_1 <- table(lda_auto_pred_1$class, auto_ex_test$mpg01)
conf_mat_fun(lda_auto_tab_1)

# LDA was able to correctly predict all 'mpg above median' observations and 83% of 'mpg below median' observations. Test error is slightly less than 9%.

# e)
auto_ex_train %>%
  MASS::qda(
    mpg01 ~ cylinders + displacement + horsepower + weight,
    data = .
  ) -> qda_auto_1
qda_auto_pred_1 <- predict(qda_auto_1, auto_ex_test)
qda_auto_tab_1 <- table(qda_auto_pred_1$class, auto_ex_test$mpg01)
conf_mat_fun(qda_auto_tab_1)

# QDA provided slightly different results, correctly predicting 95% of above median and 88% of below median observations, keeping classification error at the same level.

# f)
auto_ex_train %>%
  glm(
    mpg01 ~ cylinders + displacement + horsepower + weight,
    data = .,
    family = binomial
  ) -> glm_auto_1

glm_auto_pred_1 <- predict.glm(
  glm_auto_1,
  newdata = auto_ex_test,
  type = 'response'
)
glm_auto_pred_1 <- ifelse(glm_auto_pred_1 > .5, 1, 0)
glm_auto_tab_1 <- table(glm_auto_pred_1, auto_ex_test$mpg01)
conf_mat_fun(glm_auto_tab_1)

# Logistic regression was able to correctly predict 97% of 'mpg above median' and 90% of 'mpg below median' observations. Classification error is 6.4%.

# g)
auto_ex_train %>%
  e1071::naiveBayes(
    mpg01 ~ cylinders + displacement + horsepower + weight,
    data = .
  ) -> nb_auto_1
nb_auto_tab_1 <- table(
  predict(nb_auto_1, auto_ex_test),
  auto_ex_test$mpg01
)
conf_mat_fun(nb_auto_tab_1)

# Naive Bayes method was able to correctly predict 92% of 'mpg above median' and 89% of 'mpg below median' observations. Classification error is 7.7%.

# h)
set.seed(1)
knn_auto_tib <- tibble(
  K = seq(1, 10, 1),
  correct_0 = rep(NA, 10),
  correct_1 = rep(NA, 10),
  correct_all = rep(NA, 10)
)
for (i in 1:10) {
  temp_model <- class::knn(
    auto_ex_train[c('cylinders', 'displacement', 'horsepower', 'weight')],
    auto_ex_test[c('cylinders', 'displacement', 'horsepower', 'weight')],
    auto_ex_train$mpg01,
    k = i
  )
  temp_tab <- table(temp_model, auto_ex_test$mpg01)
  (temp_tab[1, 1] / sum(temp_tab[, 1])) %>%
    round(., 2) -> knn_auto_tib[i, 2]
  (temp_tab[2, 2] / sum(temp_tab[, 2])) %>%
    round(., 2) -> knn_auto_tib[i, 3]
  (sum(diag(temp_tab)) / sum(temp_tab)) %>%
    round(., 2) -> knn_auto_tib[i, 4]
}
knn_auto_tib %>%
  gather(Prediction, Percentage, -K) %>%
  ggplot(., aes(x = K, y = Percentage, color = Prediction)) +
  geom_line() +
  labs(
    title = 'Percentage of correctly predicted observations',
    subtitle = 'Using KNN method',
    x = 'Number of nearest neigbours',
    y = 'Percentage',
    caption = 'Source: Auto dataset, own study'
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    name = 'Prediction of:',
    values = c('blue', 'red', 'green'),
    labels = c('All', 0, 1)
  ) +
  theme_bw()

# KNN provided best results for K = 6. Method was able to correctly predict 95% of 'mpg above median' and 90% of 'mpg below median' observations. Classification error is 8%.

# Ex. 15.
# a)
Power <- function() {
  print(2 ^ 3)
}
Power()

# b)
Power2 <- function(x, a) {
  print(x ^ a)
}
Power2(3, 8)

# c)
Power2(10, 3)
Power2(8, 17)
Power2(131, 3)

# d)
Power3 <- function(x, a) {
  return(x ^ a)
}

# e)
tibble(
  x = seq(1, 10, 1),
  y = Power3(x, 2)
) %>%
  ggplot(., aes(x = x, y = y)) +
    geom_point() +
    labs(
      title = 'y vs x',
      subtitle = 'y = x ^ 2',
      caption = 'Source: own study'
    ) +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    scale_y_continuous(labels = scales::number) +
    theme_bw()

# f)
PlotPower <- function(x, a) {
  tibble(
    x = x,
    y = Power3(x, a)
  ) %>%
    ggplot(., aes(x = x, y = y)) +
    geom_point() +
    labs(
      title = 'y vs x',
      subtitle = paste('y = x ^', a),
      caption = 'Source: own study'
    ) +
    scale_x_continuous(breaks = x) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> power_plot
  power_plot %>% print()
}
PlotPower(1:10, 3)

# Ex. 16.
ifelse(boston_ex$crim > median(boston_ex$crim), 1, 0) -> boston_ex$crim01

boston_ex[, -1] %>%
  GGally::ggpairs()
# crim01 is positively correlated with indus, nox, age, rad and tax, as well as negatively with dis.

set.seed(1)
boston_ex$id <- 1:nrow(boston_ex)
boston_ex %>%
  sample_frac(.8) -> boston_ex_train
anti_join(boston_ex, boston_ex_train, by = 'id') -> boston_ex_test

boston_ex_train %>%
  MASS::lda(
    crim01 ~ indus + nox + age + rad + tax + dis,
    data = .
  ) -> lda_boston_1
lda_boston_pred_1 <- predict(lda_boston_1, boston_ex_test)
lda_boston_tab_1 <- table(lda_boston_pred_1$class, boston_ex_test$crim01)
conf_mat_fun(lda_boston_tab_1)

# LDA was able to correctly predict 74% 'above median' observations and 98% of 'below median' observations. Test error is slightly less than 14%.

boston_ex_train %>%
  MASS::qda(
    crim01 ~ indus + nox + age + rad + tax + dis,
    data = .
  ) -> qda_boston_1
qda_boston_pred_1 <- predict(qda_boston_1, boston_ex_test)
qda_boston_tab_1 <- table(qda_boston_pred_1$class, boston_ex_test$crim01)
conf_mat_fun(qda_boston_tab_1)

# QDA provided slightly different results, correctly predicting 86% of above median and 94% of below median observations, with classification error 10%.

# f)
boston_ex_train %>%
  glm(
    crim01 ~ indus + nox + age + rad + tax + dis,
    data = .,
    family = binomial
  ) -> glm_boston_1

glm_boston_pred_1 <- predict.glm(
  glm_boston_1,
  newdata = boston_ex_test,
  type = 'response'
)
glm_boston_pred_1 <- ifelse(glm_boston_pred_1 > .5, 1, 0)
glm_boston_tab_1 <- table(glm_boston_pred_1, boston_ex_test$crim01)
conf_mat_fun(glm_boston_tab_1)

# Logistic regression was able to correctly predict 90% of 'above median' and 76% of 'below median' observations. Classification error is 17%.

# g)
boston_ex_train %>%
  e1071::naiveBayes(
    crim01 ~ indus + nox + age + rad + tax + dis,
    data = .
  ) -> nb_boston_1
nb_boston_tab_1 <- table(
  predict(nb_boston_1, boston_ex_test),
  boston_ex_test$crim01
)
conf_mat_fun(nb_boston_tab_1)

# Naive Bayes method was able to correctly predict 74% of 'above median' and 94% of 'mpg below median' observations. Classification error is 16%.

# h)
set.seed(1)
knn_boston_tib <- tibble(
  K = seq(1, 10, 1),
  correct_0 = rep(NA, 10),
  correct_1 = rep(NA, 10),
  correct_all = rep(NA, 10)
)
for (i in 1:10) {
  temp_model <- class::knn(
    boston_ex_train[c('indus', 'nox', 'age', 'rad', 'tax', 'dis')],
    boston_ex_test[c('indus', 'nox', 'age', 'rad', 'tax', 'dis')],
    boston_ex_train$crim01,
    k = i
  )
  temp_tab <- table(temp_model, boston_ex_test$crim01)
  (temp_tab[1, 1] / sum(temp_tab[, 1])) %>%
    round(., 2) -> knn_boston_tib[i, 2]
  (temp_tab[2, 2] / sum(temp_tab[, 2])) %>%
    round(., 2) -> knn_boston_tib[i, 3]
  (sum(diag(temp_tab)) / sum(temp_tab)) %>%
    round(., 2) -> knn_boston_tib[i, 4]
}
knn_boston_tib %>%
  gather(Prediction, Percentage, -K) %>%
  ggplot(., aes(x = K, y = Percentage, color = Prediction)) +
  geom_line() +
  labs(
    title = 'Percentage of correctly predicted observations',
    subtitle = 'Using KNN method',
    x = 'Number of nearest neigbours',
    y = 'Percentage',
    caption = 'Source: Boston dataset, own study'
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    name = 'Prediction of:',
    values = c('blue', 'red', 'green'),
    labels = c('All', 0, 1)
  ) +
  theme_bw()

# KNN provided best results for K = 1. Method was able to correctly predict 92% of 'above median' and 91% of 'below median' observations. Classification error is 9%.







