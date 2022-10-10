library(ISLR2)
library(tidyverse)

# Function for printing prediction table results. 'conf_matrix' is prediction table.

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

# Ex. 4.
xy_tib <- tibble(
  x1 = rnorm(100),
  x2 = rnorm(100),
  y = factor(c(rep(-1, 20), rep(1, 80)))
)
xy_tib$x1[1:20] <- xy_tib$x1[1:20] + 1.5
xy_tib$x1[21:100] <- xy_tib$x1[21:100] - 1.5

xy_tib %>%
  ggplot(., aes(
    x = x1,
    y = x2,
    color = y,
    fill = y
  )) +
  geom_point() +
  labs(
    title = 'x2 vs x1',
    subtitle = 'For two-class y',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

1:nrow(xy_tib) %>%
  data.frame() %>%
  sample_frac(.8) %>%
  unlist() ->
  train_ind
xy_tib[train_ind, ] -> xy_tib_train
xy_tib[-train_ind, ] -> xy_tib_test

# Support vector classifier:
xy_tib_train %>%
  e1071::tune(
    e1071::svm,
    y ~ .,
    data = .,
    kernel = 'linear',
    ranges = list(cost = 1:10)
  ) -> svc_xy


# Training error rates
plot(svc_xy$best.model, xy_tib_train)
table(
  Predicted = predict(svc_xy$best.model, xy_tib_train),
  True = xy_tib_train$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 93.75 %"
# "Classification error (% of incorrectly predicted observations): 6.25 %"
# "Sensitivity  (% of correctly predicted 1 observations): 98.44 %"
# "Specificity  (% of correctly predicted -1 observations): 75 %"

# Test error rates
plot(svc_xy$best.model, xy_tib_test)
table(
  Predicted = predict(svc_xy$best.model, xy_tib_test),
  True = xy_tib_test$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 100 %"
# "Classification error (% of incorrectly predicted observations): 0 %"
# "Sensitivity  (% of correctly predicted 1 observations): 100 %"
# "Specificity  (% of correctly predicted -1 observations): 100 %"

# Support vector machine - radial:

xy_tib_train %>%
  e1071::tune(
    e1071::svm,
    y ~ .,
    data = .,
    kernel = 'radial',
    ranges = list(
      cost = 1:10,
      gamma = 1:10
    )
  ) -> svm_radial_xy

# Training error rates
plot(svm_radial_xy$best.model, xy_tib_train)
table(
  Predicted = predict(svm_radial_xy$best.model, xy_tib_train),
  True = xy_tib_train$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 97.5 %"
# "Classification error (% of incorrectly predicted observations): 2.5 %"
# "Sensitivity  (% of correctly predicted 1 observations): 100 %"
# "Specificity  (% of correctly predicted -1 observations): 87.5 %"

# Test error rates
plot(svm_radial_xy$best.model, xy_tib_test)
table(
  Predicted = predict(svm_radial_xy$best.model, xy_tib_test),
  True = xy_tib_test$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 90 %"
# "Classification error (% of incorrectly predicted observations): 10 %"
# "Sensitivity  (% of correctly predicted 1 observations): 87.5 %"
# "Specificity  (% of correctly predicted -1 observations): 100 %"

# Support vector machine - polynomial:

xy_tib_train %>%
  e1071::tune(
    e1071::svm,
    y ~ .,
    data = .,
    kernel = 'polynomial',
    ranges = list(
      cost = 1:10,
      gamma = 1:10,
      degree = 2
    )
  ) -> svm_poly_xy

# Training error rates
plot(svm_poly_xy$best.model, xy_tib_train)
table(
  Predicted = predict(svm_poly_xy$best.model, xy_tib_train),
  True = xy_tib_train$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 85 %"
# "Classification error (% of incorrectly predicted observations): 15 %"
# "Sensitivity  (% of correctly predicted 1 observations): 100 %"
# "Specificity  (% of correctly predicted -1 observations): 25 %"

# Test error rates
plot(svm_poly_xy$best.model, xy_tib_test)
table(
  Predicted = predict(svm_poly_xy$best.model, xy_tib_test),
  True = xy_tib_test$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 80 %"
# "Classification error (% of incorrectly predicted observations): 20 %"
# "Sensitivity  (% of correctly predicted 1 observations): 100 %"
# "Specificity  (% of correctly predicted -1 observations): 0 %"

# SVM with radial kernel is better than SVC on training dataset (prediction accuracy 97.5% vs 93.75%). However, SVC is perfect on test dataset. SVM with polynomial kernel is the worst in both cases.

# Ex. 5.
# a)
xy_tib <- tibble(
  x1 = runif(500) - 0.5,
  x2 = runif(500) - 0.5,
  y = factor(1 * (x1 ^ 2 - x2 ^ 2 > 0))
)

# b)
xy_tib %>%
  ggplot(., aes(
    x = x1,
    y = x2,
    color = y,
    fill = y
  )) +
  geom_point() +
  labs(
    title = 'x2 vs x1',
    subtitle = 'For two-class y',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# c)
1:nrow(xy_tib) %>%
  data.frame() %>%
  sample_frac(.8) %>%
  unlist() ->
  train_ind
xy_tib[train_ind, ] -> xy_tib_train
xy_tib[-train_ind, ] -> xy_tib_test

xy_tib_train %>%
  glm(y ~ ., data = ., family = binomial) ->
  glm_xy
glm_xy %>% summary()

# d)
tibble(
  Predicted = ifelse(
    predict(glm_xy, xy_tib_train, type = 'response') > .5,
    1, 0) %>% factor(),
  x1 = xy_tib_train$x1,
  x2 = xy_tib_train$x2
) %>%
  ggplot(., aes(
    x = x1,
    y = x2,
    color = Predicted,
    fill = Predicted
  )) +
  geom_point() +
  labs(
    title = 'x2 vs x1',
    subtitle = 'For two-class y, using logistic regression y ~ x1 + x2',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Training error rates
table(
  Predicted = ifelse(
    predict(glm_xy, xy_tib_train, type = 'response') > .5,
    1, 0
  ),
  True = xy_tib_train$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 40.75 %"
# "Classification error (% of incorrectly predicted observations): 59.25 %"
# "Sensitivity  (% of correctly predicted 1 observations): 60.19 %"
# "Specificity  (% of correctly predicted 0 observations): 20.1 %"

# e)
xy_tib_train %>%
  glm(y ~ poly(x1, 2) + I(x1 * x2) + exp(x2), data = ., family = binomial) ->
  glm_xy_2
glm_xy_2 %>% summary()

# f)
tibble(
  Predicted = ifelse(
    predict(glm_xy_2, xy_tib_train, type = 'response') > .5,
    1, 0) %>% factor(),
  x1 = xy_tib_train$x1,
  x2 = xy_tib_train$x2
) %>%
  ggplot(., aes(
    x = x1,
    y = x2,
    color = Predicted,
    fill = Predicted
  )) +
  geom_point() +
  labs(
    title = 'x2 vs x1',
    subtitle = 'For two-class y, using logistic regression y = x1 ^ 2 + x1 * x2',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Training error rates
table(
  Predicted = ifelse(
    predict(glm_xy_2, xy_tib_train, type = 'response') > .5,
    1, 0
  ),
  True = xy_tib_train$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 77.25 %"
# "Classification error (% of incorrectly predicted observations): 22.75 %"
# "Sensitivity  (% of correctly predicted 1 observations): 73.79 %"
# "Specificity  (% of correctly predicted 0 observations): 80.93 %"

# g)
xy_tib_train %>%
  e1071::tune(
    e1071::svm,
    y ~ .,
    data = .,
    kernel = 'linear',
    ranges = list(cost = 1:10)
  ) -> svc_xy

tibble(
  Predicted = predict(svc_xy$best.model, xy_tib_train, type = 'response') %>% 
    factor(),
  x1 = xy_tib_train$x1,
  x2 = xy_tib_train$x2
) %>%
  ggplot(., aes(
    x = x1,
    y = x2,
    color = Predicted,
    fill = Predicted
  )) +
  geom_point() +
  labs(
    title = 'x2 vs x1',
    subtitle = 'For two-class y, using SVC',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Training error rates
plot(svc_xy$best.model, xy_tib_train)
table(
  Predicted = predict(svc_xy$best.model, xy_tib_train),
  True = xy_tib_train$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 51.5 %"
# "Classification error (% of incorrectly predicted observations): 48.5 %"
# "Sensitivity  (% of correctly predicted 1 observations): 100 %"
# "Specificity  (% of correctly predicted 0 observations): 0 %"

# h)
xy_tib_train %>%
  e1071::tune(
    e1071::svm,
    y ~ .,
    data = .,
    kernel = 'polynomial',
    ranges = list(
      cost = 1:10,
      gamma = 1:10,
      degree = 2
    )
  ) -> svm_poly_xy

tibble(
  Predicted = predict(svm_poly_xy$best.model, xy_tib_train, type = 'response') %>% 
    factor(),
  x1 = xy_tib_train$x1,
  x2 = xy_tib_train$x2
) %>%
  ggplot(., aes(
    x = x1,
    y = x2,
    color = Predicted,
    fill = Predicted
  )) +
  geom_point() +
  labs(
    title = 'x2 vs x1',
    subtitle = 'For two-class y, using SVC',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Training error rates
plot(svm_poly_xy$best.model, xy_tib_train)
table(
  Predicted = predict(svm_poly_xy$best.model, xy_tib_train),
  True = xy_tib_train$y
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 92.75 %"
# "Classification error (% of incorrectly predicted observations): 7.25 %"
# "Sensitivity  (% of correctly predicted 1 observations): 91.26 %"
# "Specificity  (% of correctly predicted 0 observations): 94.33 %"

# i)
# SVM with polynomial kernel clearly provides the best fit. Logistic regression y = x1 ^ 2 + x1 * x2 gave worse result, though still better than SVC.

# Ex. 6.
# a)
set.seed(1)
xy_tib <- tibble(
  x1 = runif(100, -1, 1),
  x2 = runif(100, -1, 1),
  y = ifelse(x1 > x2, 1, -1) %>% factor()
)

xy_tib %>%
  ggplot(., aes(
    x = x1,
    y = x2,
    color = y,
    fill = y
  )) +
  geom_point() +
  labs(
    title = 'x2 vs x1',
    subtitle = 'For two-class y',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# b)
1:nrow(xy_tib) %>%
  data.frame() %>%
  sample_frac(.8) %>%
  unlist() ->
  train_ind
xy_tib[train_ind, ] -> xy_tib_train
xy_tib[-train_ind, ] -> xy_tib_test

xy_tib_train %>%
  e1071::tune(
    e1071::svm,
    y ~ .,
    data = .,
    kernel = 'linear',
    ranges = list(cost = 1:100)
  ) -> svc_xy

svc_xy$performances %>% 
  ggplot(., aes(x = cost, y = error)) +
  geom_line() +
  labs(
    title = 'CV errors vs cost',
    subtitle = 'Using SVC',
    x = 'Cost',
    y = 'CV error',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = seq(0, 100, 5)
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

xy_class_tib <- tibble(
  Cost = 1:100,
  Class_err = rep(NA, 100)
)

for (i in 1:100) {
  xy_tib_train %>%
    e1071::svm(
      y ~ .,
      data = .,
      kernel = 'linear',
      cost = i,
      scale = F
    ) -> temp_model
  
  table(
    Predicted = predict(temp_model, xy_tib_train),
    True = xy_tib_train$y
  ) -> conf_matrix
  
  (100 - (sum(diag(conf_matrix)) / sum(conf_matrix) * 100)) %>%
    round(., 2) ->
    xy_class_tib[i, 2]
}

xy_class_tib %>% 
  ggplot(., aes(x = Cost, y = Class_err)) +
  geom_line() +
  labs(
    title = 'Classification errors vs cost',
    subtitle = 'Using SVC, training set',
    x = 'Cost',
    y = 'Classification error in %',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = seq(0, 100, 5)
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# CV errors are equal zero for cost higher than 54. Classification errors are equal zero for cost in range 1-5, 33-45 and higher than 85.

# c)

for (i in 1:100) {
  xy_tib_train %>%
    e1071::svm(
      y ~ .,
      data = .,
      kernel = 'linear',
      cost = i,
      scale = F
    ) -> temp_model
  
  table(
    Predicted = predict(temp_model, xy_tib_test),
    True = xy_tib_test$y
  ) -> conf_matrix
  
  (100 - (sum(diag(conf_matrix)) / sum(conf_matrix) * 100)) %>%
    round(., 2) ->
    xy_class_tib[i, 2]
}

xy_class_tib %>% 
  ggplot(., aes(x = Cost, y = Class_err)) +
  geom_line() +
  labs(
    title = 'Classification errors vs cost',
    subtitle = 'Using SVC, test set',
    x = 'Cost',
    y = 'Classification error in %',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = seq(0, 100, 5)
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Classification errors for test set are equal zero for cost equal 2 and in range 36-41.

# Ex. 7.
auto_ex <- Auto

# a)
auto_ex$high_mpg <- ifelse(auto_ex$mpg > median(auto_ex$mpg), 1, 0)

# b)
auto_ex %>%
  e1071::tune(
    e1071::svm,
    high_mpg ~ . -mpg,
    data = .,
    kernel = 'linear',
    ranges = list(cost = 1:10)
  ) -> svc_auto_ex

svc_auto_ex$performances %>% 
  ggplot(., aes(x = cost, y = error)) +
  geom_line() +
  labs(
    title = 'CV errors vs cost',
    subtitle = 'Using SVC',
    x = 'Cost',
    y = 'CV error',
    caption = 'Auto dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = seq(1, 10, 1)
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# CV error clearly rises with higher cost value.

# c)
auto_ex %>%
  e1071::tune(
    e1071::svm,
    high_mpg ~ . -mpg,
    data = .,
    kernel = 'polynomial',
    ranges = list(
      cost = 1:10,
      gamma = 1:10,
      degree = 2
    )
  ) -> svm_poly_auto_ex

svm_poly_auto_ex$performances %>% 
  plotly::plot_ly(., x = ~cost, y = ~error, z = ~gamma) %>%
  plotly::add_markers(size = 1.5)

# The lowest CV error is obtained using cost = 1 and gamma = 1

auto_ex %>%
  e1071::tune(
    e1071::svm,
    high_mpg ~ . -mpg,
    data = .,
    kernel = 'radial',
    ranges = list(
      cost = 1:10,
      gamma = 1:10
    )
  ) -> svm_radial_auto_ex

svm_radial_auto_ex$performances %>% 
  plotly::plot_ly(., x = ~cost, y = ~error, z = ~gamma) %>%
  plotly::add_markers(size = 1.5)

# The lowest CV error is obtained using cost = 1 and gamma = 1

# Ex. 8.
oj_ex <- OJ

# a)
set.seed(1)
1:nrow(oj_ex) %>%
  data.frame() %>%
  sample_n(800) %>%
  unlist() ->
  train_ind
oj_ex[train_ind, ] -> oj_ex_train
oj_ex[-train_ind, ] -> oj_ex_test

# b)
oj_ex_train %>%
  e1071::svm(
    Purchase ~ .,
    data = .,
    kernel = 'linear',
    cost = .01,
    scale = F
  ) -> svc_oj_ex
svc_oj_ex %>% summary()
# There is 615 support vectors, 309 in one class, 306 in the other.

# c)
# Training error rates
table(
  Predicted = predict(svc_oj_ex, oj_ex_train),
  True = oj_ex_train$Purchase
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 78.75 %"
# "Classification error (% of incorrectly predicted observations): 21.25 %"
# "Sensitivity  (% of correctly predicted 1 observations): 66.67 %"
# "Specificity  (% of correctly predicted -1 observations): 86.6 %"

# Test error rates
table(
  Predicted = predict(svc_oj_ex, oj_ex_test),
  True = oj_ex_test$Purchase
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 76.67 %"
# "Classification error (% of incorrectly predicted observations): 23.33 %"
# "Sensitivity  (% of correctly predicted 1 observations): 57.84 %"
# "Specificity  (% of correctly predicted -1 observations): 88.1 %"

# d)
oj_ex_train %>%
  e1071::tune(
    e1071::svm,
    Purchase ~ .,
    data = .,
    kernel = 'linear',
    ranges = list(cost = c(
      seq(.01, .09, .01),
      seq(.1, .9, .1),
      1:10
    ))
  ) -> svc_oj_ex_tune
svc_oj_ex_tune$best.model # Optimal cost = 7

# e)
# Training error rates
table(
  Predicted = predict(svc_oj_ex_tune$best.model, oj_ex_train),
  True = oj_ex_train$Purchase
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 83.75 %"
# "Classification error (% of incorrectly predicted observations): 16.25 %"
# "Sensitivity  (% of correctly predicted 1 observations): 78.41 %"
# "Specificity  (% of correctly predicted -1 observations): 87.22 %"

# Test error rates
table(
  Predicted = predict(svc_oj_ex_tune$best.model, oj_ex_test),
  True = oj_ex_test$Purchase
) %>%
  conf_mat_fun()
# "Prediction accuracy (% of correctly predicted observations): 84.81 %"
# "Classification error (% of incorrectly predicted observations): 15.19 %"
# "Sensitivity  (% of correctly predicted 1 observations): 71.57 %"
# "Specificity  (% of correctly predicted -1 observations): 92.86 %"

# f)
oj_ex_train %>%
  e1071::svm(
    Purchase ~ .,
    data = .,
    kernel = 'radial',
    cost = .01,
    scale = F
  ) -> svm_radial_oj_ex
svm_radial_oj_ex %>% summary()
# There is 642 support vectors, 327 in one class, 315 in the other.

# Training error rates
table(
  Predicted = predict(svm_radial_oj_ex, oj_ex_train),
  True = oj_ex_train$Purchase
) %>%
  conf_mat_fun()

# Test error rates
table(
  Predicted = predict(svm_radial_oj_ex, oj_ex_test),
  True = oj_ex_test$Purchase
) %>%
  conf_mat_fun()

oj_ex_train %>%
  e1071::tune(
    e1071::svm,
    Purchase ~ .,
    data = .,
    kernel = 'radial',
    ranges = list(cost = c(
      seq(.01, .09, .01),
      seq(.1, .9, .1),
      1:10
    ))
  ) -> svm_radial_oj_ex_tune
svm_radial_oj_ex_tune$best.model # Optimal cost = 0.7

# Training error rates
table(
  Predicted = predict(svm_radial_oj_ex_tune$best.model, oj_ex_train),
  True = oj_ex_train$Purchase
) %>%
  conf_mat_fun()

# Test error rates
table(
  Predicted = predict(svm_radial_oj_ex_tune$best.model, oj_ex_test),
  True = oj_ex_test$Purchase
) %>%
  conf_mat_fun()

# g)
oj_ex_train %>%
  e1071::svm(
    Purchase ~ .,
    data = .,
    kernel = 'polynomial',
    cost = .01,
    degree = 2,
    scale = F
  ) -> svm_poly_oj_ex
svm_poly_oj_ex %>% summary()
# There is 333 support vectors, 166 in one class, 167 in the other.

# Training error rates
table(
  Predicted = predict(svm_poly_oj_ex, oj_ex_train),
  True = oj_ex_train$Purchase
) %>%
  conf_mat_fun()

# Test error rates
table(
  Predicted = predict(svm_poly_oj_ex, oj_ex_test),
  True = oj_ex_test$Purchase
) %>%
  conf_mat_fun()

oj_ex_train %>%
  e1071::tune(
    e1071::svm,
    Purchase ~ .,
    data = .,
    kernel = 'radial',
    degree = 2,
    ranges = list(cost = c(
      seq(.01, .09, .01),
      seq(.1, .9, .1),
      1:10
    ))
  ) -> svm_poly_oj_ex_tune
svm_poly_oj_ex_tune$best.model # Optimal cost = 0.6

# Training error rates
table(
  Predicted = predict(svm_poly_oj_ex_tune$best.model, oj_ex_train),
  True = oj_ex_train$Purchase
) %>%
  conf_mat_fun()

# Test error rates
table(
  Predicted = predict(svm_poly_oj_ex_tune$best.model, oj_ex_test),
  True = oj_ex_test$Purchase
) %>%
  conf_mat_fun()

# h)
# Summary of classification errors:
# SVC:
# Training - 21.25 %
# Test - 23.33 %
# Tuned SVC:
# Training - 16.25 %
# Test - 15.19 %

# Radial SVM:
# Training - 39.38 %
# Test - 37.78 %
# Tuned radial SVM:
# Training - 15 %
# Test - 18.52 %

# Polynomial SVM:
# Training - 16.5 %
# Test - 15.93 %
# Tuned polynomial SVM:
# Training - 14.88 %
# Test - 17.78 %

# The lowest classification for training set - Tuned polynomial SVM (14.88%)
# The lowest classification for test set - Tuned SVC (15.19%)
