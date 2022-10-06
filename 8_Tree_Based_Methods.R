library(ISLR2)
library(tidyverse)
library(tree)

# Ex. 7.
boston_ex <- Boston
set.seed(1)
1:nrow(boston_ex) %>%
  data.frame() %>%
  sample_frac(.8) %>%
  unlist() ->
  train_ind
boston_ex[train_ind, ] -> boston_ex_train
boston_ex[-train_ind, ] -> boston_ex_test

rf_boston_tib <- tibble(
  MTRY = rep(c(12, 6, sqrt(12)), each = 100),
  NTREE = rep(1:100, 3),
  MSE = NA
)
n = 1
for (i in c(12, 6, sqrt(12))) {
  for (j in 1:100) {
    boston_ex_train %>%
      randomForestSRC::rfsrc(
        medv ~ ., .,
        mtry = i,
        ntree = j,
        importance = T
      ) -> temp_model
    
    rf_boston_tib[n, 3] <- mean((
      boston_ex_test$medv -
        predict(
          temp_model,
          boston_ex_test
        )$predicted
    ) ^ 2)
    n = n + 1
  }
}

rf_boston_tib %>%
  mutate_at(vars(MTRY), as.factor) %>%
  ggplot(., aes(
    x = NTREE,
    y = MSE,
    color = MTRY
  )) +
  geom_line() +
  labs(
    title = 'Test MSE vs number of trees',
    subtitle = 'For different values of variables',
    x = 'Number of trees',
    y = 'Test MSE',
    caption = 'Boston dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = seq(0, 100, 5)
  ) +
  scale_y_continuous(
    labels = scales::number,
    breaks = 1:25
  ) +
  scale_color_manual(
    name = 'Number of variables',
    values = c('blue', 'red', 'green'),
    labels = c('m = sqrt(p)', 'm = p / 2', 'm = p')
  ) +
  theme_bw()

# The lowest test MSE values are obtained for m = 12. Increasing number of trees over 20 does not improve test MSE significantly, as it fluctuates between 4.5 and 6. The highest test MSE values are obtained for m = sqrt(12).

# Ex. 8.
carseats_ex <- Carseats

# a)
set.seed(1)
1:nrow(carseats_ex) %>%
  data.frame() %>%
  sample_frac(.8) %>%
  unlist() ->
  train_ind
carseats_ex[train_ind, ] -> carseats_ex_train
carseats_ex[-train_ind, ] -> carseats_ex_test

# b)
carseats_ex_train %>%
  tree(
    Sales ~ .,
    data = .
  ) -> tree_carseats_1
tree_carseats_1 %>% summary()
tree_carseats_1 %>% plot()
tree_carseats_1 %>% text()
tree_carseats_1 

# Significant predictors are ShelveLoc, Price, Age, Income, CompPrice and Advertising.
# The most important is shelving location, its good quality translate into higher sales.
# For car seats in good locations the next important variable is price (lower than 109.5$), then advertising budget (higher than 500$) and community income level (higher than 35k$).

mean((
  carseats_ex_test$Sales - 
    predict(tree_carseats_1, carseats_ex_test)
) ^ 2)
# Test MSE = 4.94

# c)
cv.tree(tree_carseats_1) -> cv_tree_carseats_1

cv_tib_carseats_1 <- tibble(
  CV_error = cv_tree_carseats_1$dev,
  Size = cv_tree_carseats_1$size
)
cv_tib_carseats_1 %>%
  ggplot(., aes(x = Size, y = CV_error)) +
  geom_point() +
  labs(
    title = 'CV error vs number of terminal nodes',
    x = 'Number of terminal nodes',
    y = 'CV error',
    caption = 'Sales of Child Car Seats dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = 1:16
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# CV error rise a bit when number of terminal nodes is higher than 9.

prune.tree(tree_carseats_1, best = 9) -> ptree_carseats_1

ptree_carseats_1 %>% summary()
ptree_carseats_1 %>% plot()
ptree_carseats_1 %>% text()
ptree_carseats_1 

# Significant predictors are ShelveLoc, Price, Age, Income and CompPrice.
# The most important is shelving location, its good quality translate into higher sales.
# For car seats in good locations the next important variable is price (lower than 109.5$).

mean((
  carseats_ex_test$Sales - 
    predict(ptree_carseats_1, carseats_ex_test)
) ^ 2)
# Test MSE = 5.24. It's higher than MSE for previous tree.

# We could check what's test MSE for different sizes:
carseats_mse_tib <- tibble(
  Size = 2:16,
  MSE = rep(NA, 15)
)

for (i in 2:16) {
  prune.tree(tree_carseats_1, best = i) ->
    temp_tree
  
  mean((
    carseats_ex_test$Sales - 
      predict(temp_tree, carseats_ex_test)
  ) ^ 2) -> carseats_mse_tib[i - 1, 2]
}

carseats_mse_tib %>%
  ggplot(., aes(x = Size, y = MSE)) +
  geom_point() +
  labs(
    title = 'Test MSE vs number of terminal nodes',
    x = 'Number of terminal nodes',
    y = 'Test MSE',
    caption = 'Sales of Child Car Seats dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = 2:16
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Test MSE is the lowest for tree of size 14.

# d)
set.seed(1)
carseats_ex_train %>%
  randomForestSRC::rfsrc(Sales ~ ., ., mtry = 16, importance = T) ->
  bag_carseats_1
bag_carseats_1 %>% plot()

# Most important variables are Price and ShelveLoc.

mean((
  carseats_ex_test$Sales - 
    predict(bag_carseats_1, carseats_ex_test)$predicted
) ^ 2)
# Test MSE is 3.09, so it's significantly smaller than in regression trees.

# e)
set.seed(1)
carseats_ex_train %>%
  randomForestSRC::rfsrc(Sales ~ ., ., importance = T) ->
  rf_carseats_1
rf_carseats_1 %>% plot()

# Most important variables are Price and ShelveLoc.

mean((
  carseats_ex_test$Sales - 
    predict(rf_carseats_1, carseats_ex_test)$predicted
) ^ 2)
# Test MSE is 3.39, so it's a bit higher than in bagging.

# Let's look at different values of m:

rf_carseats_tib <- tibble(
  MTRY = rep(c(16, 8, 6, 4), each = 500),
  NTREE = rep(1:500, 4),
  MSE = NA
)
n = 1
for (i in c(16, 8, 6, 4)) {
  for (j in 1:500) {
    carseats_ex_train %>%
      randomForestSRC::rfsrc(
        Sales ~ ., .,
        mtry = i,
        ntree = j,
        importance = T
      ) -> temp_model
    
    rf_carseats_tib[n, 3] <- mean((
      carseats_ex_test$Sales -
        predict(
          temp_model,
          carseats_ex_test
        )$predicted
    ) ^ 2)
    n = n + 1
  }
}

rf_carseats_tib %>%
  mutate_at(vars(MTRY), as.factor) %>%
  ggplot(., aes(
    x = NTREE,
    y = MSE,
    color = MTRY
  )) +
  geom_line() +
  labs(
    title = 'Test MSE vs number of trees',
    subtitle = 'For different values of variables',
    x = 'Number of trees',
    y = 'Test MSE',
    caption = 'Sales of Child Car Seats dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = seq(0, 500, 25)
  ) +
  scale_y_continuous(labels = scales::number) +
  scale_color_manual(
    name = 'Number of variables',
    values = c('blue', 'red', 'green', 'lightblue'),
    labels = c('m = sqrt(p)', 'm = p / 3', 'm = p / 2', 'm = p')
  ) +
  theme_bw()

# The highest test MSE values are obtained for m = 4. Increasing number of trees over 100 does not improve test MSE significantly, as it fluctuates around 3 for m in [6, 8, 16].

# f)
set.seed(1)
BART::gbart(
  carseats_ex_train[, 2:11],
  carseats_ex_train$Sales,
  x.test = carseats_ex_test[, 2:11]
) -> bart_carseats_ex_1
bart_carseats_ex_1$varcount.mean

# The most important variables are Price, Comprice and ShelveLoc.

mean((carseats_ex_test$Sales - bart_carseats_ex_1$yhat.test.mean) ^ 2)
# Test MSE is 1.42 - much less than in other methods!

# Ex. 9
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

# b) c) d)
oj_ex_train %>%
  tree(
    Purchase ~ .,
    data = .
  ) -> tree_oj_1
tree_oj_1 %>% summary()
tree_oj_1 %>% plot()
tree_oj_1 %>% text()
tree_oj_1 

# Training error is 0.1588 for 9 terminal nodes.

# Significant predictors are LoyalCH, PriceDiff, SpecialCH, ListPriceDiff and PctDiscMM.
# The most important is customer brand loyalty for Citrus Hill, higher than 0.5 translates into purchase of Citrus Hill juice.
# For CH loyalty higher than 0.76 the only choice is CH juice.
# For CH loyalty between 0.5 and 0.76 difference in price between Citrus Hill and Minute Maid is also important. If difference is lower than 0.235 and there is discount for MM higher than 20%, they would choose MM.

# e)

table(
  predict(tree_oj_1, oj_ex_test, type = 'class'),
  oj_ex_test$Purchase
) -> prediction_tab
# Prediction accuracy (% of correctly predicted observations)
(sum(diag(prediction_tab)) / sum(prediction_tab) * 100) # 82.96%
# Classification error (% of incorrectly predicted observations)
(100 - (sum(diag(prediction_tab)) / sum(prediction_tab) * 100)) # 17.04%
# Sensitivity  (% of correctly predicted MM observations)
(prediction_tab[2, 2] / sum(prediction_tab[, 2]) * 100) # 62.75%
# Specificity  (% of correctly predicted CH observations)
(prediction_tab[1, 1] / sum(prediction_tab[, 1]) * 100) # 95.24%

# f) g) h)
cv.tree(tree_oj_1) -> cv_tree_oj_1

cv_tib_oj_1 <- tibble(
  CV_error = cv_tree_oj_1$dev,
  Size = cv_tree_oj_1$size
)
cv_tib_oj_1 %>%
  ggplot(., aes(x = Size, y = CV_error)) +
  geom_point() +
  labs(
    title = 'CV error vs number of terminal nodes',
    x = 'Number of terminal nodes',
    y = 'CV error',
    caption = 'Orange Juice dataset, own study'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = 1:9
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# The lowest cv error is obtained for 7 terminal nodes.

# i) j) k)
ptree_oj_1 <- prune.misclass(tree_oj_1, best = 7)
ptree_oj_1 %>% summary()
# Training error is 0.1625 - it's slightly higher than in previous tree.

table(
  predict(ptree_oj_1, oj_ex_test, type = 'class'),
  oj_ex_test$Purchase
) -> prediction_tab
# Prediction accuracy (% of correctly predicted observations)
(sum(diag(prediction_tab)) / sum(prediction_tab) * 100) # 83.7% (previously 82.96%)
# Classification error (% of incorrectly predicted observations)
(100 - (sum(diag(prediction_tab)) / sum(prediction_tab) * 100)) # 16.29% (previously 17.04%)
# Sensitivity  (% of correctly predicted MM observations)
(prediction_tab[2, 2] / sum(prediction_tab[, 2]) * 100) # 64.71% (previously 62.75%)
# Specificity  (% of correctly predicted CH observations)
(prediction_tab[1, 1] / sum(prediction_tab[, 1]) * 100) # 95.24% (previously 95.24%)
# The results are slightly better than in previous tree.

# Ex. 10.
# a)
hitters_ex <- Hitters[complete.cases(Hitters[, 'Salary']), ]
hitters_ex$Salary <- log(hitters_ex$Salary)

# b)
set.seed(1)
1:nrow(hitters_ex) %>%
  data.frame() %>%
  sample_n(200) %>%
  unlist() ->
  train_ind
hitters_ex[train_ind, ] -> hitters_ex_train
hitters_ex[-train_ind, ] -> hitters_ex_test

# c) d)
hitters_mse_tib <- tibble(
  Lambda = seq(.05, 1, .05),
  Train_MSE = rep(NA, 20),
  Test_MSE = rep(NA, 20)
)
for (i in 1:20) {
  hitters_ex_train %>%
    gbm::gbm(
      Salary ~ .,
      data = .,
      distribution = 'gaussian',
      n.trees = 1000,
      shrinkage = i / 20
    ) -> temp_model
  mean((
    hitters_ex_train$Salary -
      predict(temp_model, hitters_ex_train)
  ) ^ 2) ->
    hitters_mse_tib[i, 2]
  mean((
    hitters_ex_test$Salary -
      predict(temp_model, hitters_ex_test)
  ) ^ 2) ->
    hitters_mse_tib[i, 3]
}

hitters_mse_tib %>%
  ggplot(., aes(x = Lambda, y = Train_MSE)) +
  geom_point() +
  labs(
    title = 'Training MSE vs lambda parameter',
    x = 'Lambda parameter',
    y = 'Training MSE',
    caption = 'Hitters dataset, own study'
  ) +
  scale_x_continuous(breaks = seq(.05, 1, .05)) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()
# Lowest training MSE is obtained for lambda = 1 and equals 0.0003.

hitters_mse_tib %>%
  ggplot(., aes(x = Lambda, y = Test_MSE)) +
  geom_point() +
  labs(
    title = 'Test MSE vs lambda parameter',
    x = 'Lambda parameter',
    y = 'Test MSE',
    caption = 'Hitters dataset, own study'
  ) +
  scale_x_continuous(breaks = seq(.05, 1, .05)) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()
# Lowest test MSE is obtained for lambda = 0.05 and equals 0.287.

# e)
# Linear model
hitters_ex_train %>%
  lm(Salary ~ ., .) ->
  lm_hitters_ex_1
mean((
  hitters_ex_test$Salary -
    predict(lm_hitters_ex_1, hitters_ex_test)
) ^ 2)
# Test MSE = 0.48 (0.29 using boosting)

# Ridge regression
glmnet::cv.glmnet(
  model.matrix(Salary ~ ., hitters_ex_train)[, -1],
  hitters_ex_train$Salary,
  alpha = 0
) -> ridge_hitters_ex_1
mean((
  hitters_ex_test$Salary -
    predict(ridge_hitters_ex_1, model.matrix(Salary ~ ., hitters_ex_test)[, -1])
) ^ 2)
# Test MSE = 0.45 (0.29 using boosting)

# Lasso
glmnet::cv.glmnet(
  model.matrix(Salary ~ ., hitters_ex_train)[, -1],
  hitters_ex_train$Salary,
  alpha = 1
) -> lasso_hitters_ex_1
mean((
  hitters_ex_test$Salary -
    predict(lasso_hitters_ex_1, model.matrix(Salary ~ ., hitters_ex_test)[, -1])
) ^ 2)
# Test MSE = 0.45 (0.29 using boosting)

# PCR
hitters_ex_train %>%
  pls::pcr(
    Salary ~ .,
    data = .,
    scale = T,
    validation = 'CV'
  ) ->
  pcr_hitters_ex_1
mean((
  hitters_ex_test$Salary -
    predict(pcr_hitters_ex_1, hitters_ex_test)
) ^ 2)
# Test MSE = 0.46 (0.29 using boosting)

# PLS
hitters_ex_train %>%
  pls::plsr(
    Salary ~ .,
    data = .,
    scale = T,
    validation = 'CV'
  ) ->
  pls_hitters_ex_1
mean((
  hitters_ex_test$Salary -
    predict(pls_hitters_ex_1, hitters_ex_test)
) ^ 2)
# Test MSE = 0.48 (0.29 using boosting)

# In all approaches test MSE is significantly higher than using boosting.

# f)
hitters_ex_train %>%
  gbm::gbm(
    Salary ~ .,
    data = .,
    distribution = 'gaussian',
    n.trees = 1000,
    shrinkage = .05
  ) -> boost_hitters_ex_1
boost_hitters_ex_1 %>% summary()
# The most important variables are CRuns (number of runs during career), CRBI (number of runs batted in during career) and CAtBat (number of times at bat during career). The higher these predictors are, the higher the salary is.
boost_hitters_ex_1 %>% plot(., i = 'CRuns')
boost_hitters_ex_1 %>% plot(., i = 'CRBI')
boost_hitters_ex_1 %>% plot(., i = 'CAtBat')

# g)
set.seed(1)
hitters_ex_train %>%
  randomForestSRC::rfsrc(Salary ~ ., ., mtry = 20, importance = T) ->
  bag_hitters_ex_1
bag_hitters_ex_1 %>% plot()

# Most important variables are AtBat and CAtBat.

mean((
  hitters_ex_test$Salary - 
    predict(bag_hitters_ex_1, hitters_ex_test)$predicted
) ^ 2)
# Test MSE is 0.25, so it's slightly lower than in boosting.

# Ex. 11.
caravan_ex <- Caravan

# a)
set.seed(1)
1:nrow(caravan_ex) %>%
  data.frame() %>%
  sample_n(1000) %>%
  unlist() ->
  train_ind
caravan_ex[train_ind, ] -> caravan_ex_train
caravan_ex[-train_ind, ] -> caravan_ex_test

# b)
caravan_ex_train %>%
  gbm::gbm(
    Purchase ~ .,
    data = .,
    distribution = 'gaussian',
    n.trees = 1000,
    shrinkage = .01
  ) -> boost_caravan_ex_1
boost_caravan_ex_1 %>% summary()
# The most important predictors are PPERSAUT (contribution car policies), MAUT2 (2 cars) and ALEVEN (number of life insurances).

# c)

table(
  ifelse(
    predict(boost_caravan_ex_1, caravan_ex_test, type = 'response') > .2,
    'Yes', 'No'
  ),
  caravan_ex_test$Purchase
) -> prediction_tab
# Prediction accuracy (% of correctly predicted observations)
(prediction_tab[1, 2] / sum(prediction_tab) * 100) # 6.14%
# Classification error (% of incorrectly predicted observations)
(100 - (prediction_tab[1, 2] / sum(prediction_tab) * 100)) # 93.86%
# Sensitivity  (% of correctly predicted Yes observations)
(prediction_tab[1, 2] / sum(prediction_tab[, 2]) * 100) # 100%
# Specificity  (% of correctly predicted No observations)
# 0% - all No observations are predicted as Yes.


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
# KNN
# We can check KNN with different values of K:
set.seed(1)
knn_caravan_tib <- tibble(
  K = seq(1, 10, 1),
  correct_no = rep(NA, 10),
  correct_yes = rep(NA, 10),
  correct_all = rep(NA, 10)
)
for (i in 1:10) {
  temp_model <- class::knn(
    caravan_ex_train[, !names(caravan_ex_train) %in% c('Purchase')],
    caravan_ex_test[, !names(caravan_ex_test) %in% c('Purchase')],
    caravan_ex_train$Purchase,
    k = i
  )
  temp_tab <- table(temp_model, caravan_ex_test$Purchase)
  (temp_tab[1, 1] / sum(temp_tab[, 1])) %>%
    round(., 2) -> knn_caravan_tib[i, 2]
  (temp_tab[2, 2] / sum(temp_tab[, 2])) %>%
    round(., 2) -> knn_caravan_tib[i, 3]
  (sum(diag(temp_tab)) / sum(temp_tab)) %>%
    round(., 2) -> knn_caravan_tib[i, 4]
}
knn_caravan_tib %>%
  gather(Prediction, Percentage, -K) %>%
  ggplot(., aes(x = K, y = Percentage, color = Prediction)) +
  geom_line() +
  labs(
    title = 'Percentage of correctly predicted observations',
    subtitle = 'Using KNN method',
    x = 'Number of nearest neigbours',
    y = 'Percentage',
    caption = 'Source: Caravan dataset, own study'
  ) +
  scale_x_continuous(breaks = seq(1, 10, 1)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(
    name = 'Prediction of:',
    values = c('blue', 'red', 'green'),
    labels = c('All', 'No', 'Yes')
  ) +
  theme_bw()

# Let's try KNN with k = 4:
set.seed(1)
knn_caravan_1 <- class::knn(
  caravan_ex_train[, !names(caravan_ex_train) %in% c('Purchase')],
  caravan_ex_test[, !names(caravan_ex_test) %in% c('Purchase')],
  caravan_ex_train$Purchase,
  k = 4
)
knn_caravan_tab_1 <- table(knn_caravan_1, caravan_ex_test$Purchase)
conf_mat_fun(knn_caravan_tab_1)
# KNN works well with predicting 'No' observations, however it performs poorly when it comes to 'Yes' observations. It can be caused by imbalance in number of 'No' and 'Yes' - 'No' observations make more than 90% of population.

# Logistic regression
caravan_ex_train %>%
  glm(Purchase ~ ., data = ., family = binomial) -> glm_caravan_ex_1

glm_caravan_pred_1 <- predict.glm(
  glm_caravan_ex_1,
  newdata = caravan_ex_test,
  type = 'response'
)
glm_caravan_pred_1 <- ifelse(glm_caravan_pred_1 > .2, 'Yes', 'No')
glm_caravan_tab_1 <- table(glm_caravan_pred_1, caravan_ex_test$Purchase)
conf_mat_fun(glm_caravan_tab_1)
# Logistic regression is slightly worse than KNN, though is better in predicting 'Yes' observations.

  