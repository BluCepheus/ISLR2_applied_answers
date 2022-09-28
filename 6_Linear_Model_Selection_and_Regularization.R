library(ISLR2)
library(tidyverse)

# Functions used in this chapter. Should be run before use.

# Subset selection function. Input 'selection' is leaps::regsubsets result.
sub_sel_fun <- function(selection) {
  
  measure_tib <- tibble(
    Size = 1:(selection$nvmax - 1),
    Cp = summary(selection)$cp,
    BIC = summary(selection)$bic,
    Adj_R2 = summary(selection)$adjr2
  )
  
  measure_tib %>%
    ggplot(., aes(x = Size, y = Cp)) +
    geom_point() +
    labs(
      title = 'Cp vs size of model',
      x = 'Size of model',
      y = 'Cp'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = 1:(selection$nvmax - 1)
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_cp
  
  measure_tib %>%
    ggplot(., aes(x = Size, y = BIC)) +
    geom_point() +
    labs(
      title = 'BIC vs size of model',
      x = 'Size of model',
      y = 'BIC'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = 1:(selection$nvmax - 1)
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_bic
  
  measure_tib %>%
    ggplot(., aes(x = Size, y = Adj_R2)) +
    geom_point() +
    labs(
      title = 'Adjusted R2 vs size of model',
      x = 'Size of model',
      y = 'Adjusted R2'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = 1:(selection$nvmax - 1)
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_adjr2
  
  measure_tib %>% print()
  gridExtra::grid.arrange(plot_cp, plot_bic, plot_adjr2, nrow = 3)
}

# Ridge & lasso. 'model_mat' is model.matrix object, 'response' is vector of dependent variable, 'type' is in c('ridge', 'lasso').
ridge_lasso_fun <- function(model_mat, response, type) {
  if (type == 'ridge') {
    alpha = 0
  } else if (type == 'lasso') {
    alpha = 1
  }
  
  lambda_tib <- tibble(
    Lambda = rep(NA, 10),
    MSE = rep(NA, 10)
  )
  
  for (i in 1:10) {
    glmnet::cv.glmnet(
      model_mat[folds != i, ],
      response[folds != i],
      alpha = alpha
    ) -> temp_model
    lambda_tib[i, 1] <- temp_model$lambda.min
  }
  
  print('Coefficients matrix:')
  predict(
    glmnet::glmnet(model_mat, response),
    type = 'coefficients',
    s = lambda_tib$Lambda %>% mean(),
    alpha = alpha
  ) %>%
    round(., 4) %>%
    print()
  
  for (i in 1:10) {
    lambda_tib[i, 2] <-
      mean(
        (
          response[folds == i] -
            predict(
              glmnet::glmnet(
                model_mat[folds != i, ],
                response[folds != i]
              ),
              newx = model_mat[folds == i, ]
            )
        ) ^ 2
      )
  }
  print(
    paste(
      'Test MSE for model is:',
      lambda_tib$MSE %>% mean() %>% round(., 0)
    )
  )
}

# PCR & PLS. 'dataset' is given dataset, 'response' is vector of dependent variable, 'type' is in c('PCR', 'PLS')
pcr_pls_fun <- function(dataset, response, type) {
  cv_tibble <- 
    1:ncol(dataset) %>% 
    purrr::map_dfc(
      setNames,
      object = list(numeric())
    )
  
  for (i in 1:10) {
    if (type == 'PCR') {
      pls::pcr(
        response[folds != i] ~ .,
        data = dataset[folds != i, ],
        scale = T,
        validation = 'CV'
      ) -> temp_model
    } else if (type == 'PLS') {
      pls::plsr(
        response[folds != i] ~ .,
        data = dataset[folds != i, ],
        scale = T,
        validation = 'CV'
      ) -> temp_model
    }
    cv_tibble[i, ] <- temp_model$validation$PRESS
  }
  cv_tibble <- tibble(
    Components = 1:ncol(dataset),
    CV_error = apply(cv_tibble, 2, mean) %>% round(., 2)
  )
  cv_tibble %>% print()
  cv_tibble %>% 
    ggplot(., aes(x = Components, y = CV_error)) +
    geom_point() +
    labs(
      title = 'CV error vs number of components',
      subtitle = paste('Using', type, 'method'),
      x = 'Number of components',
      y = 'CV error',
    ) +
    scale_x_continuous(breaks = seq(1, ncol(dataset), 1)) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> temp_plot
  temp_plot %>% print()
  
  min_comp <- readline(
    prompt = 'Choose best number of components: '
  ) %>% as.numeric()
  
  mse_tibble <- tibble(MSE = rep(NA, 10))
  
  for (i in 1:10) {
    if (type == 'PCR') {
      pls::pcr(
        response[folds != i] ~ .,
        data = dataset[folds != i, ],
        scale = T,
        validation = 'CV'
      ) -> temp_model
    } else if (type == 'PLS') {
      pls::plsr(
        response[folds != i] ~ .,
        data = dataset[folds != i, ],
        scale = T,
        validation = 'CV'
      ) -> temp_model
    }
    mse_tibble[i, 1] <- mean(
      (
        response[folds == i] -
          predict(
            temp_model,
            dataset[folds == i, ],
            ncomp = min_comp
          )
      ) ^ 2
    )
  }
  
  print(
    paste(
      'Test MSE for model is',
      mse_tibble$MSE %>% mean() %>% round(., 0),
      'with',
      min_comp,
      'components'
    )
  )
  
}

# Ex. 8.
# a)
set.seed(1)
x <- rnorm(100)
e <- rnorm(100)

# b)
y <- 1 + 2 * x + 3 * x ^ 2 + 4 * x ^ 3 + e

# c)
xy_df <- data.frame(y)
for (i in 1:10) {
  xy_df[paste('x^', i, sep = '')] <- x ^ i
}

leaps::regsubsets(
  y ~ .,
  data = xy_df,
  nvmax = 20
) -> rs_xy_1

sub_sel_fun(rs_xy_1)
# Cp is the lowest for model of size 4. BIC is the lowest for model of size 3. Adjusted R2 is the highest for model of size 3. Measures indicates model of size 3.
rs_xy_1 %>% coef(id = 3)
# B0 = 1.06, B1 = 1.98, B2 = 2.88, B3 = 4.02 - these values are very close to true values of coefficients.

# d)

leaps::regsubsets(
  y ~ .,
  data = xy_df,
  nvmax = 10,
  method = 'forward'
) -> rs_xy_2

sub_sel_fun(rs_xy_2)
# Forward selection provided the same results as above.

leaps::regsubsets(
  y ~ .,
  data = xy_df,
  nvmax = 10,
  method = 'backward'
) -> rs_xy_3

sub_sel_fun(rs_xy_3)
# Cp is the lowest for model of size 4. BIC is the lowest for model of size 3. Adjusted R2 is the highest for model of size 3. Measures indicates model of size 3.

# e)
set.seed(1)
folds <- sample(rep(1:10, length = nrow(xy_df)))
xy_mat <- model.matrix(y ~ ., xy_df)[, -1]

ridge_lasso_fun(xy_mat, y, 'lasso')
# Lasso model considers intercept and coefficients for x^1 - x^5 & x^7 as statistically significant, though values for x^4, x^5 and x^7 are very close to 0. Other coefficients are close to true coefficients (1.18 vs 1 for B0, 2.14 vs 2 for B1, 2.62 vs 3 for B2 and 3.81 vs 4 for B3).

# f)
y <- 1 + 7 * x ^ 7 + e
xy_df <- data.frame(y)
for (i in 1:10) {
  xy_df[paste('x^', i, sep = '')] <- x ^ i
}


leaps::regsubsets(
  y ~ .,
  data = xy_df,
  nvmax = 10
) -> rs_xy_4
sub_sel_fun(rs_xy_4)
# Cp is the lowest for model of size 2. BIC is the lowest for model of size 1. Adjusted R2 is the highest for model of size 3.

ridge_lasso_fun(xy_mat, y, 'lasso')
# Lasso model considers only intercept (1.82 vs 1) and coefficient for x^7 (6.79 vs 7). This result is much closer to the true form of y than best subset selection.

# Ex. 9.
# a)
set.seed(1)
folds <- sample(rep(1:10, length = nrow(college_ex)))

# b)
mse_college_tib <- tibble(MSE = rep(NA, 10))
for (i in 1:10) {
  mse_college_tib[i, 1] <-
    mean(
      (
        college_ex$Apps[folds == i] -
          predict.glm(
            glm(
              Apps ~ .,
              data = college_ex[folds != i, ]
            ),
            newdata = college_ex[folds == i, ]
          )
      ) ^ 2
    )
}
mse_college_tib$MSE %>% mean() # 1 307 343

# c)
college_mat <- model.matrix(Apps ~ ., college_ex)[, -1]
ridge_lasso_fun(college_mat, college_ex$Apps, 'ridge')
# Test MSE: 2 411 191 with 4 components

# d)
ridge_lasso_fun(college_mat, college_ex$Apps, 'lasso')
# Test MSE: 2 411 191 with 17 components

# e)
pcr_pls_fun(college_ex, college_ex$Apps, 'PCR')
# Test MSE: 54 168 with 18 components

# f)
pcr_pls_fun(college_ex, college_ex$Apps, 'PLS')
# Test MSE: 10 083 with 9 components

# g)
# The lowest test MSE (10 083) is provided by PLS model with 9 components. There is huge difference between approaches above: PCR provides relatively low test error, but with 18 components., while linear regression, ridge regression and lasso model have test errors higher than 1 million.

# Ex. 10.
# a)
set.seed(1)
x <- matrix(rnorm(20000), 1000, 20)
beta <- rnorm(20)
beta[5] <- 0
beta[10] <- 0
beta[15] <- 0
beta[20] <- 0
e <- rnorm(1000)
y <- x %*% beta + e 

# b)
xy_colnames <- c()
for (i in 1:20) {
  xy_colnames <- c(xy_colnames, paste('x_', i, sep = ''))
}
xy_df_train <- data.frame(y[1:100, ], x[1:100, ])
colnames(xy_df_train) <- c('y', xy_colnames)
xy_df_test <- data.frame(y[101:1000, ], x[101:1000, ])
colnames(xy_df_test) <- c('y', xy_colnames)

# c)

leaps::regsubsets(
  y ~ .,
  data = xy_df_train,
  nvmax = 20
) -> rs_xy_7

sub_sel_fun(rs_xy_7)
# Just to be sure: Cp is the lowest for model of size 16. BIC is the lowest for model of size 15. Adjusted R2 is the highest for models of size 16-18.

xy_train_mse_tib <- tibble(
  Size = 1:20,
  MSE = summary(rs_xy_7)$rss / nrow(xy_df_train)
)
xy_train_mse_tib %>%
  ggplot(., aes(x = Size, y = MSE)) +
  geom_point() +
  labs(
    title = 'Train MSE vs size of model',
    caption = 'Source: own study',
    x = 'Size of model',
    y = 'Train MSE'
  ) +
  scale_x_continuous(breaks = 1:20) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()
# Train MSE is decreasing as size of model is growing, reaching minimum at size of 19

# d)
xy_test_mse_tib <- tibble(
  Size = 1:20,
  MSE = rep(NA, 20)
)
for (i in 1:20) {
  xy_test_mse_tib[i, 2] <-
    mean(
      (
        xy_df_test$y -
          predict(
            HH::lm.regsubsets(rs_xy_7, i),
            newdata = xy_df_test,
            type = 'response'
          )
      ) ^ 2
    )
}
xy_test_mse_tib %>%
  ggplot(., aes(x = Size, y = MSE)) +
  geom_point() +
  labs(
    title = 'Test MSE vs size of model',
    caption = 'Source: own study',
    x = 'Size of model',
    y = 'Test MSE'
  ) +
  scale_x_continuous(breaks = 1:20) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()
# Test MSE is decreasing as size of model is growing, reaching minimum at size of 15, when it starts to increase.

# e)
# Test MSE takes on its minimum value for model of size 15.

# f)
xy_coeff_arr <- rs_xy_7 %>% coef(., 15) %>% names()
xy_coeff_arr[-1] %>%
  str_sub(start = 3, end = -1) %>%
  as.numeric ->
  xy_coeff_arr_num

tibble(
  Coefficient = xy_coeff_arr,
  Estimated = rs_xy_7 %>% coef(., 15),
  True = c(0, beta[xy_coeff_arr_num]),
  Difference = True - Estimated
)
# For most cases differences between estimated and true values of coefficients are relatively small with exception of intercept and coefficients for x_7, x_8, x_12 and x_19.

# g)

xy_error_tib <- tibble(
  Size = 1:20,
  Error = rep(NA, 20)
)

for (i in 1:20) {
  xy_temp_coeff_arr <- rs_xy_7 %>% coef(., i) %>% names()
  xy_temp_coeff_arr[-1] %>%
    str_sub(start = 3, end = -1) %>%
    as.numeric ->
    xy_temp_coeff_arr_num
  
  (
    c(0, beta[xy_coeff_arr_num]) -
      rs_xy_7 %>% coef(., i)
  ) ^ 2 %>%
    sum() %>%
    sqrt() ->
    xy_error_tib[i, 2]
}

xy_error_tib %>%
  ggplot(., aes(x = Size, y = Error)) +
  geom_point() +
  labs(
    title = 'Coefficient error vs size of model',
    caption = 'Source: own study',
    x = 'Size of model',
    y = 'Coefficient error'
  ) +
  scale_x_continuous(breaks = 1:20) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Errors take various values without visible order or trend, nonetheless they reach minimum value for model of size 15.

xy_final_tib <- tibble(
  Size = xy_test_mse_tib$Size,
  MSE = xy_test_mse_tib$MSE,
  Error = xy_error_tib$Error
)

xy_final_tib %>%
  gather(Error_type, Value, -Size) %>%
  ggplot(
    .,
    aes(
      x = Size,
      y = Value,
      color = Error_type
    )
  ) +
  geom_point() +
  labs(
    title = 'Errors vs size of model',
    caption = 'Source: own study',
    x = 'Size of model',
    y = 'Error value'
  ) +
  scale_x_continuous(breaks = 1:20) +
  scale_y_continuous(labels = scales::number) +
  scale_color_manual(
    name = 'Error type',
    values = c('blue', 'red'),
    labels = c('Coefficient error', 'MSE')
  ) +
  theme_bw()

# While test MSE generally decreases as size of model grows, we cannot say the same thing about coefficient error. However, in both case minimum value can be observed for model of size 15.

# Ex. 11.
# a) b) c)

leaps::regsubsets(
  crim ~ .,
  data = boston_ex,
  nvmax = 20
) -> rs_boston_1

sub_sel_fun(rs_boston_1)
# Cp is the lowest for model of size 7. BIC is the lowest for model of size 2. Adjusted R2 is the highest for models of size 8-10.

set.seed(1)
folds <- sample(rep(1:10, length = nrow(boston_ex)))
boston_mat <- model.matrix(crim ~ ., boston_ex)[, -1]

ridge_lasso_fun(boston_mat, boston_ex$crim, 'lasso')
# Lasso proposes model without 'age' variable. Test MSE is 46.

ridge_lasso_fun(boston_mat, boston_ex$crim, 'ridge')
# Lasso proposes model with dis, rad, lstat and medv. Test MSE is 46.

pcr_pls_fun(boston_ex, boston_ex$crim, 'PCR')
# Test MSE for model is 2 with 8 components.

pcr_pls_fun(boston_ex, boston_ex$crim, 'PLS')
# Test MSE for model is 0 with 6 components.
















