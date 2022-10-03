library(ISLR2)
library(tidyverse)

# Functions used in this chapter. Should be run before use.

# Polynomial regression with cross-validation and ANOVA. 'dataset' is given dataset, 'response' is vector of dependent variable, 'predictor' is vector of independent variable.

poly_reg_fun <- function(dataset, response, predictor) {
  set.seed(1)
  folds <- sample(rep(1:10, length = nrow(dataset)))
  measure_tib <- tibble(
    Degree = 1:10,
    CV_error = rep(NA, 10),
    ANOVA = rep(NA, 10)
  )
  anova_mat <- matrix(NA, nrow = 10, ncol = 10)
  
  for (i in 1:10) {
    boot::cv.glm(
      dataset,
      glm(
        response ~ poly(predictor, i, raw = T),
        data = dataset
      ),
      K = 10
    )$delta[2] ->
      measure_tib[i, 2]
    
    model_list <- list()
    for (j in 1:10) {
      model_list[[j]] <-
        lm(
          response[folds != i] ~ poly(predictor[folds != i], j, raw = T),
          data = dataset[folds != i, ]
        )
    }
    anova_mat[i, ] <- t(do.call(anova, model_list)['Pr(>F)'])
  }
  measure_tib$ANOVA <- apply(anova_mat, 2, mean) %>% round(., 4)
  measure_tib %>% print()
  
  measure_tib %>%
    ggplot(., aes(x = Degree, y = CV_error)) +
    geom_point() +
    labs(
      title = 'CV error vs degree of polynomial',
      x = 'Degree of polynomial',
      y = 'CV error'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = 1:10
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_cv
  
  measure_tib %>%
    ggplot(., aes(x = Degree, y = ANOVA)) +
    geom_point() +
    labs(
      title = 'ANOVA p-value vs degree of polynomial',
      x = 'Degree of polynomial',
      y = 'ANOVA p-value'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = 1:10
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_anova
  
  gridExtra::grid.arrange(plot_cv, plot_anova, nrow = 2)
}

# Step function. 'dataset' is given dataset, 'response' is vector of dependent variable, 'predictor' is vector of independent variable.

step_fun <- function(dataset, response, predictor) {
  measure_tib <- tibble(
    Cuts = 2:10,
    CV_error = rep(NA, 9)
  )
  
  for (i in 2:10) {
    boot::cv.glm(
      dataset,
      glm(
        response ~ cut(predictor, i),
        data = dataset
      ),
      K = 10
    )$delta[2] ->
      measure_tib[i - 1, 2]
  }
  measure_tib %>% print()
  
  measure_tib %>%
    ggplot(., aes(x = Cuts, y = CV_error)) +
    geom_point() +
    labs(
      title = 'CV error vs number of cuts',
      x = 'Number of cuts',
      y = 'CV error'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = 2:10
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_cv
  plot_cv %>% print()
}

# Splines and local regression function. 'dataset' is given dataset, 'response' is vector of dependent variable, 'predictor' is vector of independent variable.

spline_lr_fun <- function(dataset, response, predictor) {
  measure_tib <- tibble(
    DF = 1:10,
    NS_CV_error = rep(NA, 10),
    BS_CV_error = rep(NA, 10),
    LR_CV_error = rep(NA, 10)
  )
  
  for (i in 1:10) {
    boot::cv.glm(
      dataset,
      glm(
        response ~ splines::ns(predictor, df = i),
        data = dataset
      ),
      K = 10
    )$delta[2] ->
      measure_tib[i, 2]
    
    boot::cv.glm(
      dataset,
      glm(
        response ~ splines::bs(predictor, df = i),
        data = dataset
      ),
      K = 10
    )$delta[2] ->
      measure_tib[i, 3]
    
    boot::cv.glm(
      dataset,
      loess(
        response ~ predictor,
        span = i / 10,
        data = dataset
      ),
      K = 10
    )$delta[2] ->
      measure_tib[i, 4]
  }
  
  measure_tib %>% print()
  
  smooth.spline(
    predictor,
    response,
    cv = T
  ) -> temp_model
  
  print(
    paste(
      'Smooth spline selected',
      temp_model$df %>% round(., 0),
      'as optimal number of degrees of freedom, with CV error',
      temp_model$cv.crit %>% round(., 2)
    )
  )
  
  measure_tib %>%
    ggplot(., aes(x = DF, y = NS_CV_error)) +
    geom_point() +
    labs(
      title = 'CV error vs degrees of freedom',
      subtitle = 'Using natural spline',
      x = 'Degrees of freedom',
      y = 'CV error'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = 1:10
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_ns
  
  measure_tib %>%
    ggplot(., aes(x = DF, y = BS_CV_error)) +
    geom_point() +
    labs(
      title = 'CV error vs degrees of freedom',
      subtitle = 'Using B-spline',
      x = 'Degrees of freedom',
      y = 'CV error'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = 1:10
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_bs
  
  measure_tib %>%
    ggplot(., aes(x = DF / 10, y = LR_CV_error)) +
    geom_point() +
    labs(
      title = 'CV error vs span',
      subtitle = 'Using local regression',
      x = 'Span',
      y = 'CV error'
    ) +
    scale_x_continuous(
      labels = scales::number,
      breaks = seq(.1, 1, .1)
    ) +
    scale_y_continuous(labels = scales::number) +
    theme_bw() -> plot_lr
  
  gridExtra::grid.arrange(plot_ns, plot_bs, plot_lr, nrow = 3)
}

# Ex. 6.
# a)
wage_ex <- Wage
poly_reg_fun(
  wage_ex,
  wage_ex$wage,
  wage_ex$age
)
# CV errors and ANOVA suggest 3rd degree for the polynomial.

wage_ex %>%
  ggplot(., aes(x = age, y = wage)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    formula = y ~ poly(x, 3, raw = T),
    color = 'blue',
    fill = 'blue'
  ) +
  labs(
    title = 'Workers raw wage vs age of worker',
    subtitle = 'with 3rd degree polynomial fit',
    caption = 'Source: Mid-Atlantic Wage dataset, own study',
    x = 'Age of worker',
    y = 'Workers raw wage'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# b)
step_fun(
  wage_ex,
  wage_ex$wage,
  wage_ex$age
)
# CV errors suggest 2 cuts.

wage_ex %>%
  ggplot(., aes(x = age, y = wage)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    formula = y ~ cut(x, 2),
    color = 'blue',
    fill = 'blue'
  ) +
  labs(
    title = 'Workers raw wage vs age of worker',
    subtitle = 'with step function with 2 cuts',
    caption = 'Source: Mid-Atlantic Wage dataset, own study',
    x = 'Age of worker',
    y = 'Workers raw wage'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Ex. 7.
# Wage vs marital status

wage_ex %>%
  ggplot(., aes(x = maritl, y = wage)) +
  geom_boxplot() +
  labs(
    title = 'Workers raw wage vs marital status',
    caption = 'Source: Mid-Atlantic Wage dataset, own study',
    x = 'Marital status',
    y = 'Workers raw wage'
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

wage_ex$maritl %>% table()
tapply(wage_ex$wage, wage_ex$maritl, summary)

# Married workers have higher median of wage than other groups, on the other hand never married persons have the lowest median of wage. Both factors have relatively long whiskers between upper hinge and maximum. Let's see how statistics above looks like for wages lower than 250:
wage_ex %>%
  filter(., wage <= 250) ->
  wage_ex_lower

wage_ex_lower %>%
  ggplot(., aes(x = maritl, y = wage)) +
  geom_boxplot() +
  labs(
    title = 'Workers raw wage vs marital status',
    subtitle = 'excluding observations with wage higher than 250',
    caption = 'Source: Mid-Atlantic Wage dataset, own study',
    x = 'Marital status',
    y = 'Workers raw wage'
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

wage_ex_lower$maritl %>% table()
tapply(wage_ex_lower$wage, wage_ex_lower$maritl, summary)

# We can see that 'Widowed', 'Divorced' and 'Separated' are relatively small groups. We can add 'Separated' to 'Married' and unite 'Widowed' and 'Divorced' into one group.
maritl_new_levels <- c(
  '3. Widowed' = '3. Not married anymore',
  '4. Divorced' = '3. Not married anymore',
  '5. Separated' = '2. Married'
)
wage_ex_lower$maritl %>%
  recode_factor(., !!!maritl_new_levels) -> 
  wage_ex_lower$maritl
wage_ex_lower$maritl %>% table()
tapply(wage_ex_lower$wage, wage_ex_lower$maritl, summary)

# Now GAM:
wage_ex_lower %>%
  gam::gam(wage ~ maritl, data = .) -> gam_wage_1
gam_wage_1 %>% summary()
gam_wage_1 %>% gam::plot.Gam(., se = T , col = 'blue')

# Ex. 8.
auto_ex %>%
  GGally::ggpairs(., columns = 1:8)
# There is suspicion of non-linear relationship between mpg and weight.
# Let's check polynomial regression:
poly_reg_fun(
  auto_ex,
  auto_ex$mpg,
  auto_ex$weight
)
# CV errors suggest polynomial of degree 6, while ANOVA suggests polynomial of degree 2.
auto_ex %>%
  ggplot(., aes(x = weight, y = mpg)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    formula = y ~ poly(x, 2, raw = T),
    aes(color = '2nd', fill = '2nd')
  ) +
  geom_smooth(
    method = 'glm',
    formula = y ~ poly(x, 6, raw = T),
    aes(color = '6th', fill = '6th')
  ) +
  labs(
    title = 'Miles per gallons vs vehicle weight',
    subtitle = 'with 2nd and 6th degree polynomial fit',
    caption = 'Source: Auto dataset, own study',
    x = 'Vehicle weight',
    y = 'Miles per gallons'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  scale_fill_manual(
    name = 'Polynomial degree',
    values = c('blue', 'red')) +
  scale_colour_manual(
    name = 'Polynomial degree',
    values = c('blue', 'red'),
    labels = c('2nd', '6th')
  ) +
  theme_bw()
# Both polynomials are relatively similar except of very low and very high valus of vehicle weight, where confidence interval for polynomial of degree 6 is visibly wider.

# Step function:
step_fun(
  auto_ex,
  auto_ex$mpg,
  auto_ex$weight
)
# CV errors suggest 3 or 4 cuts. Let's plot them both:
auto_ex %>%
  ggplot(., aes(x = weight, y = mpg)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    formula = y ~ cut(x, 3),
    aes(color = '3', fill = '3')
  ) +
  geom_smooth(
    method = 'glm',
    formula = y ~ cut(x, 4),
    aes(color = '4', fill = '4')
  ) +
  labs(
    title = 'Miles per gallons vs vehicle weight',
    subtitle = 'with step function with 3 and 4 cuts',
    caption = 'Source: Auto dataset, own study',
    x = 'Vehicle weight',
    y = 'Miles per gallons'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  scale_fill_manual(
    name = 'Number of cuts',
    values = c('blue', 'red')) +
  scale_colour_manual(
    name = 'Number of cuts',
    values = c('blue', 'red'),
    labels = c('3', '4')
  ) +
  theme_bw()

# Splines and local regression:
spline_lr_fun(
  auto_ex,
  auto_ex$mpg,
  auto_ex$weight
)

# Natural and B splines suggest 6 degrees of freedom, while smooth spline suggests 10 degrees of freedom.
# Local regression suggests span = .6.

auto_ex %>%
  ggplot(., aes(x = weight, y = mpg)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    formula = y ~ splines::ns(x, 6),
    aes(color = 'NS', fill = 'NS')
  ) +
  geom_smooth(
    method = 'glm',
    formula = y ~ splines::bs(x, 6),
    aes(color = 'BS', fill = 'BS')
  ) +
  geom_smooth(
    method = 'gam',
    formula = y ~ gam::s(x, df = 10),
    aes(color = 'Smooth', fill = 'Smooth')
  ) +
  geom_smooth(
    method = 'loess',
    span = .6,
    aes(color = 'LR', fill = 'LR')
  ) +
  labs(
    title = 'Miles per gallons vs vehicle weight',
    subtitle = 'with splines and local regression',
    caption = 'Source: Auto dataset, own study',
    x = 'Vehicle weight',
    y = 'Miles per gallons'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  scale_fill_manual(
    name = 'Method',
    values = c('blue', 'red', 'green', 'yellow'),
    labels = c('Natural spline', 'B-spline', 'Smooth spline', 'Local regression')
  ) +
  scale_colour_manual(
    name = 'Method',
    values = c('blue', 'red', 'green', 'yellow'),
    labels = c('Natural spline', 'B-spline', 'Smooth spline', 'Local regression')
  ) +
  theme_bw()

# Ex. 9.
# a)
boston_ex %>%
  glm(nox ~ poly(dis, 3), data = .) -> 
  boston_poly_1
boston_poly_1 %>% summary()
# Coefficients for all polynomial degrees are statistically significant.

boston_ex %>%
  ggplot(., aes(x = dis, y = nox)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    formula = y ~ poly(x, 3, raw = T),
    color = 'blue',
    fill = 'blue'
  ) +
  labs(
    title = 'NO concentration vs distances to Boston employment centres',
    subtitle = 'with 3rd degree polynomial fit',
    caption = 'Source: Boston dataset, own study',
    x = 'Weighted mean of distances to five Boston employment centres',
    y = 'Nitrogen oxides concentration (parts per 10 million)'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# b)
boston_ex %>%
  ggplot(., aes(x = dis, y = nox)) +
  geom_point() +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 1, raw = T), aes(color = '1', fill = '1')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 2, raw = T), aes(color = '2', fill = '2')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 3, raw = T), aes(color = '3', fill = '3')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 4, raw = T), aes(color = '4', fill = '4')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 5, raw = T), aes(color = '5', fill = '5')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 6, raw = T), aes(color = '6', fill = '6')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 7, raw = T), aes(color = '7', fill = '7')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 8, raw = T), aes(color = '8', fill = '8')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 9, raw = T), aes(color = '9', fill = '9')) +
  geom_smooth(method = 'glm', formula = y ~ poly(x, 10, raw = T), aes(color = '10', fill = '10')) +
  labs(
    title = 'NO concentration vs distances to Boston employment centres',
    subtitle = 'with 1st - 10th degree polynomial fit',
    caption = 'Source: Boston dataset, own study',
    x = 'Weighted mean of distances to five Boston employment centres',
    y = 'Nitrogen oxides concentration (parts per 10 million)'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  scale_fill_manual(
    name = 'Degree of polynomial',
    values = c('aquamarine', 'azure', 'bisque', 'brown', 'blue', 'red', 'green', 'yellow', 'cadetblue', 'darkorange'),
    labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')
  ) +
  scale_colour_manual(
    name = 'Degree of polynomial',
    values = c('aquamarine', 'azure', 'bisque', 'brown', 'blue', 'red', 'green', 'yellow', 'cadetblue', 'darkorange'),
    labels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10')
  ) +
  theme_bw()

boston_rss_tib <- tibble(
  Degree = 1:10,
  RSS = rep(NA, 10)
)

for (i in 1:10) {
  boston_ex %>%
    glm(nox ~ poly(dis, i), data = .) %>%
    deviance() ->
    boston_rss_tib[i, 2]
}

boston_rss_tib

boston_rss_tib %>%
  ggplot(., aes(x = Degree, y = RSS)) +
  geom_point() +
  labs(
    title = 'RSS vs degree of polynomial',
    subtitle = 'For polynomial degrees 1 to 10',
    caption = 'Source: Boston dataset, own study',
    x = 'Degree of polynomial',
    y = 'RSS'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = 1:10
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# c)
poly_reg_fun(boston_ex, boston_ex$nox, boston_ex$dis)
# Cross-validation suggests polynomial of degree 4, while ANOVA suggests polynomials of degrees 2 and 3.

# d)
boston_ex %>%
  glm(nox ~ splines::bs(dis, df = 4), data = .) ->
  boston_bs_model

boston_bs_model %>% summary()
# Spline is statistically significant.
attr(splines::bs(boston$dis, df = 4), 'knots')
# splines::bs can automatically choose number of knots, this time it's one knot equals mean of predictor.

boston_ex %>%
  ggplot(., aes(x = dis, y = nox)) +
  geom_point() +
  geom_smooth(
    method = 'glm',
    formula = y ~ splines::bs(x, df = 4),
    color = 'blue',
    fill = 'blue'
  ) +
  labs(
    title = 'NO concentration vs distances to Boston employment centres',
    subtitle = 'with B-Spline with 4 degrees of freedom',
    caption = 'Source: Boston dataset, own study',
    x = 'Weighted mean of distances to five Boston employment centres',
    y = 'Nitrogen oxides concentration (parts per 10 million)'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# e)

boston_rss_bs_tib <- tibble(
  DF = 3:10,
  RSS = rep(NA, 8)
)

for (i in 3:10) {
  boston_ex %>%
    glm(nox ~ splines::bs(dis, df = i), data = .) %>%
    deviance() ->
    boston_rss_bs_tib[i - 2, 2]
}

boston_rss_bs_tib

boston_rss_bs_tib %>%
  ggplot(., aes(x = DF, y = RSS)) +
  geom_point() +
  labs(
    title = 'RSS vs degrees of freedom',
    subtitle = 'For degrees of freedem from 3 to 10',
    caption = 'Source: Boston dataset, own study',
    x = 'Degrees of freedom',
    y = 'RSS'
  ) +
  scale_x_continuous(
    labels = scales::number,
    breaks = 3:10
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# Lowest RSS value is obtained for DF = 10.

boston_ex %>%
  ggplot(., aes(x = dis, y = nox)) +
  geom_point() +
  geom_smooth(method = 'glm', formula = y ~ splines::bs(x, 3), aes(color = '3', fill = '3')) +
  geom_smooth(method = 'glm', formula = y ~ splines::bs(x, 4), aes(color = '4', fill = '4')) +
  geom_smooth(method = 'glm', formula = y ~ splines::bs(x, 5), aes(color = '5', fill = '5')) +
  geom_smooth(method = 'glm', formula = y ~ splines::bs(x, 6), aes(color = '6', fill = '6')) +
  geom_smooth(method = 'glm', formula = y ~ splines::bs(x, 7), aes(color = '7', fill = '7')) +
  geom_smooth(method = 'glm', formula = y ~ splines::bs(x, 8), aes(color = '8', fill = '8')) +
  geom_smooth(method = 'glm', formula = y ~ splines::bs(x, 9), aes(color = '9', fill = '9')) +
  geom_smooth(method = 'glm', formula = y ~ splines::bs(x, 10), aes(color = '10', fill = '10')) +
  labs(
    title = 'NO concentration vs distances to Boston employment centres',
    subtitle = 'with degrees of freedom from 3 to 10',
    caption = 'Source: Boston dataset, own study',
    x = 'Weighted mean of distances to five Boston employment centres',
    y = 'Nitrogen oxides concentration (parts per 10 million)'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  scale_fill_manual(
    name = 'Degree of polynomial',
    values = c('aquamarine', 'azure', 'bisque', 'brown', 'blue', 'red', 'green', 'yellow'),
    labels = c('3', '4', '5', '6', '7', '8', '9', '10')
  ) +
  scale_colour_manual(
    name = 'Degree of polynomial',
    values = c('aquamarine', 'azure', 'bisque', 'brown', 'blue', 'red', 'green', 'yellow'),
    labels = c('3', '4', '5', '6', '7', '8', '9', '10')
  ) +
  theme_bw()

# f)
spline_lr_fun(boston_ex, boston_ex$nox, boston_ex$dis)
# Cross validation for natural spline suggests 1 DF, for B-Spline - 2 DFs, for smooth splines - 15 DFs.

# Ex. 10.
# a)
# We would use custom function from chapter 6:
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

set.seed(1)
college_ex$id <- 1:nrow(college_ex)
college_ex %>%
  sample_frac(.8) -> college_ex_train
anti_join(college_ex, college_ex_train, by = 'id') -> college_ex_test

leaps::regsubsets(
  Outstate ~ . - id,
  data = college_ex_train,
  nvmax = 20,
  method = 'forward'
) -> rs_college_1

sub_sel_fun(rs_college_1)
# Cp is the lowest for model of size 12. BIC is the lowest for model of size 11. Adjusted R2 is the highest for models of size 13-16.
# However, models of 'neighbouring' size have only slightly worse statistics. Model of size 12 seems the best.

rs_college_1 %>% summary()
# Model of size 12 has following predictors: Private, Apps, Accept, Enroll, Top10perc, Room.Board, Personal, PhD, Terminal, perc.alumni, Expend, Grad.Rate

# b-d)
# Firstly let's look at relationships between Outstate and predictors:
GGally::ggduo(
  college_ex_train,
  'Outstate', 
  c(
    'Private', 'Apps', 'Accept', 'Enroll', 'Top10perc', 
    'Room.Board', 'Personal', 'PhD', 'Terminal', 'perc.alumni', 
    'Expend', 'Grad.Rate'
  )
)

# Number of degrees of freedom was choosed using cross-validation of smooth spline from custom function spline_lr_fun:

college_ex_train %>%
  gam::gam(
    Outstate ~ Private + gam::s(Apps, 8) + gam::s(Accept, 24) + 
      gam::s(Enroll, 13) + gam::s(Top10perc, 2) + gam::s(Room.Board, 8) + 
      gam::s(Personal, 25) + gam::s(PhD, 5) + gam::s(Terminal, 4) +
      gam::s(perc.alumni, 2) + gam::s(Expend, 5) + gam::s(Grad.Rate, 9),
    data = .
  ) -> gam_college_1
gam_college_1 %>% summary()
gam_college_1$coef
# Coefficients for all variables are statistically significant, except Enroll.
# Number of applications received, number of new students enrolled and personal spending have negative influence on value of out-of-state tuition (assuming that other variables are fixed).
par(mfrow = c(3, 4))
gam_college_1 %>% gam::plot.Gam(., se = T , col = 'blue')

# c)
mean(
  (college_ex_test$Outstate -
    predict(gam_college_1, newdata = college_ex_test)) ^ 2
)
# Test MSE is 3 227 322. Hence RMSE is 1796.475

# d)
# We can check e.g. Expend variable.

college_ex_train %>%
  gam::gam(
    Outstate ~ Private + gam::s(Apps, 8) + gam::s(Accept, 24) + 
      gam::s(Enroll, 13) + gam::s(Top10perc, 2) + gam::s(Room.Board, 8) + 
      gam::s(Personal, 25) + gam::s(PhD, 5) + gam::s(Terminal, 4) +
      gam::s(perc.alumni, 2) + gam::s(Expend, 5),
    data = .
  ) -> gam_college_2

anova(gam_college_2, gam_college_1, test = 'F')

# Ex. 11.
# a) b)
x1 <- rnorm(100)
x2 <- rnorm(100)
e <- rnorm(100)
y <- 1 + 2 * x1 + 3 * x2 + e

# c)
y_new <- y - 2 * x1 
lm(y_new ~ x2) -> lm_xy_17
lm_xy_17 %>% summary()

# d)
y_new <- y - 3 * x2
lm(y_new ~ x1) -> lm_xy_18
lm_xy_18 %>% summary()

# e)
xy_coef_tib <- tibble(
  Iteration = 1:1000,
  B0 = rep(NA, 1000),
  B1 = rep(NA, 1000),
  B2 = rep(NA, 1000)
)

B1 <- 2
for (i in 1:1000) {
  y_new <- y - B1 * x1
  lm(y_new ~ x2) -> temp_model
  temp_model$coef[1] -> xy_coef_tib[i, 2]
  temp_model$coef[2] -> xy_coef_tib[i, 4]
  temp_model$coef[2] -> B2
  
  y_new <- y - B2 * x2
  lm(y_new ~ x1) -> temp_model
  temp_model$coef[2] -> xy_coef_tib[i, 3]
  temp_model$coef[2] -> B1
}

xy_coef_tib %>%
  gather(Coef, Value, -Iteration) %>%
  ggplot(., aes(x = Iteration, y = Value, color = Coef)) +
  geom_line() +
  labs(
    title = 'Value of coefficient vs iteration',
    caption = 'Source: own study',
    y = 'Value of coefficient',
    x = 'Iteration'
  ) +
  scale_y_continuous(labels = scales::number) +
  scale_color_manual(
    name = 'Coefficient',
    values = c('blue', 'red', 'green'),
    labels = c('B0', 'B1', 'B2')
  ) +
  theme_bw() -> plot_xy_4

# f)
glm(y ~ x1 + x2) -> glm_xy_1
glm_xy_1 %>% summary()

plot_xy_4 +
  geom_hline(yintercept = glm_xy_1$coef[1]) +
  geom_hline(yintercept = glm_xy_1$coef[2]) +
  geom_hline(yintercept = glm_xy_1$coef[3])

# Black horizontal lines covered coefficients from previous chart. As we can see, differences between coefficients from loop and glm are close to zero:
xy_coef_tib[1000, 2:4] - glm_xy_1$coef
xy_coef_tib[1, 2:4] - glm_xy_1$coef

# g)
# After 7th iteration coefficients from loop were actually equal to those from glm.
# We can show that better in table of differences between coefficients:
tibble(
  Iteration = 1:10,
  B0 = (xy_coef_tib[1:10, 2] - rep(glm_xy_1$coef[1], 10)),
  B1 = (xy_coef_tib[1:10, 3] - rep(glm_xy_1$coef[2], 10)),
  B2 = (xy_coef_tib[1:10, 4] - rep(glm_xy_1$coef[3], 10))
) %>%
  rows_insert(
    .,
    tibble(
      Iteration = 1000,
      B0 = (xy_coef_tib[1000, 2] - glm_xy_1$coef[1]),
      B1 = (xy_coef_tib[1000, 3] - glm_xy_1$coef[2]),
      B2 = (xy_coef_tib[1000, 4] - glm_xy_1$coef[3])
    )
  )


