library(ISLR2)
library(tidyverse)

# Ex. 8
# a)
college_ex <- read.csv('https://www.statlearning.com/s/College.csv')
# Alternatively, College dataset is part of ISLR2 package, so there is no need to load it using read.csv.
college_ex <- College

# b)
View(college_ex) # View the dataset
rownames(college_ex) <- college_ex[, 1] # Use first column as row names
View(college_ex) # View the dataset again
college_ex <- college_ex[, -1] # Remove first column
View(college_ex) # Voila

# c)
# i.
college_ex %>% summary()

# ii.
# Using ggplot2:
college_ex %>%
  GGally::ggpairs(., columns = 1:10)

# iii.
# Using ggplot2:
college_ex %>%
  ggplot(., aes(x = Private, y = Outstate)) +
  geom_boxplot() +
  labs(
    title = 'Out-of-state tuition vs type of university',
    subtitle = 'Breakdown by private and public universities',
    x = 'Private university',
    y = 'Out-of-state tuition in $',
    caption = 'Source: College dataset, own study'
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# iv.
# Same as in exercise description, but using mutate:
college_ex <- college_ex %>%
  mutate(
    Elite = cut(
      Top10perc,
      breaks = c(-Inf, 50, Inf),
      labels = c('No', 'Yes')
    )
  )

college_ex %>% summary() # 78 elite universities

college_ex %>%
  ggplot(., aes(x = Elite, y = Outstate)) +
  geom_boxplot() +
  labs(
    title = 'Out-of-state tuition vs type of university',
    subtitle = 'Breakdown by elite and non-elite universities',
    x = 'Elite university',
    y = 'Out-of-state tuition in $',
    caption = 'Source: College dataset, own study'
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# v.
college_ex %>%
  ggplot(., aes(x = Apps)) +
  geom_histogram(bins = 10) +
  labs(
    title = 'Number of applications received',
    x = 'Applications received',
    y = 'Number of universities',
    caption = 'Source: College dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  theme_bw()
# Most universities received less than 10k applications. What about breakdown by public/private universities?
college_ex %>%
  ggplot(., aes(x = Apps, fill = Private)) +
  geom_histogram(bins = 20) +
  labs(
    title = 'Number of applications received',
    subtitle = 'Breakdown by private and public universities',
    x = 'Applications received',
    y = 'Number of universities',
    caption = 'Source: College dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  theme_bw()
# And elite vs non-elite?
college_ex %>%
  ggplot(., aes(x = Apps, fill = Elite)) +
  geom_histogram(bins = 20) +
  labs(
    title = 'Number of applications received',
    subtitle = 'Breakdown by elite and non-elite universities',
    x = 'Applications received',
    y = 'Number of universities',
    caption = 'Source: College dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  theme_bw()

# vi.
# Let's look at correlation between Outstate and other quantitative variables:
cor(college_ex[, c(2:8, 10:18)], college_ex$Outstate) %>% round(., 2)
# Instructional expenditure per student has the highest correlation value with out-of-state tuition (0.67).
# Room and board costs have slightly less correlation value (0.65).
# Let's plot some graphs:
college_ex %>%
  ggplot(., aes(x = Expend, y = Outstate)) +
  geom_point() +
  labs(
    title = 'Out-of-state tuition vs instructional expenditure per student',
    x = 'Instructional expenditure per student in $',
    y = 'Out-of-state tuition in $',
    caption = 'Source: College dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()
# Graph suggests logarithmic relationship between these variables. Let's see:
college_ex %>%
  ggplot(., aes(x = log(Expend), y = Outstate)) +
  geom_point() +
  labs(
    title = 'Out-of-state tuition vs instructional expenditure per student',
    subtitle = 'Using logarithmic expenditure',
    x = 'Instructional expenditure per student (log)',
    y = 'Out-of-state tuition in $',
    caption = 'Source: College dataset, own study'
  ) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()
# Yep, now it's more linear.

# Now Outstate vs Room.Board
college_ex %>%
  ggplot(., aes(x = Room.Board, y = Outstate)) +
  geom_point() +
  labs(
    title = 'Out-of-state tuition vs room and board costs',
    x = 'Room and board costs in $',
    y = 'Out-of-state tuition in $',
    caption = 'Source: College dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# 9.
auto_ex <- Auto
auto_ex %>% is.na() %>% sum() # There is no missing values

# a)
auto_ex %>% View()
# Name & origin are qualitative variables, the rest is quantitative.

# b)
auto_ex[, 1:7] %>% sapply(., range)

# c)
auto_ex[, 1:7] %>% sapply(., mean)
auto_ex[, 1:7] %>% sapply(., sd)

# d)
auto_ex[-c(10:85), 1:7] %>% sapply(., range)
# Range changes for
# mpg (min 9 -> 11)
# weight (1613-5140 -> 1649-4997)
# acceleration (min 8 -> 8.5)
auto_ex[-c(10:85), 1:7] %>% sapply(., mean)
auto_ex[-c(10:85), 1:7] %>% sapply(., sd)
# Of course there are some changes in mean and sd of each variable.

# e)
auto_ex %>%
  GGally::ggpairs(., columns = 1:7)

# Miles per gallon is most closely (and negatively) correlated with engine displacement and vehicle weight. This shouldn't be a surprise - heavier vehicles with bigger engines should consume more fuel.

auto_ex %>%
  ggplot(., aes(x = displacement, y = mpg)) +
  geom_point() +
  labs(
    title = 'Miles per gallon vs engine displacement',
    x = 'Engine displacement in cubic inches',
    y = 'Miles per gallon',
    caption = 'Source: Auto dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

auto_ex %>%
  ggplot(., aes(x = weight, y = mpg)) +
  geom_point() +
  labs(
    title = 'Miles per gallon vs vehicle weight',
    x = 'Vehicle weight in lbs',
    y = 'Miles per gallon',
    caption = 'Source: Auto dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# We can also look at mpg vs displacement with breakdown by number of cylinders:

auto_ex %>%
  ggplot(., aes(x = displacement, y = mpg)) +
  geom_point(aes(color = factor(cylinders))) +
  scale_colour_discrete(name = 'No of cylinders') +
  labs(
    title = 'Miles per gallon vs engine displacement',
    subtitle = 'Breakdown by number of cylinders',
    x = 'Engine displacement in cubic inches',
    y = 'Miles per gallon',
    caption = 'Source: Auto dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# We can see that smaller engines has less cylinders.

# f)
# Once again:
auto_ex %>%
  GGally::ggpairs(., columns = 1:7)
# Miles per gallon is most closely (and negatively) correlated with engine displacement and vehicle weight, then with number of cylinders and engine horsepower. Acceleration rate and model year should have less (and positive) influence on vehicle range.

# 10.
# a)
boston_ex <- Boston
boston_ex %>% dim() # 506 observations, 13 variables
?Boston # Contains housing values in 506 suburbs of Boston describing general quality of life in given area.

# b)
boston_ex %>%
  GGally::ggpairs()
# Looking at medv (median value of houses) should be most interesting. It's most closely and negatively correlated with lstat (lower status of the population) and positively with rm (average number of rooms per dwelling). It seems that bigger houses, with more rooms, and in better districts, tends to be more expensive.

# c) 
# crim (per capita crime rate) is mostly correlated (positively) with rad (index of accessibility to radial highways) and tax (full-value property-tax rate). 

# d)
boston_ex %>%
  ggplot(., aes(x = crim)) +
  geom_histogram(bins = 20) +
  labs(
    title = 'Per capita crime rate by town',
    x = 'Per capita crime rate',
    y = 'Number of towns',
    caption = 'Source: Boston dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  theme_bw()

# There is a number of towns with high crime rate:
boston_ex %>%
  filter(., crim > mean(crim) + 2 * sd(crim)) %>%
  nrow()
# 16 towns have crime rate higher than 95% of suburbs. There are also towns with extremely high crime rate per capita:
boston_ex$crim %>% summary()
# Range of crime rate goes from ~0 to almost 90, while mean value is 3.61.

boston_ex %>%
  ggplot(., aes(x = tax)) +
  geom_histogram(bins = 20) +
  labs(
    title = 'Full-value property-tax rate per $10,000',
    x = 'Property-tax',
    y = 'Number of towns',
    caption = 'Source: Boston dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  theme_bw()

boston_ex %>%
  filter(., tax > mean(tax) + 2 * sd(tax)) %>%
  nrow()
# There are no towns with property-tax higher than 95% of other suburbs.
boston_ex$tax %>% summary()
# Range of tax goes from 187$ to 711$, while mean value is 408.2$

boston_ex %>%
  ggplot(., aes(x = ptratio)) +
  geom_histogram(bins = 20) +
  labs(
    title = 'Pupil-teacher ratio by town',
    x = 'Pupil-teacher ratio',
    y = 'Number of towns',
    caption = 'Source: Boston dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  theme_bw()

boston_ex %>%
  filter(., ptratio > mean(ptratio) + 2 * sd(ptratio)) %>%
  nrow()
# There are no towns with pupil-teacher ratio higher than 95% of other suburbs.
boston_ex$ptratio %>% summary()
# Range of ratio goes from 12.6 to 22, while mean value is 18.46. It would rather suggest, that there is number of towns with low pupil-teacher ratio.
boston_ex %>%
  filter(., ptratio < mean(ptratio) - 2 * sd(ptratio)) %>%
  nrow()
# There are 16 suburbs with lower pupil-teacher ratio than 95% of other towns.

# e)
boston_ex %>%
  filter(., chas == 1) %>%
  nrow()
# 35 tracts bound Charles River.

# f)
boston_ex$ptratio %>% median()
# Median pupil-teacher ratio for whole dataset is 19.05.

# g)
boston_ex %>%
  filter(., medv == min(medv))
boston_ex %>% summary()
# There are two towns with median value of house worth 5000$. Both has exceptionally high crime rate per capita.
# There are no residential land zoned for lots over 25k sq.ft. in these towns.
# Proportion of non-retail business acres in these towns is at the level of 3rd quantile of whole dataset.
# These towns aren't bound to Charles River.
# These towns have nitrogen oxides concentration higher than 3rd quantile of whole dataset.
# Average number of rooms per dwelling is lower than 1st quantile of whole dataset.
# All owner-occupied units in these town were built prior to 1940.
# These towns are pretty close to five Boston employment centres.
# They have also the highest index of accessibility to radial highways.
# Both towns have the same property-tax rate and pupil-teacher ratio, in both cases equal 3rd quantile of whole dataset.
# Both towns have high proportion of lower status of the population.

# h)
boston_ex %>%
  filter(., rm > 7) %>%
  nrow()
# 64 towns have on average more than 7 room per dwelling.

boston_ex %>%
  filter(., rm > 8)
# 13 towns have on average more than 8 room per dwelling.
# Generally, they have relatively low crime rate, average nox concentration and more than 70% of owner-occupied units were built before 1940 in most cases. Theye have high median value and low percentage of lower status population.












