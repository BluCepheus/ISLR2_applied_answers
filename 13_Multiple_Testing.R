library(ISLR2)
library(tidyverse)

# Ex. 7.
carseats_ex <- Carseats

# a)
model_coefs <- c(
  summary(lm(Sales ~ CompPrice, data = carseats_ex))$coef,
  summary(lm(Sales ~ Income, data = carseats_ex))$coef,
  summary(lm(Sales ~ Advertising, data = carseats_ex))$coef,
  summary(lm(Sales ~ Population, data = carseats_ex))$coef,
  summary(lm(Sales ~ Price, data = carseats_ex))$coef,
  summary(lm(Sales ~ Age, data = carseats_ex))$coef,
  summary(lm(Sales ~ Education, data = carseats_ex))$coef
)

tibble(
  Variable = c('CompPrice', 'Income', 'Advertising', 'Population', 'Price',
               'Age', 'Education'),
  Coef = model_coefs[seq(2, 56, 8)],
  PValue = model_coefs[seq(8, 56, 8)],
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Coef == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Coef == 0 & PValue <= .05 ~ 'Type I Error',
    Coef != 0 & PValue > .05 ~ 'Type II Error',
    Coef != 0 & PValue <= .05 ~ 'Correct',
    Coef == 0 & PValue > .05 ~ 'Correct'
  )
) -> result_tib

table(result_tib$Results, result_tib$Truth)

# b)
# At level a = 0.05 we can reject null hypothesis for Income, Advertising, Price and Age.

# c)
# Bonferroni method
tibble(
  Variable = c('CompPrice', 'Income', 'Advertising', 'Population', 'Price',
               'Age', 'Education'),
  Coef = model_coefs[seq(2, 56, 8)],
  PValue = p.adjust(model_coefs[seq(8, 56, 8)], method = 'bonferroni'),
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Coef == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Coef == 0 & PValue <= .05 ~ 'Type I Error',
    Coef != 0 & PValue > .05 ~ 'Type II Error',
    Coef != 0 & PValue <= .05 ~ 'Correct',
    Coef == 0 & PValue > .05 ~ 'Correct'
  )
) -> result_tib_2

table(result_tib_2$Results, result_tib_2$Truth)
# At level a = 0.05 we can reject null hypothesis for Income, Advertising, Price and Age.

# Holm's method
tibble(
  Variable = c('CompPrice', 'Income', 'Advertising', 'Population', 'Price',
               'Age', 'Education'),
  Coef = model_coefs[seq(2, 56, 8)],
  PValue = p.adjust(model_coefs[seq(8, 56, 8)], method = 'holm'),
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Coef == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Coef == 0 & PValue <= .05 ~ 'Type I Error',
    Coef != 0 & PValue > .05 ~ 'Type II Error',
    Coef != 0 & PValue <= .05 ~ 'Correct',
    Coef == 0 & PValue > .05 ~ 'Correct'
  )
) -> result_tib_3

table(result_tib_3$Results, result_tib_3$Truth)
# At level a = 0.05 we can reject null hypothesis for Income, Advertising, Price and Age.

# d)
# Benjamini-Hochberg procedure
tibble(
  Variable = c('CompPrice', 'Income', 'Advertising', 'Population', 'Price',
               'Age', 'Education'),
  Coef = model_coefs[seq(2, 56, 8)],
  PValue = p.adjust(model_coefs[seq(8, 56, 8)], method = 'BH'),
  Results = ifelse(PValue <= .2, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Coef == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Coef == 0 & PValue <= .2 ~ 'Type I Error',
    Coef != 0 & PValue > .2 ~ 'Type II Error',
    Coef != 0 & PValue <= .2 ~ 'Correct',
    Coef == 0 & PValue > .2 ~ 'Correct'
  )
) -> result_tib_4

table(result_tib_4$Results, result_tib_4$Truth)
# At level a = 0.2 we can reject null hypothesis for Income, Advertising, Price and Age.

# Ex. 8.
set.seed(1)
x <- matrix(rnorm(20 * 100), ncol = 100)

# a)
p_values = rep(NA, 100)
for (i in 1:100) {
  t.test(x[, i], mu = 0)$p.value -> p_values[i]
}

fund_tib <- tibble(
  Manager = 1:100,
  Mean = apply(x, 2, mean),
  PValue = p_values,
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Mean == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Mean == 0 & PValue <= .05 ~ 'Type I Error',
    Mean != 0 & PValue > .05 ~ 'Type II Error',
    Mean != 0 & PValue <= .05 ~ 'Correct',
    Mean == 0 & PValue > .05 ~ 'Correct'
  )
)

fund_tib %>%
  ggplot(., aes(x = PValue)) +
  geom_histogram(bins = 10) +
  labs(
    title = 'P-value histogram',
    subtitle = 'For t-test with mean = 0',
    x = 'P-value',
    y = 'Count',
    caption = 'Random dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(breaks = 1:20) +
  theme_bw()

# b)
table(fund_tib$Results, fund_tib$Truth)
# 4 null hypotheses rejected.

# c)
# Bonferroni method
fund_tib_2 <- tibble(
  Manager = 1:100,
  Mean = apply(x, 2, mean),
  PValue = p.adjust(p_values, method = 'bonferroni'),
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Mean == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Mean == 0 & PValue <= .05 ~ 'Type I Error',
    Mean != 0 & PValue > .05 ~ 'Type II Error',
    Mean != 0 & PValue <= .05 ~ 'Correct',
    Mean == 0 & PValue > .05 ~ 'Correct'
  )
)
table(fund_tib_2$Results, fund_tib_2$Truth)
# No null hypotheses rejected.

# Holm's method
fund_tib_3 <- tibble(
  Manager = 1:100,
  Mean = apply(x, 2, mean),
  PValue = p.adjust(p_values, method = 'holm'),
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Mean == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Mean == 0 & PValue <= .05 ~ 'Type I Error',
    Mean != 0 & PValue > .05 ~ 'Type II Error',
    Mean != 0 & PValue <= .05 ~ 'Correct',
    Mean == 0 & PValue > .05 ~ 'Correct'
  )
)
table(fund_tib_3$Results, fund_tib_3$Truth)
# No null hypotheses rejected.

# d)
# Benjamini-Hochberg procedure
fund_tib_4 <- tibble(
  Manager = 1:100,
  Mean = apply(x, 2, mean),
  PValue = p.adjust(p_values, method = 'BH'),
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Mean == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Mean == 0 & PValue <= .05 ~ 'Type I Error',
    Mean != 0 & PValue > .05 ~ 'Type II Error',
    Mean != 0 & PValue <= .05 ~ 'Correct',
    Mean == 0 & PValue > .05 ~ 'Correct'
  )
)
table(fund_tib_4$Results, fund_tib_4$Truth)
# No null hypotheses rejected.

# e)
fund_tib[, 1:2] %>%
  arrange(desc(Mean)) %>%
  slice(1:10) ->
  best_managers

x[, best_managers$Manager] -> best_x

p_values = rep(NA, 10)
for (i in 1:10) {
  t.test(best_x[, i], mu = 0)$p.value -> p_values[i]
}

fund_tib_5 <- tibble(
  Manager = best_managers$Manager,
  Mean = apply(best_x, 2, mean),
  PValue = p.adjust(p_values, method = 'bonferroni'),
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Mean == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Mean == 0 & PValue <= .05 ~ 'Type I Error',
    Mean != 0 & PValue > .05 ~ 'Type II Error',
    Mean != 0 & PValue <= .05 ~ 'Correct',
    Mean == 0 & PValue > .05 ~ 'Correct'
  )
)
table(fund_tib_5$Results, fund_tib_5$Truth)
# 1 null hypothesis rejected.

# Holm's method
fund_tib_6 <- tibble(
  Manager = best_managers$Manager,
  Mean = apply(best_x, 2, mean),
  PValue = p.adjust(p_values, method = 'holm'),
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Mean == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Mean == 0 & PValue <= .05 ~ 'Type I Error',
    Mean != 0 & PValue > .05 ~ 'Type II Error',
    Mean != 0 & PValue <= .05 ~ 'Correct',
    Mean == 0 & PValue > .05 ~ 'Correct'
  )
)
table(fund_tib_6$Results, fund_tib_6$Truth)
# 1 null hypothesis rejected.

# Benjamini-Hochberg procedure
fund_tib_7 <- tibble(
  Manager = best_managers$Manager,
  Mean = apply(best_x, 2, mean),
  PValue = p.adjust(p_values, method = 'BH'),
  Results = ifelse(PValue <= .05, 'Reject H0', 'Do not reject H0'),
  Truth = ifelse(Mean == 0, 'H0 is true', 'H0 is false'),
  Correctnes = case_when(
    Mean == 0 & PValue <= .05 ~ 'Type I Error',
    Mean != 0 & PValue > .05 ~ 'Type II Error',
    Mean != 0 & PValue <= .05 ~ 'Correct',
    Mean == 0 & PValue > .05 ~ 'Correct'
  )
)
table(fund_tib_7$Results, fund_tib_7$Truth)
# 1 null hypothesis rejected.

