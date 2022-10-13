library(ISLR2)
library(tidyverse)
library(survival)

# Ex. 10.
bc_ex <- BrainCancer

# a)
bc_ex %>%
  survfit(
    Surv(time, status) ~ 1,
    data = .
  ) -> survfit_bc_1

tibble(
  time = survfit_bc_1$time,
  surv = survfit_bc_1$surv,
  lower = survfit_bc_1$surv - survfit_bc_1$std.err,
  upper = survfit_bc_1$surv + survfit_bc_1$std.err
) %>%
  ggplot(., aes(x = time, y = surv)) +
  geom_line() +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    fill = 'blue',
    alpha = .3
  ) +
  labs(
    title = 'Estimated probability of survival vs months',
    subtitle = 'with +-1 SE bands',
    x = 'Months',
    y = 'Probability of survival',
    caption = 'Brain Cancer dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  theme_bw()

# b)
boot_fun <- function(dataset, index) {
  dataset %>%
    survfit(
      Surv(time, status) ~ 1,
      data = .
    ) -> temp_model
  temp_model$std.err
}
boot::boot(bc_ex, boot_fun, 200) -> boot_survfit_bc_1
boot_survfit_bc_1$t0 - survfit_bc_1$std.err
# The results are exactly the same, as in both cases we used full dataset.

# c)
bc_ex %>%
  survival::coxph(
    survival::Surv(time, status) ~ ., data = .
  ) -> coxph_bc_1
coxph_bc_1 %>% summary()
# Diagnosis HG glioma coeff should be interpreted that the risk associated with HG glioma is exp(2.15) ~ 8.62 times higher than risk associated with meningioma (baseline of diagnosis)
# Karnofsky index coeff is negative, hence larger values of index are associated with lower risk, i.e. longer survival.
# Other coefficients are not statistically significant.

# d)
bc_ex %>%
  survfit(
    Surv(time, status) ~ factor(ki),
    data = .
  ) -> survfit_bc_2

tibble(
  time = survfit_bc_2$time,
  surv = survfit_bc_2$surv,
  ki = c(
    rep('60', survfit_bc_2$n[2]),
    rep('70', survfit_bc_2$n[3]),
    rep('80', survfit_bc_2$n[4]),
    rep('90', survfit_bc_2$n[5]),
    rep('100', survfit_bc_2$n[6])
  )
) %>%
  ggplot(., aes(x = time, y = surv, color = ki)) +
  geom_line() +
  labs(
    title = 'Estimated probability of survival vs months',
    subtitle = 'breakdown by Karnofsky index',
    x = 'Months',
    y = 'Probability of survival',
    caption = 'Brain Cancer dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  scale_color_manual(
    name = 'Karnofsky index',
    values = c('red', 'blue', 'green', 'yellow', 'brown'),
    labels = c('60', '70', '80', '90', '100')
  ) +
  theme_bw()

# Ex. 11.
ex_tib <- tibble(
  Observation_Y = c(26.5, 37.2, 57.3, 90.8, 20.2, 89.8),
  Censoring_Indicator_D = c(1, 1, 1, 0, 0, 0),
  Covariate_X = c(.1, 11, -.3, 2.8, 1.8, .4),
  Group = ifelse(Covariate_X < 2, 'A', 'B')
)

# a)

ex_tib %>%
  survfit(
    Surv(Observation_Y, Censoring_Indicator_D) ~ Group,
    data = .
  ) -> survfit_ex_1

tibble(
  time = survfit_ex_1$time,
  surv = survfit_ex_1$surv,
  group = c(
    rep('A', 4),
    rep('B', 2)
  )
) %>%
  ggplot(., aes(x = time, y = surv, color = group)) +
  geom_line() +
  labs(
    title = 'Estimated probability of survival vs time in days',
    subtitle = 'breakdown by group',
    x = 'Time in days',
    y = 'Probability of survival',
    caption = 'Ex. 11. dataset, own study'
  ) +
  scale_x_continuous(labels = scales::number) +
  scale_y_continuous(labels = scales::number) +
  scale_color_manual(
    name = 'Group',
    values = c('red', 'blue'),
    labels = c('A', 'B')
  ) +
  theme_bw()

# Probability of survival in group A decreases from 1 to 0.33 in 57 days and stays at this level. Probability of survival in group B is equal 0.5 regardless of time.

# b)
ex_tib %>%
  survival::coxph(
    survival::Surv(Observation_Y, Censoring_Indicator_D) ~ Group, data = .
  ) -> coxph_ex_1
coxph_ex_1 %>% summary()
# p-value = 0.8, indicating no evidence of a difference in survival between the two groups.
# Group A is a baseline, p-value for group B coefficient is 0.783, so it's not statistically significant.
# Group B coefficient is negative, hence larger values of index are associated with lower risk, i.e. longer survival.

# c)
ex_tib %>%
  survival::survdiff(
    survival::Surv(Observation_Y, Censoring_Indicator_D) ~ Group, data = .
  ) -> lrt_ex_1
lrt_ex_1
# p-value = 0.8, indicating no evidence of a difference in survival between the two groups. It's the same as in Cox's proportional hazards model.












