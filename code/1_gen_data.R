rm(list = ls())

library(tidyverse)
set.seed(14)

# two period ====
b0 <- 1
b1 <- 1
b2 <- 1
b3 <- 1
b4 <- 1
b5 <- 1
b6 <- 1
b7 <- 1

sigma <- 1

n_obs_a <- 25
n_obs_b <- 25
prob_treated_a <- 0.5
prob_treated_b <- 0.5

treat <- c(rbinom(n_obs_a, 1, prob_treated_a), rbinom(n_obs_b, 1, prob_treated_b))
placebo <- c(rep(0, n_obs_a), rep(1, n_obs_b))
post <- rep(c(0,1), n_obs_a+n_obs_b)

eps <- rnorm((n_obs_a+n_obs_b)*2, 0, sigma)

df <- tibble(
  treat = rep(treat, each=2),
  placebo = rep(placebo, each=2),
  post = post,
  eps = eps
) %>% 
  mutate(
    y = b0 + b1*treat + b2*placebo + b3*post + b4*treat*placebo + b5*treat*post + b6*placebo*post + b7*treat*placebo*post + eps
  )

write.csv(df, "data/two_period_data.csv", row.names = FALSE)

# fixed effect ====
b0 <- 1
b1 <- 1
b2 <- 1
b3 <- 1
b4 <- 1
b5 <- 1
b6 <- 1
b7 <- 1

sigma <- 1

n_obs_a <- 25
n_obs_b <- 25
prob_treated_a <- 0.5
prob_treated_b <- 0.5

df <- tibble(
  id = rep(1:(n_obs_a+n_obs_b), each=2),
  id_fixed = rep(rnorm(n_obs_a+n_obs_b, 0, sigma), each=2),
  treat = rep(c(rbinom(n_obs_a, 1, prob_treated_a), rbinom(n_obs_b, 1, prob_treated_b)), each=2),
  placebo = rep(c(rep(0, n_obs_a), rep(1, n_obs_b)), each=2),
  post = rep(c(0,1), n_obs_a+n_obs_b),
) %>% 
  mutate(
    y = id_fixed + b1*treat + b2*placebo + b3*post + b4*treat*placebo + b5*treat*post + b6*placebo*post + b7*treat*placebo*post
  )

write.csv(df, "data/fixed_effect_data.csv", row.names = FALSE)

# event study ====
## hold parallel trend assumption ====
b1 <- 1
b2 <- 1
b4 <- 1
b5 <- 1
b6 <- 1
b7 <- 1

sigma <- 1

n_obs_a <- 25
n_obs_b <- 25
prob_treated_a <- 0.5
prob_treated_b <- 0.5

total_periods <- 7

df <- tibble(
  id = rep(1:(n_obs_a+n_obs_b), each=total_periods),
  id_fixed = rep(rnorm(n_obs_a+n_obs_b, 0, sigma), each=total_periods),
  treat = rep(c(rbinom(n_obs_a, 1, prob_treated_a), rbinom(n_obs_b, 1, prob_treated_b)), each=total_periods),
  placebo = rep(c(rep(0, n_obs_a), rep(1, n_obs_b)), each=total_periods),
  period = rep(-(total_periods%/%2):(total_periods%/%2), n_obs_a+n_obs_b),
  post = rep(c(rep(0, total_periods%/%2+1), rep(1, total_periods%/%2)), n_obs_a+n_obs_b)
) %>% 
  left_join(
    tibble(
      period = -(total_periods%/%2):(total_periods%/%2),
      period_fixed = rnorm(total_periods, 0, sigma)
    ),
    by = "period"
  ) %>%
  mutate(
    y = id_fixed + b1*treat + b2*placebo + period_fixed + b4*treat*placebo + b5*treat*post + b6*placebo*post + b7*treat*placebo*post + id_fixed
  )

write.csv(df, "data/event_study_parallel_data.csv", row.names = FALSE)

## no parallel trend assumption ====
b1 <- 1
b2 <- 1
b4 <- 1
b5 <- 1
b6 <- 1
b7 <- 1

parallel_noize <- 0.5

sigma <- 1

n_obs_a <- 25
n_obs_b <- 25
prob_treated_a <- 0.5
prob_treated_b <- 0.5

total_periods <- 7

df <- tibble(
  id = rep(1:(n_obs_a+n_obs_b), each=total_periods),
  id_fixed = rep(rnorm(n_obs_a+n_obs_b, 0, sigma), each=total_periods),
  treat = rep(c(rbinom(n_obs_a, 1, prob_treated_a), rbinom(n_obs_b, 1, prob_treated_b)), each=total_periods),
  placebo = rep(c(rep(0, n_obs_a), rep(1, n_obs_b)), each=total_periods),
  period = rep(-(total_periods%/%2):(total_periods%/%2), n_obs_a+n_obs_b),
  post = rep(c(rep(0, total_periods%/%2+1), rep(1, total_periods%/%2)), n_obs_a+n_obs_b)
) %>% 
  left_join(
    tibble(
      period = -(total_periods%/%2):(total_periods%/%2),
      period_fixed = rnorm(total_periods, 0, sigma)
    ),
    by = "period"
  ) %>%
  mutate(
    y = id_fixed + b1*treat + b2*placebo + period_fixed + b4*treat*placebo + b5*treat*post + b6*placebo*post + b7*treat*placebo*post + id_fixed + rnorm(n_obs_a+n_obs_b, 0, sigma) + parallel_noize*treat*post
  )

write.csv(df, "data/event_study_no_parallel_data.csv", row.names = FALSE)

