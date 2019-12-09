library(rstanarm)
library(brms)
library(lme4)
data(cbpp)

n_iters <- 1e3

# arm is from rstanarm, brm is from brms
# 1 is where the size parameter is in the RHS, 2 is when it is not

fit1_arm <- stan_glmer(cbind(incidence, size - incidence) ~ size + period + (1|herd),
                      data = cbpp, family = binomial,
                      chains = 2, cores = 1, seed = 12345, iter = n_iters)
fit2_arm <- stan_glmer(cbind(incidence, size - incidence) ~ period + (1|herd),
                      data = cbpp, family = binomial,
                      chains = 2, cores = 1, seed = 12345, iter = n_iters)


fit1_brm <- brms::brm(incidence | trials(size) ~ size + period + (1|herd),
                      data = cbpp, family = binomial,
                      chains = 2, cores = 1, seed = 12345, iter = n_iters)
fit2_brm <- brms::brm(incidence | trials(size) ~ period + (1|herd),
                      data = cbpp, family = binomial,
                      chains = 2, cores = 1, seed = 12345, iter = n_iters)

broom::tidy(fit1_arm) %>%  filter(term == "(Intercept)")
broom::tidy(fit2_arm) %>%  filter(term == "(Intercept)")
broom::tidy(fit1_brm) %>%  filter(term == "b_Intercept")
broom::tidy(fit2_brm) %>%  filter(term == "b_Intercept")


t_brm_start <- Sys.time()
pred_brm <- predict(fit1_brm, newdata = cbpp)
t_brm_end <- Sys.time()

t_arm_start <- Sys.time()
pred_arm <- posterior_predict(fit1_brm, newdata = cbpp)
t_arm_end <- Sys.time()

t_brm_end - t_brm_start
t_arm_end - t_arm_start