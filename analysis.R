log_pageviews <- avg_daily_traffic %>%
  dplyr::select(-c(views, pages)) %>%
  tidyr::spread(period, logViews, fill = 0) %>%
  dplyr::left_join(dplyr::select(meta_data, language, n_articles), by = "language") %>%
  dplyr::mutate(log_articles = log(n_articles)) %>%
  dplyr::group_by(language) %>%
  dplyr::mutate(keep = all(c("control", "treatment") %in% test_group)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(keep) %>%
  identity

# sign test:
avg_daily_traffic %>%
  dplyr::select(-c(logViews, pages)) %>%
  tidyr::spread(period, views, fill = 0) %>%
  dplyr::group_by(language, test_group) %>%
  # adding 1 to both to adjust for cases where average pageviews per day was 0:
  dplyr::transmute(ratio = (`after deployment` + 1) / (`before deployment` + 1)) %>%
  dplyr::ungroup() %>%
  tidyr::spread(test_group, ratio) %>%
  dplyr::mutate(better = treatment > control) %>%
  dplyr::count(better) # FALSE: 108, TRUE: 161
binom.test(x = 159, n = 269, p = 0.5, alternative = "greater")
# p-value = 0.0015

length(unique(log_pageviews$language))
nrow(log_pageviews)

# fixed effects model:

fit <- lm(`after deployment` ~ `before deployment` + test_group, data = log_pageviews)
res <- residuals(fit)

partial_lm <- function(group) {
  log_pageviews <- log_pageviews[log_pageviews$test_group == group, ]
  fit <- lm(`after deployment` ~ `before deployment`, data = log_pageviews)
  return(fit)
}
control_fit <- partial_lm("control")
treatment_fit <- partial_lm("treatment")

s <- list(overall = fit, control = control_fit, treatment = treatment_fit) %>%
  purrr::map(summary)

par(mfrow = c(3, 1))
plot(
  log_pageviews$`before deployment`, log_pageviews$`after deployment`,
  xlab = "before deployment", ylab = "after deployment",
  main = "log average pageviews per day", pch = 16
)
title(sub = sprintf("R2: %.3f", s$overall$r.squared))
abline(fit)
plot(
  log_pageviews$`before deployment`[log_pageviews$test_group == "control"],
  log_pageviews$`after deployment`[log_pageviews$test_group == "control"],
  pch = 16, col = "blue", xlab = "before deployment", ylab = "after deployment",
  main = "log average pageviews per day"
)
abline(control_fit, col = "blue")
points(
  log_pageviews$`before deployment`[log_pageviews$test_group == "treatment"],
  log_pageviews$`after deployment`[log_pageviews$test_group == "treatment"],
  pch = 16, col = "red"
)
abline(treatment_fit, col = "red")
legend(
  "topleft", c("control", "treatment"), bty = "n",
  pch = 16, col = c("blue", "red"), lty = "solid"
)
title(sub = sprintf("R2 (control): %.3f, R2 (treatment): %.3f",
                    s$control$r.squared, s$treatment$r.squared))
hist(res, xlab = "Residual value from model", main = "Distribution of residuals")

par(mfrow = c(2, 2))
plot(fit)
summary(fit)

# mixed effects model with random intercepts:

library(lme4)

mix_fit <- lmer(
  `after deployment` ~ `before deployment` + test_group + (1|language),
  data = log_pageviews
)
# summary(mix_fit); fixef(mix_fit); ranef(mix_fit)
car::deltaMethod(mix_fit, "exp(test_grouptreatment)")
broom::tidy(mix_fit, conf.int = TRUE)

AIC(fit)
AIC(mix_fit)

# plot(mix_fit)

# fully bayesian multilevel model for confirmation:
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
mlm_spec <- "
data {
  int<lower=0> N;            // number of observations
  int<lower=0> J;            // number of languages
  int<lower=1,upper=J> j[N]; // language of i-th observation
  real y[N];                 // post-deployment average page views per day on log scale
  real x[N];                 // pre-deployment traffic on log scale
  int<lower=0,upper=1> T[N]; // indicator variable of treatment
}
parameters {
  real beta[2];           // beta[1] is beta_x, beta[2] is beta_T
  real<lower=0> sigma[2]; // sigma[1] is group std.dev, sigma[2] is individual std.dev
  real mu;
  real alpha_offset[J];
}
transformed parameters {
  real alpha[J];
  for (jj in 1:J) {
    alpha[jj] = mu + alpha_offset[jj];
  }
}
model {
  mu ~ normal(0, 10);
  beta ~ normal(0, 10);
  sigma ~ cauchy(0, 1);
  for (jj in 1:J) {
    alpha_offset[jj] ~ normal(0, sigma[1]);
  }
  for (i in 1:N) {
    y[i] ~ normal(alpha[j[i]] + beta[1] * x[i] + beta[2] * T[i], sigma[2]);
  }
}
generated quantities {
  real impact;
  impact = exp(beta[2]);
}
"

mlm_model <- stan_model(model_code = mlm_spec)
mlm_fit <- sampling(
  mlm_model,
  data = list(
    N = nrow(log_pageviews),
    J = length(unique(log_pageviews$language)),
    y = log_pageviews$`after deployment`,
    x = log_pageviews$`before deployment`,
    T = as.numeric(log_pageviews$test_group == "treatment"),
    j = as.numeric(factor(log_pageviews$language))
  ),
  pars = c("sigma", "beta", "mu", "alpha", "impact"),
  iter = 4000, control = list(max_treedepth = 15, adapt_delta = 0.85)
)
draws <- extract(mlm_fit)
broom::tidyMCMC(
  mlm_fit, pars = c("sigma", "mu", "beta", "impact"),
  conf.int = TRUE, conf.method = "HPDinterval", conf.level = 0.95
)
