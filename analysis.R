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
