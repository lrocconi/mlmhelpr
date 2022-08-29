fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
data=hsb, REML=T)

fit2 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1 + ses|id),
                  data=hsb, REML=T)

test_that("hausman works", {
  expect_message(hausman(fit))
  expect_equal(as.numeric(round(hausman(fit)$statistic,3)),
               265.978)
  expect_warning(hausman(fit2))
})

