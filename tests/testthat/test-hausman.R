fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
data=hsb, REML=T)

tmp <- hausman(fit)

test_that("hausman works", {
  expect_message(hausman(fit))
  expect_equal(as.numeric(round(hausman(fit)$statistic,3)),
               265.978)
})

