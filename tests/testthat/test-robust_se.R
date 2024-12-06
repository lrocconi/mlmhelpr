fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
data=hsb, REML=T)

fit_lm <- lm(mathach ~ 1 + ses + catholic, data=hsb)

cr2 <- robust_se(fit)
cr1 <- robust_se(fit, "CR1")
cr1_case <- robust_se(fit, "cr1")

test_that("robust se works", {
  expect_equal(round(cr2$coef[1,7],3),
               11.269)
  expect_equal(cr1, cr1_case)
  expect_false(isTRUE(all.equal(cr2, cr1)))
  expect_error(robust_se(fit_lm))
  expect_error(robust_se(fit, "ce1"))

})


