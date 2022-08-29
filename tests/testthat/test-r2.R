#test models
fit_1 <- lme4::lmer(mathach ~ 1 + ses  + (1|id),
                  data=hsb, REML=T)

fit_2 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
                    data=hsb, REML=T)

fit_3 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1 + ses |id),
                    data=hsb, REML=F)

x <- r2(fit_1, fit_2)
round(x[1,2],3)

test_that("R^2 works", {
  expect_equal(round(x[1,2],3), 0.227)
  expect_equal(capture.output(r2(fit_1)), "The squared correlation between predicted and observed values for \"fit_1\" is 0.233")
})

