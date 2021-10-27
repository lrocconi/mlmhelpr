library(mlmhelpr)

#test models
fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
                  data=hsb, REML=T)

fit_2 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id) + (ses | catholic),
                    data=hsb, REML=F)

test_that("R^2 works", {
  #verbose = F
  expect_equal(round(r2(fit, F),3), 0.232)
  expect_equal(round(r2(fit_2, F),3), 0.236)
  # default, verbose = T
  expect_equal(capture.output(r2(fit)), "The squared correlation between predicted and observed values is 0.232")
})



