fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
            data=hsb, REML=T)


test_that("icc works", {
  expect_equal(icc(fit)[1,2], .09)
  expect_equal(icc(fit)[2,2], .91)
  expect_equal(icc(model1_ml)[1,2], .113) # null model
  expect_equal(icc(model4_ml)[3,2], -.007) # random slope
  expect_equal(icc(model7_ml)[1,2], .513) # three level
  expect_equal(icc(model16)[1,2], .328) # logistic
  #messages
  expect_message(icc(model4_ml))
})



