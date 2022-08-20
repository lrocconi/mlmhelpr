fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
data=hsb, REML=T)


test_that("plausible values works", {
  expect_equal(round(plausible_values(fit)[1,3],3), 7.956)
  expect_equal(round(plausible_values(fit,.99)[1,3],3), 6.774)
  # test messages
  expect_error(plausible_values(fit, pct=95))
})
