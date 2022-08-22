#test model
fit <- lmer(mathach ~ 1 + ses + catholic + (1|id),
data=hsb, REML=T)

x <- r2_pseudo(fit, FALSE)
r2_pseudo(fit)

test_that("R^2_pseudo works", {
  # not verbose
  expect_equal(round(x,3), .232)
  # verbose
  expect_equal(capture.output(r2_pseudo(fit)), "The squared correlation between predicted and observed values is 0.232")
})

