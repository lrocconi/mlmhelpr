#test model
fit <- lmer(mathach ~ 1 + ses + catholic + (1|id),
data=hsb, REML=T)

x <- r2_cor(fit, FALSE)

test_that("R^2_cor works", {
  # not verbose
  expect_equal(round(x,3), .232)
  # verbose
  expect_message(r2_cor(fit, verbose = TRUE))
})

