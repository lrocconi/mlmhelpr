
fit1 <- lme4::lmer(mathach ~ 1 + (1|id), data=hsb, REML=F)
fit2 <- lme4::lmer(mathach ~ 1 + ses + (1|id), data=hsb, REML=F)

fit3 <- lme4::lmer(mathach ~ 1 + (1|id), data=hsb, REML=T)
fit4 <- lme4::lmer(mathach ~ 1 + ses + (1|id), data=hsb, REML=F)

fit5 <- lme4::lmer(mathach ~ 1 + ses + (1 + ses|id), data=hsb, REML=F)



test_that("multiplication works", {
  expect_equal(round(r2_pve(fit1, fit2)[1,2],3),
               0.447)
  #ml + reml
  expect_message(r2_pve(fit3, fit4))
  #not nested
  expect_error(r2_pve(fit4, fit5))


})
