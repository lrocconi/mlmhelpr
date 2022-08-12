
fit1 <- lme4::lmer(mathach ~ 1 + (1|id), data=hsb, REML=F)
fit2 <- lme4::lmer(mathach ~ 1 + ses + (1|id), data=hsb, REML=F)

fit3 <- lme4::lmer(mathach ~ 1 + (1|id), data=hsb, REML=T)
fit4 <- lme4::lmer(mathach ~ 1 + ses + (1|id), data=hsb, REML=F)

fit5 <- lme4::lmer(stress ~ 1 + expcon + (1|hospital:ward) +
                    (1+expcon|hospital), nurses, REML=F)
fit6 <- lme4::lmer(stress ~ 1 + (1|hospital:ward) + (1|hospital),
                  nurses, REML=F)

test_that("multiplication works", {
  expect_equal(round(pve(fit, fit2)[1,2],3),
               0.447)
  #ml + reml
  expect_message(pve(fit3, fit4))
  #not nested
  expect_error(pve(fit1, fit5))
  #three level
  expect_equal(round(pve(fit6, fit5)[3,2],5),
               7e-05)
})
