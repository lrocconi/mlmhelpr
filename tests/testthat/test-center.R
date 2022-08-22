#original
fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
data=hsb, REML=T)

#grand-mean centered via subtraction
hsb2 <- hsb
hsb2$ses_grand <- hsb$ses - mean(hsb$ses)

fit_c <- lme4::lmer(mathach ~ 1 + ses_grand + catholic + (1|id),
                  data=hsb2, REML=T)

# grand-mean centered via `scale`
hsb3 <- hsb
hsb3$ses_grand <- scale(hsb3$ses, scale=F)

fit_sc <- lme4::lmer(mathach ~ 1 + ses_grand + catholic + (1|id),
                    data=hsb3, REML=T)

# function used
fit_gmc <- center(fit, grand_variables ="ses")


test_that("center via grand mean works", {
  #grand mean centering
  expect_equal(lme4::fixef(fit_gmc),
               lme4::fixef(fit_c))
  expect_equal(lme4::fixef(fit_gmc),
               lme4::fixef(fit_sc))
  #group mean centering
})


