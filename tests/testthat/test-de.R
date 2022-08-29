#examples
fit_lmer1 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
            data=hsb, REML=T)

fit_lmer2 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1 + ses|id),
                        data=hsb, REML=T)


hsb$binary_math <- ifelse(hsb$mathach <= 13, 0, 1)
fit_glmer <- lme4::glmer(binary_math ~ 1 + ses + catholic + (1|id),
                          data=hsb, family = binomial(link="logit"))

fit_lm <- lm(mathach ~ 1 + ses + catholic,
                       data=hsb)

# making sure output is the same each time
df1 <- design_effect(fit_lmer1)
df2 <- design_effect(fit_lmer1)

df_m1 <- design_effect(fit_lmer1, median = T)
df_m2 <- design_effect(fit_lmer1, median = T)

test_that("design_effect works", {
  # check that they are the same
  expect_equal(df1, df2)
  expect_equal(df_m1, df_m2)
  # check they are different
  expect_false(isTRUE(all.equal(df1, df_m1)))
  # error messages
  expect_error(design_effect(fit_lm))
  expect_message(design_effect(fit_lmer2))
  expect_error(design_effect(fit_glmer))
})


