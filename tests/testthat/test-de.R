#examples
fit_lmer1 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
            data=hsb, REML=T)

fit_lmer2 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id) + (catholic|ses),
                        data=hsb, REML=T)


fit_lmer3 <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id) + (catholic|ses) + (1|ses:id),
                        data=hsb, REML=T)


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
  expect_message(design_effect(fit_lmer3))
})


