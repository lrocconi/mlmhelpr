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
df1 <- de(fit_lmer1)
df2 <- de(fit_lmer1)

df_m1 <- de(fit_lmer1, median = T)
df_m2 <- de(fit_lmer1, median = T)

test_that("de works", {
  # check that they are the same
  expect_equal(df1, df2)
  expect_equal(df_m1, df_m2)
  # check they are different
  expect_false(isTRUE(all.equal(df1, df_m1)))
  # error messages
  expect_error(de(fit_lm))
  expect_message(de(fit_lmer2))
  expect_message(de(fit_lmer3))
})


