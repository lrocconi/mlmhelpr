load("misc/models.Rdata")

# simulate a two-level model - https://github.com/debruine/faux

# note - open and run the ICC function from icc.R

twolevel_fun = function(sub_sd = 1, item_sd=2, error_sd=3) {
  set.seed(123)
  dat <- faux::sim_mixed_cc(
    sub_n = 100,  # subject sample size
    item_n = 50,  # item sample size
    grand_i = 10, # overall mean of the score
    sub_sd = sub_sd,   # SD of subject random intercepts
    item_sd = item_sd,  # SD of item random intercepts
    error_sd = error_sd  # SD of residual error
  )

  #estimate models
  fe_model <<- lm(y ~ 1 + as.factor(item_id), data=dat)
  re_model <<- lme4::lmer(y ~ 1 + (1|item_id), data = dat)


  #coefficients (b_fe - be_re)
  coef_diff <- coef(fe_model)[1] - lme4::fixef(re_model)[1]
  coef_diff <- as.vector(coef_diff)
  #variance (var(b_fe) - var(be_re))
  var_diff = vcov(fe_model)[1,1] - vcov(re_model)[1,1]

  # Fox formula, pg. 732
  z2 <- coef_diff^2/var_diff

  p <- pchisq(abs(z2), df = 1, lower.tail = FALSE)

  res <- list(
    p.value      = p,
    parameter    = z2,
    method       = "Hausman Test")
  class(res) <- "htest"
  return(res)
  intraclass <- icc(re_model)
  print(intraclass)

}

# high individual level variation
twolevel_fun(sub_sd = 100, item_sd=10, error_sd=1)
icc(re_model)

# low individual level variation
twolevel_fun(sub_sd = 1, item_sd=10, error_sd=1)
icc(re_model)

# Do the Hausman test results make sense?

# specifying fixed effect model
model1_ols <- lm(mathach ~ 1 + ses + as.factor(id), data=hsb)

# Centering SES and computing an average ses for each school (meanses)
hsb$meanses <- ave(hsb$ses, hsb$id)
hsb$ses.cwc <- hsb$ses - ave(hsb$ses, hsb$id)

# A simplied fixed-effect model using differencing
model1b_ols <- lm(mathach ~ 1 + ses.cwc, data=hsb)

# Fox p. 731 paragraph starting "Consider, however, the following, likely
# surprising, fact:" notes that you can also get the fixed effect estimate by
# including the group mean as a regressor. He states that "if our object is to
# estimate the individual-level coefficient beta_2 of X, controlling for all
# group-level differences, it suffices to control for the within- group means,
# X_bar_i. The test that beta_3, the coefficient of the compositional variable, is
# 0 is related to the so-called Hausman test." Below is that test.
model1c_ols <- lm(mathach ~ 1 + ses + meanses, data=hsb)
summary(model1c_ols)

# random effects model
model1_ml


#coefficients (b_fe - be_re)
(coef_diff <- coef(model1b_ols)[2] - lme4::fixef(model1_ml)[2])

#variance (var(b_fe) - var(be_re))
(var_diff = vcov(modelb1_ols)[2,2] - vcov(model1_ml)[2,2])

# Fox formula, pg. 732
(z2 <- coef_diff^2/var_diff)

pchisq(z2, df = 1, lower.tail = FALSE)

# I think this is working correctly. I think plm and Stata are using different/modified formulas
# and probably ones more related to panel/longitudinal data and Fox's is more general (?? I think??)

# another method from https://stat.ethz.ch/pipermail/r-help/2011-January/265671.html - I followed this, but got the same result as above

#testing using hausman test from plm

#fixed effect model
fe <- plm::plm(mathach ~ 1 + ses, index="id", data=hsb, model="within") #note same estimate for model1_ols ses

re <- plm::plm(mathach ~ 1 + ses, index="id", data=hsb, model="random")

plm::phtest(fe, re)

#plm::phtest(model1b_ols, re)

# plm::phtest and Stata hausman give different results (based on defaults)
# I think the difference has to do with how plm treats unbalanced data
# https://cran.rstudio.com/web/packages/plm/vignettes/B_plmFunction.html

# following phtest from source code ----
#from https://rdrr.io/cran/plm/src/R/test_general.R

  coef.wi <- coef(model1_ols)
  coef.re <- lme4::fixef(model1_reml)
  vcov.wi <- vcov(model1_ols)
  vcov.re <- vcov(model1_reml)
  names.wi <- names(coef.wi)
  names.re <- names(coef.re)
  common_coef_names <- names.re[names.re %in% names.wi]
  coef.h <- common_coef_names[!(common_coef_names %in% "(Intercept)")] # drop intercept if included (relevant when between model inputed)
  if(length(coef.h) == 0) stop("no common coefficients in models")
  dbeta <- coef.wi[coef.h] - coef.re[coef.h]
  df <- length(dbeta)
  dvcov <- vcov.wi[coef.h, coef.h] - vcov.re[coef.h, coef.h]

  stat <- as.numeric(abs(t(dbeta) %*% solve(dvcov) %*% dbeta))
  pval <- pchisq(stat, df = df, lower.tail = FALSE)

plm::phtest(fe, re)


