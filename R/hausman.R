load("misc/models.Rdata")


# specifying fixed effect model
model1_ols <- lm(mathach ~ 1 + ses + as.factor(id), data=hsb)

# random effects model
model1_reml


#coefficients (b_fe - be_re)
coef_diff <- coef(model1_ols)[2] - lme4::fixef(model1_reml)[2]

#variance (var(b_fe) - var(be_re))
var_diff = vcov(model1_ols)[2,2] - vcov(model1_reml)[2,2]

# Fox formula, pg. 732
z2 <- coef_diff^2/var_diff

pchisq(z2, df = 1, lower.tail = FALSE)

# another method from https://stat.ethz.ch/pipermail/r-help/2011-January/265671.html - I followed this, but got the same result as above

#testing using hausman test from plm

#fixed effect model
fe <- plm::plm(mathach ~ 1 + ses, index="id", data=hsb, model="within") #note same estimate for model1_ols ses

re <- plm::plm(mathach ~ 1 + ses, index="id", data=hsb, model="random")
plm::phtest(fe, model1_reml)

plm::phtest(fe, re, method="chisq")


cf_diff <- coef(model1_ols)[1:2] - lme4::fixef(model1_reml)
vc_diff <- vcov(model1_ols)[1:2,1:2] - vcov(model1_reml)
x2_diff <- as.vector(t(cf_diff) %*% solve(vc_diff) %*% cf_diff)
pchisq(x2_diff, df = 2, lower.tail = FALSE)

#from https://rdrr.io/cran/plm/src/R/test_general.R

phtest.panelmodel <- function(x, x2, ...){
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
