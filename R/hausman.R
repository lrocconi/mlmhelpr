load("misc/models.Rdata")

# specifying fixed effect model
model1_ols <- lm(mathach ~ 1 + ses + as.factor(id), data=hsb)

# random effects model
model1_ml


#coefficients (b_fe - be_re)
coef_diff <- coef(model1_ols)[2] - lme4::fixef(model1_ml)[2]

#variance (var(b_fe) - var(be_re))
var_diff = vcov(model1_ols)[2,2] - vcov(model1_ml)[2,2]

# Fox formula, pg. 732
z2 <- coef_diff^2/var_diff

pchisq(z2, df = 1, lower.tail = FALSE)

# another method from https://stat.ethz.ch/pipermail/r-help/2011-January/265671.html - I followed this, but got the same result as above

#testing using hausman test from plm

#fixed effect model
fe <- plm::plm(mathach ~ 1 + ses, index="id", data=hsb, model="within") #note same estimate for model1_ols ses

re <- plm::plm(mathach ~ 1 + ses, index="id", data=hsb, model="random")

plm::phtest(fe, re)



