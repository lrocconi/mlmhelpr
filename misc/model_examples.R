library(haven)
library(tidyverse)
library(lme4)
load("misc/models.Rdata") # loads everything below

# load high school and beyond data ---
hsb <- haven::read_spss("data-raw/HSB.sav")

# prep data
hsb <- hsb %>%
  mutate_at(vars(minority, female, catholic),
            as_factor)


# load nurses data ----

nurses <- haven::read_spss("data-raw/nurses.sav")

# prep data
nurses <- nurses %>%
  mutate_at(vars(expcon, specarewrd, hospsize),
            as_factor) %>%
  select(-13:-21)

# load gpa2 data ----

gpa <- haven::read_spss("data-raw/gpa2.sav")

# prep data

gpa <- gpa %>%
  mutate_at(vars(sex, admitted), as_factor) %>%
  pivot_longer(gpa1:gpa6, names_to="gpa_occasion",
               values_to="gpa") %>%
  group_by(student) %>%
  mutate(occas = row_number(), .before=1) %>%
  pivot_longer(job1:job6,  names_to="job_occasion",
               values_to="job") %>%
  distinct(student, occas, .keep_all=T)

# load UTHAI1 data for logistic regression ----

uthai <- haven::read_spss("data-raw/UTHAI1.sav")

# random intercept model examples ----

# Unconditional model
model0_ml <- lmer(mathach ~ 1 + (1|id), data=hsb, REML=F)
model0_reml <- lmer(mathach ~ 1 + (1|id), data=hsb, REML=T)

# Model 1 - level 1 variables
model1_ml <- lmer(mathach ~ 1 + ses + (1|id), data=hsb, REML=F)
model1_reml <- lmer(mathach ~ 1 + ses + (1|id), data=hsb, REML=T)


# Model 2 - level 2 variables
model2_ml <- lmer(mathach ~ 1 + ses + catholic + (1|id),
                  data=hsb, REML=F)
model2_reml <- lmer(mathach ~ 1 + ses + catholic + (1|id),
                    data=hsb, REML=T)

# random coefficient model examples ----

# Model 4: allow level 1 variable slope to vary
model4_ml <- lmer(mathach ~ 1 + ses + (1 + ses|id), data=hsb, REML=F)
model4_reml <- lmer(mathach ~ 1 + ses + (1 + ses|id), data=hsb, REML=T)

# Model 5 - cross-level interaction
model5_ml <- lmer(mathach ~ 1 + ses + catholic + ses:catholic +
                    (1 + ses|id), data=hsb, REML=F)

# note: does not converge
model5_reml <- lmer(mathach ~ 1 + ses + catholic + ses:catholic +
                    (1 + ses|id), data=hsb, REML=T)

# models with optimizers
model5_ml_allFit <- allFit(model5_ml)
model5_reml_allFit <- allFit(model5_reml)

# models with starting values

model6_ml <- update(model5_ml,
                     start=getME(model5_ml, c("theta", "fixef")),
                     control=lmerControl(optCtrl=list(maxfun=2e4)))

model6_reml <- update(model5_reml,
                    start=getME(model5_reml, c("theta", "fixef")),
                    control=lmerControl(optCtrl=list(maxfun=2e4)))


# three-level model ----

model7_ml <- lmer(stress ~ 1 + (1|hospital:ward) + (1|hospital),
                  nurses, REML=F)

model7_reml <- lmer(stress ~ 1 + (1|hospital:ward) + (1|hospital),
                    nurses, REML=T)

# Include random effect for treatment condition
# note: convergence error
model8_ml <- lmer(stress ~ 1 + expcon + (1|hospital:ward) +
                    (1+expcon|hospital), nurses, REML=F)
# no error
model8_reml <- lmer(stress ~ 1 + expcon + (1|hospital:ward) +
                    (1+expcon|hospital), nurses, REML=T)

# longitudinal models ----

# Unconditional means model
model9_ml <- lmer(gpa ~ 1  + (1|student), data=gpa, REML=F)
model9_reml <- lmer(gpa ~ 1  + (1|student), data=gpa, REML=T)

# unconditional linear growth model (non-random)
model10_ml <- lmer(gpa ~ 1 + occas + (1|student), data=gpa, REML=F)
model10_reml <- lmer(gpa ~ 1 + occas + (1|student), data=gpa, REML=T)

# unconditional quadractic growth
model11_ml <- lmer(gpa ~ 1 + occas + I(occas^2) +
                 (1|student), data=gpa, REML=F)
model11_reml <- lmer(gpa ~ 1 + occas + I(occas^2) +
                     (1|student), data=gpa, REML=T)

# unconditional cubic growth
model12_ml <- lmer(gpa ~ 1 + occas + I(occas^3) +
                     (1|student), data=gpa, REML=F)
model12_reml <- lmer(gpa ~ 1 + occas + I(occas^3) +
                       (1|student), data=gpa, REML=T)

# unconditional linear growth model (RANDOM)
model13_ml <- lmer(gpa ~ 1 + occas + (1 + occas|student), data=gpa, REML=F)
model13_reml <- lmer(gpa ~ 1 + occas + (1 + occas|student), data=gpa, REML=T)

# model with cross-level interactions
model14_ml <- lmer(gpa ~ 1 + occas + job + sex + highgpa + sex:occas + highgpa:occas + (1 + occas|student), data=gpa, REML=F)
model14_reml <- lmer(gpa ~ 1 + occas + job + sex + highgpa + sex:occas + highgpa:occas + (1 + occas|student), data=gpa, REML=T)

# correlated random effects

model15_ml <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), sleepstudy, REML=F)
model15_reml <- lmer(Reaction ~ 1 + Days + (1 + Days|Subject), sleepstudy, REML=T)

# logistic models ----
# Unconditional Model

# note: failed convergence
model16 <- glmer(rep1 ~ 1 + (1|schoolid), family=binomial, data=uthai)
model17 <- glmer(rep1 ~ 1 + (1|schoolid), family=binomial, data=uthai, nAGQ = 10)

# cross classified models ----

# simple cross-classified from Bates
model18_ml <- lmer(diameter ~ 1 + (1|plate) + (1|sample), Penicillin, REML = F)
model18_reml <- lmer(diameter ~ 1 + (1|plate) + (1|sample), Penicillin, REML = T)

# partial crossed from Bates

model19_ml <- lmer(y ~ 1 + (1|s) + (1|d)+(1|dept:service), InstEval, REML=F)
model19_reml <- lmer(y ~ 1 + (1|s) + (1|d)+(1|dept:service), InstEval, REML=T)

save.image(file="models.Rdata")
