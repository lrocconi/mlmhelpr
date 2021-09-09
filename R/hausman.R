#' Hausman Test
#'
#' @param x model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @description The Hausman test tests whether there are significant differences between fixed effect and random effect models with similar specifications. If there is a significant difference, a random effects models (i.e. a multilevel model) *may* be more suitable (efficient). This function takes a model estimated with `lme4::lmer`, automatically re-estimates a fixed effects model, applies the Hausman test, and returns the test statistic and p-value.
#'
#' The Hausman test is based on @fox2016, p. 732 (footnote 46). The Hausman test statistic is distributed as chi-square with degrees of freedom equal to the number of coefficients.
#'
#' **Note**: The selection of a mixed effect (random effect/multilevel) model should not be solely driven by the Hausman test or any other single statistic. Proper model selection should reflect the research questions and nested nature of the data. In addition, Fox suggests that "the choice between random and fixed effects should reflect our view of the process that generates the data" (p. 732). See also https://stats.stackexchange.com/questions/502811/should-a-hausman-test-be-used-to-decide-between-fixed-vs-random-effects for a discussion of the test and its results.
#'
#'
#' @return an object of class "htest"
#'
#'
#' @references{
#'   \insertRef{fox2016}{mlmhemlpr}
#' }
#'
#' @importFrom lme4 VarCorr fixef
#'
#' @examples
#' fit <- lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=T)
#'
#' hausman(fit)
#'

hausman <- function(re_model){

# re-estimate re_model ----

#get data
data <- re_model@frame

#get random effect names ----
  #get names
grps <- as.data.frame(lme4::VarCorr(re_model))[1]
  #remove residual
groups <- subset(grps, grps != "Residual")
  #add as.factor() for fe model
groups$grp <- paste0("as.factor(", groups$grp,")")
  #make it partially a formula
groups <- capture.output(cat(groups[,1], sep=" + "))

#get fixed effect names ----
  #grab names
fixed <- names(lme4::fixef(re_model))
  #remove intercept, if it exists
fixed <- fixed[!(fixed %in% "(Intercept)")]
  #remove duplicated names for factors
fixed <- sub("^(.*)\\1$", "\\1", fixed, perl = TRUE)
  #make it partially a formula
fixed <- capture.output(cat(fixed, sep=" + "))


# set intercept
intercept <- if(fixed[1] == "(Intercept)") {1} else {0}

#get dv
dv <- re_model@call[["formula"]][[2]]

#rebuild formula for fe model
fe_formula <- paste0(dv, " ~ ", capture.output(cat(intercept, fixed, groups, sep=" + ")))

#estimate model
fe_model <- lm(fe_formula, data=data)



# begin hausman test ----

fe_coef <- coef(fe_model)
re_coef <- lme4::fixef(re_model)
fe_vcov <- vcov(fe_model)
re_vcov <- vcov(re_model)
fe_names <- names(fe_coef)
re_names <- names(re_coef)
common_coef_names <- re_names[re_names %in% fe_names]
coefs <- common_coef_names[!(common_coef_names %in% "(Intercept)")] # drop intercept if included

betas <- fe_coef[coefs] - re_coef[coefs]
vcovs <- fe_vcov[coefs, coefs] - re_vcov[coefs, coefs]

z <- as.numeric(abs(t(betas) %*% solve(vcovs) %*% betas))
df <- length(betas)
p <- pchisq(z, df, lower.tail = FALSE)

#prep results
stat <- z
names(stat) <- "chi-square"
parameter <- df
names(parameter) <- "df"
alpha = .05

results <- list(statistic  = stat,
            p.value      = p,
            parameter    = parameter,
            method       = "Hausman Test",
            data.name    = "hsb"
            )
class(results) <- "htest" #Object of class "htest"

#caution: might have gotten this backwards!
message_text <- if(p < .05){
  "\n\nResults are significantly different. \nThe multilevel model may not be suitable."
} else {
  "\n\nResults are not significantly different. \nThe multilevel model may be more suitable."
}

message(message_text)
return(results)


}

# test----
load("misc/models.Rdata")

re_model1 <- lme4::lmer(mathach ~ 1 + ses + (1|id), data=hsb)
hausman(re_model1)

re_model2 <- lme4::lmer(mathach ~ 1 + ses + female + (1|id),
            data=hsb, REML=T)
hausman(re_model2)

re_model3 <- lme4::lmer(mathach ~ 1 + ses + female + (1|id) + (1|pracad),
                        data=hsb, REML=T)
hausman(re_model3)

#I'm not sure if random slopes are considered correctly in the function above- I have not tested it
re_model4 <- lme4::lmer(mathach ~ 1 + ses + female + (ses|id),
                        data=hsb, REML=T)
hausman(re_model4)#


