#' Plausible Values Range
#'
#' @param x model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param pct Percentile for the plausible value range, similar to a confidence interval. Must be specified as a whole number, e..g. 99, 95, 80, etc. The 95% value range is used by default.
#'
#' @description The plausible values range is useful for gauging the magniture of variation in the intercept. See @raudenbush2002, p. 71.
#'
#' @return A data frame specifying lower and upper bounds.
#'
#' @references{
#'   \insertRef{raudenbush2002}{mlmhemlpr}
#' }
#'
#' @examples
#' fit <- lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=T)
#'
#' plausible_values(fit) #default is 95% range
#' plausible_values(fit, 99)
#'
plausible_values <- function(x, pct=95){

#get Y00
intercept <- as.data.frame(x@beta)[1,]

#get CI
 #convert percentile to z-score
 if(pct < 1){stop("Percentiles should be written as whole numbers. Ex: 95, 99, 80, etc.")}

pct <- ((100-pct)/2)/100
sd <- qnorm(pct,lower.tail=FALSE)

#get T00
var_df <- as.data.frame(lme4::VarCorr(x))
variance <- subset(var_df, var1 == "(Intercept)")$sdcor

upper <- intercept + (sd*variance)
lower <- intercept - (sd*variance)



message("Plausible values range/predicitve interval:")
message(paste0("95% of values for the intercepts fall between ",
               round(lower,2), " and ", round(upper,2), "."))
data.frame("lower"=lower, "upper" = upper)
}
