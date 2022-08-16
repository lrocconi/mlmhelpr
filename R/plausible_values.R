#' Plausible Values Range / Random Effect Confidence Intervals
#'
#' @param x model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param pct Percentile for the plausible value range, similar to a confidence interval. Must be specified as a whole number (e.g., 99, 95, 80). The 95% value range is used by default.
#'
#' @description The plausible values range is useful for gauging the magnitude of variation around fixed effects. See @raudenbush2002, p. 71 and @hoffman2015, p. 166.
#'
#' @return A data frame specifying lower and upper bounds for each fixed effect.
#'
#' @references{
#'   \insertRef{hoffman2015}{mlmhemlpr}
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

  #get CI
  #convert percentile to z-score
  if(pct < 1){stop("Percentiles should be written as whole numbers (e.g., 95, 99, 80)")}
  if(pct >= 100){stop("Percentiles should be less than 100 (e.g., 95, 99, 80)")}

  tail <- ((100-pct)/2)/100
  sd <- qnorm(tail,lower.tail=FALSE)

  #get T00
  var_df <- as.data.frame(lme4::VarCorr(x))
  variance <- subset(var_df, var1 == "(Intercept)")$sdcor

  #get random effects
  re_df <- as.data.frame(lme4::VarCorr(x))
  #remove residual
  re_df <- subset(re_df, grp != "Residual")
  #remove covariance
  re_df <- subset(re_df, is.na(var2))

  #get fixed effects
  fe_df <- as.data.frame(lme4::fixef(x), optional=TRUE)
  fe_df_rows <- rownames(fe_df)
  fe_df <- cbind(fe_df_rows, fe_df)
  names(fe_df) <- c("fe_df_rows", "fixed_effect")

  # combine fe and re into one df

  pv_df <- merge(re_df, fe_df, by.x = "var1", by.y="fe_df_rows")

  # calculations
  pv_df$upper_ci <- pv_df$fixed_effect + (sd*pv_df$sdcor)
  pv_df$lower_ci <- pv_df$fixed_effect - (sd*pv_df$sdcor)

  cat(pct, "% plausible value interval: \n\n", sep = "")
  return(pv_df[c("grp", "var1", "lower_ci", "fixed_effect", "upper_ci")])
}

