#' Plausible Values Range / Random Effect Confidence Intervals
#'
#' @param x model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param pct Percentile for the plausible value range, similar to a confidence interval. Must be specified as a whole number between 1 and 100 (e.g., 99, 95, 80). The 95% value range is used by default.
#'
#' @description The plausible values range is useful for gauging the magnitude of variation around fixed effects. For more information, see Raudenbush and Bryk (2002, p. 71) and Hoffman (2015, p. 166).
#'
#' @return A data frame specifying lower and upper bounds for each fixed effect.
#'
#' @references{
#'   \insertRef{hoffman2015}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{raudenbush2002}{mlmhelpr}
#' }
#'
#'@export
#'
#' @examples
#' fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=TRUE)
#'
#' plausible_values(fit) #default is 95% range
#' plausible_values(fit, 99)
#'
plausible_values <- function(x, pct=95){

  #get CI
  #convert percentile to z-score
  if(pct < 1){stop("Percentiles should be written as whole numbers (e.g., 95, 99, 80)")}
  if(pct >= 100){stop("Percentiles should be less than 100 (e.g., 95, 99, 80)")}

  tail <- ((1-(pct/100))/2)/1
  sd <- stats::qnorm(tail,lower.tail=FALSE)

  #get T00
  var_df <- as.data.frame(lme4::VarCorr(x))
  variance <- subset(var_df, var_df$var1 == "(Intercept)")$sdcor

  #get random effects
  re_df <- as.data.frame(lme4::VarCorr(x))
  #remove residual
  re_df <- subset(re_df, re_df$grp != "Residual")
  #remove covariance
  re_df <- subset(re_df, is.na(re_df$var2))

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

  #cat(pct, "% plausible value interval: \n\n", sep = "")
  pv <- pv_df[c("grp", "var1", "lower_ci", "fixed_effect", "upper_ci")]

  lower_name <- paste0("lower_", pct, "%_CI")
  upper_name <- paste0("upper_", pct, "%_CI")

  colnames(pv) <- c("grp", "var1", lower_name, "fixed_effect", upper_name)

  return(pv)
}

