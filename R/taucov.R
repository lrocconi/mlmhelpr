#' Tau Covariance
#'
#' @description Quickly get the covariance and correlation between intercepts and slopes. By default, `lme4` only displays the correlation.
#'
#' @param model
#'
#' @return A data frame with the intercept, randomly-varying variable, covariance, and correlation.
#'
#' @importFrom lme4 VarCorr
#'
#' @examples
#'
#' fit <- lme4::lmer(mathach ~ 1 + ses + (1 + ses|id), data=hsb, REML=T)
#'
#' taucov(fit)
#'
taucov <- function(model) {

  # get variance components
  var_df <- as.data.frame(lme4::VarCorr(model))
  var_df <- na.omit(var_df)[2:5]



  if(nrow(var_df) == 0) {
    stop("No random slopes detected.")
  } else {


  # rename columns
  names(var_df)[3] <- "covariance"
  names(var_df)[4] <- "correlation"
  # remove parentheses
  var_df$var1 <- gsub("[()]", "", var_df$var1)
  return(var_df)
  }
}
