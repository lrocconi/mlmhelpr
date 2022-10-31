#' Pseudo R-squared: Squared correlation between predicted and observed values
#'
#' @param x A model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param verbose If true, prints an explanatory message, "The squared correlation between predicted and observed values is...". If false (default), returns a value.
#'
#' @description The `r2_cor` function estimates a pseudo R-squared by correlating predicted \eqn{\hat{Y}} values and observed \eqn{Y} values. This pseudo R-squared is similar to the \eqn{R^2} used in OLS regression. It indicates amount of variation in the outcome that is explained by the model (Peugh, 2010; Singer & Willett, 2003, p. 36).
#'
#' @return If `verbose == TRUE`, a console message. If `verbose == FALSE` (default), a numeric value.
#'
#'
#' @references{
#'   \insertRef{peugh2010}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{singer2003}{mlmhelpr}
#' }
#'
#' @importFrom lme4 getME
#'
#' @export
#'
#' @examples
#'
#' fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=TRUE)
#'
#' # returns a numeric value
#' r2_cor(fit)
#'
#' # returns a console message with the r2 value
#' r2_cor(fit, verbose = TRUE)


r2_cor <- function(x, verbose=FALSE) {

  r2 <- (stats::cor(stats::predict(x), lme4::getME(x, "y")))^2

  # return message
  if(verbose == TRUE){
  return(message(c("The squared correlation between predicted and observed values is ", round(r2,3)),sep=""))}
  # if verbose = F, just return value
  if(verbose == FALSE){
  print(r2)}

}


