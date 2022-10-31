#' Robust Standard Errors
#'
#' @param model model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param type character string specifying the estimation type. Options include "CR0", "CR1", "CR1p", "CR1S", "CR2", or "CR3". Defaults to "CR2". See details in `clubSandwich::vcovCR`.
#'
#' @param pct percentage level for confidence interval. Defaults to 95. Must be specified as a whole number between 1 and 100 (e.g., 99, 95, 80).
#'
#' @description Implements cluster-robust standard errors from the `clubSandwich` package. The `clubSandwich` package is required to use this function. See `mlmhelpr::boot_se` for an alternative.
#'
#' @return Data frame and message indicating type of robust standard error requested.
#'
#' @references{
#'   \insertRef{clubSandwich}{mlmhelpr}
#' }
#'
#' @export
#'
#' @examples
#'
#'\donttest{
#'
#' # run time > 5s
#' fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=TRUE)
#'
#' robust_se(fit)
#' }
#'
#'
robust_se <- function(model, type="CR2", pct = 95){

  if(pct < 1){stop("Percentiles should be written as whole numbers (e.g., 95, 99, 80)")}
  if(pct >= 100){stop("Percentiles should be less than 100 (e.g., 95, 99, 80)")}

   `%notin%` <- Negate(`%in%`)

  #don't require type to be case sensitive
  type <- toupper(type)

  # Check whether clubSandwich package is installed
   if(!requireNamespace('clubSandwich')){
     stop("The 'clubSandwich' package must be installed to use this function.")}

  # Check whether model is of class lmerMod
  if(class(model)[1] != "lmerMod" & class(model)[1] != "lmerModLmerTest"){
    stop("Only models fitted using the `lmer` function are supported. See `mlmhelpr::boot_se` for additional options.")}

  # Check whether "type" is correctly specified
  types <- c("CR0", "CR1", "CR1p", "CR1S", "CR2", "CR3")

  if(type %notin% types){
    stop("Estimation type must be one of CR0, CR1, CR1p, CR1S, CR2, CR3.\nLeaving this argument blank defaults to CR2.\nSee ?clubSandwich::vcovCR for more information")}

  # compute cluster robust standard errors from clubSandwich package
  covmat <- clubSandwich::vcovCR(model, type=type)

  #message(type, " correction used")

  # combine CI with parameter estimates
  est <- clubSandwich::coef_test(model, vcov=covmat)

  pct2 <- pct/100

  ci <- clubSandwich::conf_int(model, vcov=covmat, level = pct2)
  #ci <- ci[,c(1, 5, 6)]
  ci <- ci[,c("Coef", "CI_L", "CI_U")]

  lower_name <- paste0("lower_", pct, "%_CI")
  upper_name <- paste0("upper_", pct, "%_CI")

  colnames(ci) <- c("Coef", lower_name, upper_name)

  coef <- merge(est, ci, by = "Coef")

  output <- list(correction = type, coef = coef)

  return(output)
}

