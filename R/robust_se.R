#' Robust Standard Errors
#'
#' @param model model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param type character string specifying the estimation type. Options include "CR0", "CR1", "CR1p", "CR1S", "CR2", or "CR3". Defaults to "CR2". See details in clubSandwich::vcovCR.
#'
#' @description Implements cluster-robust standard errors from the clubSandwich package.
#'
#' @return Data frame and message indicating type of robust standard error requested.
#'
#' @importFrom clubSandwich vcovCR coef_test
#'
#'
#'
#' @examples
#'
#' fit <- lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=T)
#'
#' robust_se(fit)
#' robust_se(fit, type="CR1S")
#'
#'
robust_se <- function(model, type="CR2"){

  # Check whether clubSandwich package is installed
   if(!requireNamespace('clubSandwich')){
     stop("The 'clubSandwich' package must be installed to use this function.")}

  # Check whether model is of class lmerMod
  if(class(model)[1] != "lmerMod"){
    stop("Only models fitted using the `lmer` function are supported.")}

  # compute cluster robust standard errors from clubSandwich package
  covmat <- clubSandwich::vcovCR(model, type=type)
  #message(type, " correction used")
  cat(type, " correction used", "\n")
  clubSandwich::coef_test(model, vcov=covmat)

}

