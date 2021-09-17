#' Robust Standard Errors
#'
#' @param model model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param type character string specifying the estimation type. Options include "CR0", "CR1", "CR1p", "CR1S", "CR2", or "CR3". Defaults to "CR2". See details in clubSandwich package.
#'
#' @description Implements cluster-robust standard errors from the clubSanwich package.
#'
#' @return Dataframe and message indicating type of robust standard error requested
#'
#' @export
#'
#' @examples
#'
#'
robustSE <- function(model, type="CR2"){

  # Check whether clubSanwich package is installed
   if(!requireNamespace('clubSandwich')){
     stop("The 'clubSandwich' package must be installed to used this function.")}

  # Check whether model is of class lmerMod
  if(class(model)[1] != "lmerMod"){
    stop("Only models fitted using the `lmer` function are supported.")}

  # compute cluster robust standard errors from clubSandwich package
  covmat <- clubSandwich::vcovCR(model, type=type)
  #message(type, " correction used")
  cat(type, " correction used", "\n")
  clubSandwich::coef_test(model, vcov=covmat)

}

robustSE(model, type="CR1S")
robustSE(model10_ml)
robustSE(model14_reml)
robustSE(model16)
