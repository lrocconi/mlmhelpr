#' Proportion of variance explained
#'
#' @param model1 Previous model, produced using the `lme4::lmer()` function. Usually, this is the null or unconditional model.
#'
#' @param model2 Current model, produced using the `lme4::lmer()` function.
#'
#' @description `pve` calculates the proportional reduction in variance explained (PVE) by adding variables to a prior, nested model. The PVE is considered a local effect size estimate [@peugh2020, @raudenbush2002].
#'
#' @return Message (default) or data frame (with `verbose=F`)
#'
#' @references{
#'   \insertRef{peugh2010}{mlmhemlpr}
#'   \insertRef{raudenbush2002}{mlmhemlpr}
#' }
#'
#' @importFrom lme4 varCorr
#'
#' @export
#'
#' @examples
#'


pve <- function(model1, model2, verbose=T) {

  # TODO nesting test 1 - not sure if this is correct
  if(nobs(model1) != nobs(model2)){
    stop("Models were not all fitted to the same size of dataset. Models must be nested.")}

  # TODO do we need a three-level check?

  # level-2 variance explained ----
    # get variance components
    vc1_lvl2 <- as.data.frame(lme4::VarCorr(model1))$vcov[1]
    vc2_lvl2 <- as.data.frame(lme4::VarCorr(model2))$vcov[1]

    # calculation for level-2 pve
    pve_2 <- (vc1_lvl2 - vc2_lvl2) / vc1_lvl2

  # level-1 variance explained ----
    # get variance components
    vc1_lvl1 <- (summary(model1)$sigma)^2
    vc2_lvl1 <- (summary(model2)$sigma)^2

    # calculation for level-1 pve
    pve_1 <- (vc1_lvl1 - vc2_lvl1) / vc1_lvl1

    # return message by default
    if(verbose == T){
      cat("Proportion of variance explained at level-1 = ",
          round(pve_1,3), "\n")
      cat("Proportion of variance explained at level-2 = ",
          round(pve_2, 3))}
    # if verbose = F, just return dataframe of values
    if(verbose == F){
      data.frame(level=c(1, 2),
                 variance_explained = c(pve_1, pve_2))
      }


}

load("misc/models.Rdata")

pve(model0_ml, model1_ml, F) # ok
pve(model0_ml, model18_ml, F) # not nested, returns error as expected
