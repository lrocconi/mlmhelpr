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


pve <- function(model1, model2, verbose=TRUE) {

  # TODO nesting test 1 - not sure if this is correct

  # I think this works. I checked the ANOVA.merMOD function in lme4 and they did
  # the same thing (i.e. check the nbos for each object)

  if(nobs(model1) != nobs(model2)){
    stop("Models were not all fitted to the same size of dataset. Models must be nested.")}

  # TODO do we need a three-level check?
  # Yes, we need to think about how to do this. We could use the following to check the number of random effect terms
  # lme4::getME(model, "k")
  # For 3-level models k will equal 2.
  # Another hiccup is how to deal with random coefficients. Maybe throw a warring or error?
  # We could also count the number of rows in as.data.frame(lme4::VarCorr(model))

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
    if(verbose == TRUE){
      cat("Proportion of variance explained at level-1 = ",
          round(pve_1,3), "\n")
      cat("Proportion of variance explained at level-2 = ",
          round(pve_2, 3))}
    # if verbose = F, just return dataframe of values
    if(verbose == FALSE){
      data.frame(level=c(1, 2),
                 variance_explained = c(pve_1, pve_2))
      }


}

load("misc/models.Rdata")

pve(model0_ml, model1_reml, F) # ok
pve(model0_ml, model18_ml, F) # not nested, returns error as expected
