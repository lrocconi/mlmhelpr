#' ICC (Intraclass Correlation)
#'
#' @param x A model produced using the lme4::lmer() function. This is an object of class merMod and subclass lmerMod.
#'
#' @description
#'
#' @return A data frame with random effects and their intraclass correlations.
#'
#' @references
#'
#' @importFrom lme4 VarCorr
#' @export
#'
#' @examples



icc <- function(x) {

  #set up dfs for ICC
  varcorr_df <- as.data.frame(lme4::VarCorr(x))
  grps <- varcorr_df[,"grp"]
  j <- length(grps)
  grp <- vector()
  icc=NULL

  #calculate ICC
  for (i in 1:j) {
    grp[i] <- varcorr_df[i,"vcov"] / (sum(varcorr_df[,"vcov"]))
    icc <- rbind(icc, grp[i])
  }

  iccs <- (data.frame(grps, icc=round(icc,3)))
  # rownames(iccs) <- NULL
  return(iccs)

}

# Works for 2 and 3 level models (should work with any number of levels)

# Doesn't compute correctly for random-slopes. However, the ICC is weird for
# random-slopes, should we add a warning if random-slopes are included?
