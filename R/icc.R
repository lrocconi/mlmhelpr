# icc
# intra-class correlation

icc <- function(x) {
  grps <- as.data.frame(lme4::VarCorr(x))[,"grp"]
  j <- length(grps)
  grp <- vector()
  icc=NULL
  for (i in 1:j) {
    grp[i] <- as.data.frame(lme4::VarCorr(x))[i,"vcov"] / (sum(as.data.frame(lme4::VarCorr(x))[,"vcov"]))
    icc <- rbind(icc, grp[i])
  }

  iccs <- (data.frame(grps, icc=round(icc,3)))
 # rownames(iccs) <- NULL
  return(iccs)

}



# Works for 2 and 3 level models (should work with any number of levels)

# Doesn't compute correctly for random-slopes. However, the ICC is weird for
# random-slopes, should we add a warning if random-slopes are included?


