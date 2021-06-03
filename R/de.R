de <- function(x) {
  1 + ((nrow(x@frame)/as.data.frame(lme4::getME(x,"l_i"))[1,1]) - 1) *
    (as.data.frame(lme4::VarCorr(x))[1,"vcov"] / (sum(as.data.frame(lme4::VarCorr(x))[,"vcov"])))
}

# only works for the intercept