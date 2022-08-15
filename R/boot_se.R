#' Bootstrap Standard Errors (experimental)
#'
#' @param model a mixed model produced using the `lme4` package (`lmer` or `glmer` functions). This is an object of class `merMod`. This function is a wrapper for `lme4::bootMer`
#'
#' @param nsim number of bootstrap samples to compute. Defaults to 5 but should be closer to 1,000 or 5,000. Note this is time intensive.
#'
#' @param seed random number seed for reproducibility. Defaults to 1234.
#'
#' @param ... additional parameters to pass to `lme4::bootMer`. Not currently implemented.
#'
#' @description Computes bootstrapped standard errors for fixed effects.
#'
#' @return Data frame and message indicating number of bootstrapped samples.
#'
#' @importFrom lme4::bootMer
#'
#'
#'
#' @examples
#'
#' # Logistic Example
#'   # Create binary outcome
#' hsb$binary_math <- ifelse(hsb$mathach <= 13, 0, 1)
#'
#' fitb <- glmer(binary_math ~ 1 + ses + catholic + (1|id),
#' data=hsb, family = binomial(link="logit"))
#'
#' boot_se(fitb)
#'
#'
boot_se <- function(model, nsim = 5, seed = 1234, ...){

  boot_mod <- lme4::bootMer(model, FUN = lme4::fixef, nsim = nsim, seed = seed, .progress = "txt")
  boot_df <- boot_mod$t
  boot_sd <- as.data.frame(apply(boot_df, 2, sd))
  fixed_effects <- as.data.frame(boot_mod$t0)
  df <- merge(x=fixed_effects, y=boot_sd, by = "row.names")
  colnames(df) <- c("variable", "estimate", "boot se")
  df$Z = df$estimate / df$`boot se`
  df$'Pr(>|z|)' =  2*pnorm(-abs(df$Z))
  df$" " = ifelse(df$'Pr(>|z|)' < .001, "***", ifelse(df$'Pr(>|z|)' < .01, "**", ifelse(df$'Pr(>|z|)' < .05, "*","")))

  #add boostrap CIs later

  cat("\n\n", "Parametric Bootstrapped Standard Errors","\n","Number of Bootstraps =",nsim, "\n", "z-test returned using a standard normal reference distribution (interpret with caution)", "\n\n")
  return(df)

}

#boot_se(fitb)



