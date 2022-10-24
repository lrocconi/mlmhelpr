#' Bootstrap Standard Errors (experimental)
#'
#' @param model a mixed model produced using the `lme4` package (`lmer` or `glmer` functions). This is an object of class `merMod`. This function is a wrapper for `lme4::bootMer`
#'
#' @param nsim number of bootstrap samples to compute. Defaults to 5 but should be closer to 1,000 or 5,000. Note this is time intensive.
#'
#' @param seed random number seed for reproducibility. Defaults to 1234.
#'
#' @param pct percentage level for confidence interval. Defaults to 95.
#'
#' @param ... additional parameters to pass to `lme4::bootMer`. Not currently implemented.
#'
#' @description Computes bootstrapped standard errors for fixed effects.
#'
#' @return Data frame and message indicating number of bootstrapped samples.
#'
#' @importFrom lme4 bootMer
#'
#' @export
#'
#' @examples
#'
#' fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=TRUE)
#'
#' boot_se(fit)
#'
#'
boot_se <- function(model, nsim = 5, seed = 1234, pct = 95, ...){

  if(pct < 1){stop("Percentiles should be written as whole numbers (e.g., 95, 99, 80)")}
  if(pct >= 100){stop("Percentiles should be less than 100 (e.g., 95, 99, 80)")}

  boot_mod <- lme4::bootMer(model, FUN = lme4::fixef, nsim = nsim, seed = seed, .progress = "txt")
  boot_df <- boot_mod$t
  boot_sd <- as.data.frame(apply(boot_df, 2, stats::sd))
  fixed_effects <- as.data.frame(boot_mod$t0)
  df <- merge(x=fixed_effects, y=boot_sd, by = "row.names")
  colnames(df) <- c("variable", "estimate", "boot se")
  df$Z = df$estimate / df$`boot se`
  df$'Pr(>|z|)' =  2*stats::pnorm(-abs(df$Z))
  df$" " = ifelse(df$'Pr(>|z|)' < .001, "***", ifelse(df$'Pr(>|z|)' < .01, "**", ifelse(df$'Pr(>|z|)' < .05, "*","")))

  # CIs
  lower_tail <- ((100-pct)/2)/100
  upper_tail <- 1 - lower_tail

  boot_lowerCI <- as.data.frame(apply(boot_df, 2, function (x) stats::quantile(x, lower_tail)))
  boot_upperCI <- as.data.frame(apply(boot_df, 2, function (x) stats::quantile(x, upper_tail)))
  boot_CI <- merge(boot_lowerCI, boot_upperCI, by = "row.names")

  lower_name <- paste0("lower ", pct, "% CI")
  upper_name <- paste0("upper ", pct, "% CI")

  colnames(boot_CI) <- c("variable", lower_name, upper_name)

  df2 <- merge(df, boot_CI, by = "variable")
  df2 <- df2[, c("variable", "estimate", lower_name, upper_name, "boot se", "Z", "Pr(>|z|)", " ")]


  if(nsim < 1000)
  {message("\n\n", "Warning: Number of boostrapped samples is low. Consider increasing `nsim` to 1000 or larger")}

  cat("\n", "Parametric Bootstrapped Standard Errors","\n","Number of Bootstraps =",nsim, "\n", "z-test returned using a standard normal reference distribution (interpret with caution)", "\n\n")
  return(df2)

}


