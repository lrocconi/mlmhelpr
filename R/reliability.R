#' Calculate reliability coefficients for random effects
#'
#' @param model A model produced using the `lme4::lmer()` or `lme4::glmer()` functions. This is an object of class `merMod` and subclass `lmerMod` or `glmerMod`.
#'
#' @description This function computes reliability coefficients for random effects according to Raudenbush and Bryk (2002) and Snijders and Bosker (2012). The reliability coefficient is equal to the proportion of between group variance to total variance: \eqn{\frac{\tau^2}{\tau^2 + {\frac{\sigma^2}{n_j}}}}.
#' The empirical Bayes estimator for the random effect is a weighted combination of the cluster mean and grand mean with the weight given by the reliability of the random effect. We refer to this as a reliability because in classical test theory the ratio of the true score variance, \eqn{\tau^2}, relative to the observed score variance of the sample mean is a reliability.
#' A reliability close to 1 puts more weight on the cluster mean while a reliability close to 0 put more weight on the grand mean.
#'
#' @return A list with reliability estimates for each random effect
#'
#' @references{
#'   \insertRef{snijders2012}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{raudenbush2002}{mlmhelpr}
#' }
#'
#' @importFrom lme4 lmer
#'
#' @export
#'
#' @examples
#'
#' # lmer model
#' fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1 + ses|id),
#' data=hsb, REML=TRUE)
#'
#' reliability(fit)
#'
#'
reliability <- function(model){

# extract the variance-covariance matrix
varcorr_df <- as.data.frame(lme4::VarCorr(model))

# this creates full grp names ----
varcorr_df$name <- ifelse(is.na(varcorr_df$var1),
                          varcorr_df$grp,
                          paste0(varcorr_df$grp, " ",
                                 varcorr_df$var1))

varcorr_df$name <- ifelse(is.na(varcorr_df$var2),
                          varcorr_df$name,
                          paste0(varcorr_df$grp, " ",
                                 varcorr_df$var1, " ",
                                 varcorr_df$var2))

# this corrects for inclusion of random slopes ----
varcorr_df$value <- ifelse(is.na(varcorr_df$var2),
                           varcorr_df$vcov,
                           varcorr_df$vcov*2)

# removes slope-intercept covariance
varcorr_df <- varcorr_df[is.na(varcorr_df$var2),]

# if glmer, check link function
if(lme4::getME(model, "devcomp")$dims[["GLMM"]] == 1 & stats::family(model)$link != "logit")
{stop("Only glmer models with logit link functions supported")}


# OLD formula was appropriate for intercept only models [i.e., tau / (tau + sigma2 / nj)].
# Revised formula uses EB estimates [reliability = 1 - average_posterior_variance / random_effect_variance]

# Get Emprical Bayes estimates
re <- lme4::ranef(model, condVar = TRUE)

# loop over each grouping factor and compute reliability
means <- lapply(names(re), function(g){

  # random effects for grouping factor g
  re_g <- re[[g]]

  # posterior variance array
  post_var <- attr(re_g, "postVar")

  # variance-covariance matrix for grouping factor g
  tau_mat <- as.matrix(lme4::VarCorr(model)[[g]])

  # compute reliability for each random coefficient
  rel <- rep(NA_real_, ncol(re_g))

  for(i in seq_along(rel)){

    tau_i <- tau_mat[colnames(re_g)[i], colnames(re_g)[i]]

    rel[i] <- 1 - mean(post_var[i, i, ], na.rm = TRUE) / tau_i
  }

  names(rel) <- paste(g, colnames(re_g))

  return(rel)
})

names(means) <- names(re)

return(means)


}


