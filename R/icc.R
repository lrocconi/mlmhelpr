  #' Intraclass Correlation (ICC)
#'
#' @param x A model produced using the `lme4::lmer()` or `lme4::glmer()` functions. This is an object of class `merMod` and subclass `lmerMod` or `glmerMod`.
#'
#' @description The `icc` function calculates the intraclass correlation (ICC) for multilevel models. The ICC represents the proportion of group-level variance to total variance. The ICC can be calculated for two or more levels in random-intercept models [@hox2018].
#'
#' **Note**: For models with random slopes, it is generally advised to interpret with caution. According to Kreft and De Leeuw (1998)[@kreft1998], "The concept of intra-class correlation is based on a model with a random intercept only. No unique intra-class correlation can be calculated when a random slope is present in the model" (p. 63). However, Snijders and Bosker (2012) [@snijders2012] offer a calculation to derive this value (equation 7.9). This equation is implemented here.
#'
#' The `icc` function calculates the intraclass correlation for linear mixed-effects models estimated with the `lme4::lmer` function or generalized linear mixed-effect model estimated with the `lme4::glmer` function with `family = binomial(link="logit")`. For logistic models, the estimation method follows Hox et al. (2018, p. 107) recommendation of setting the level-1 residual variance to \eqn{\frac{\pi^2}{3}}. For a discussion different methods for estimating the intraclass correlation for binary responses see Wu et al. (2012) [@Wu2012].
#'
#'
#' @return A data frame with random effects and their intraclass correlations.
#'
#' @references{
#'   \insertCite{hox2018}{mlmhelpr}
#'   \insertCite{snijders2012}{mlmhelpr}
#'   \insertCite{Wu2012}{mlmhelpr}
#'   \insertCite{kreft1998}{mlmhelpr}
#' }
#'
#' @importFrom lme4 VarCorr
#'
#' @export
#'
#' @examples
#' fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=TRUE)
#'
#' icc(fit)
#'
#' # Logistic Example
#'   # Create binary outcome
#' hsb$binary_math <- ifelse(hsb$mathach <= 13, 0, 1)
#'
#' fitb <- glmer(binary_math ~ 1 + ses + catholic + (1|id),
#' data=hsb, family = binomial(link="logit"))
#'
#' icc(fitb)
#'
#' @export

icc <- function(x) {

   varcorr_df <- as.data.frame(lme4::VarCorr(x))

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

  # prepare for loop ----
  grps <- varcorr_df[,"name"]
  j <- length(grps)
  grp <- vector()
  icc=NULL

  #calculate ICC
   for (i in 1:j) {

     if(lme4::getME(x, "devcomp")$dims[["GLMM"]] == 1 & family(x)$link == "logit") {
       grp[i] <- varcorr_df[i,"value"] / (sum(varcorr_df[i,"value"]) + ((pi^2)/3))
       icc <- rbind(icc, grp[i])
     }

     else{
    grp[i] <- varcorr_df[i,"value"] / (sum(varcorr_df[,"value"]))
    icc <- rbind(icc, grp[i])
  }}

  iccs <- (data.frame(grps, icc=round(icc,3)))

  # for models with random slopes
  if(sum(!is.na(varcorr_df$var2)) > 0)
  {message("Warning: Random slopes detected! Interpret with caution.\n
           See ?mlmhelpr::icc() for more information.")}

  # if glmer, check link function
  if(lme4::getME(x, "devcomp")$dims[["GLMM"]] == 1 & family(x)$link != "logit")
   {message("Warning: Only glmer models with logit link functions supported")}
  # print
  return(iccs)
}

