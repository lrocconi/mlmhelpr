#' ICC (Intraclass Correlation)
#'
#' @param x A model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @description DESCRIPTION
#'
#' **Note**: For models with random slopes, it is generally advised to interpret with caution. According to Kreft and De Leeuw (2002), "The concept of intra-class correlation is based on a model with a random intercept only. No unique intra-class correlation can be calculated when a random slope is present in the model" (p. 74). However, Snijders and Bosker (YEAR) offer a calculation to derive this value (equation 7.9). This equation is implemented here.
#'
#'
#' @return A data frame with random effects and their intraclass correlations.
#'
#' @references{
#'   \insertRef{hox2018}{mlmhemlpr}
#'   \insertRef{kreft2002}{mlmhemlpr}
#'   \insertRef{snijders2012}{mlmhemlpr}
#' }
#'
#' @importFrom lme4 VarCorr
#'
#' @examples
#' fit <- lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=T)
#'
#' icc(fit)
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
    grp[i] <- varcorr_df[i,"value"] / (sum(varcorr_df[,"value"]))
    icc <- rbind(icc, grp[i])
  }

  iccs <- (data.frame(grps, icc=round(icc,3)))

  # print
  return(iccs)
  # for models with random slopes
  if(sum(!is.na(varcorr_df$var2)) > 0)
  {message("Warning: Random slopes detected! Interpret with caution.\n
           See ?mlmhelpr::icc() for more information.")}
}

## testing ----

load("misc/models.Rdata")


icc(model1_ml) # null model

icc(model7_ml) # three level

icc(model4_ml) # with random slope
