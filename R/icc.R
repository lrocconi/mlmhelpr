#' Intraclass Correlation (ICC)
#'
#' @param model A model produced using the `lme4::lmer()` or `lme4::glmer()` functions. This is an object of class `merMod` and subclass `lmerMod` or `glmerMod`.
#'
#' @param x A numeric value of the predictor variable associated with the random slope in the model. This is only used when random slopes are present, and the ICC is computed conditional on this value.
#'
#' @description The `icc` function calculates the intraclass correlation (ICC) for multilevel models. The ICC represents the proportion of group-level variance to total variance. The ICC can be calculated with two or more levels in random-intercept models (Hox et al, 2018).
#'
#' **Note**: For models with random slopes, it is generally advised to interpret with caution. According to Kreft and De Leeuw (1998, p. 63), "The concept of intra-class correlation is based on a model with a random intercept only. No unique intra-class correlation can be calculated when a random slope is present in the model." However, Snijders and Bosker (2012) offer a calculation to derive a conditional ICC at a specified value of `x` (equation 7.9). This equation is implemented here.
#'
#' The `icc` function calculates the intraclass correlation for linear mixed-effects models estimated with the `lme4::lmer` function or generalized linear mixed-effects models estimated with the `lme4::glmer` function with `family = binomial(link="logit")`. For logistic models, the estimation method follows Hox et al. (2018, p. 107) recommendation of setting the level-1 residual variance to \eqn{\frac{\pi^2}{3}}. For a discussion of different methods for estimating the intraclass correlation for binary responses, see Wu et al. (2012).
#'
#'
#' @return A data frame with random effects and their intraclass correlations.
#'
#' @references{
#'   \insertRef{hox2018}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{kreft1998}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{snijders2012}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{Wu2012}{mlmhelpr}
#' }
#'
#' @importFrom lme4 VarCorr
#' @importFrom Rdpack reprompt
#' @import mathjaxr
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
#' fitb <- lme4::glmer(binary_math ~ 1 + ses + catholic + (1|id),
#' data=hsb, family = binomial(link="logit"))
#'
#' icc(fitb)
#'
#' @export

icc <- function(model, x = 0) {

   varcorr_df <- as.data.frame(lme4::VarCorr(model))
   has_slopes <- any(!is.na(varcorr_df$var2))
   n_groups <- length(unique(varcorr_df$grp[varcorr_df$grp != "Residual"]))

   # stop if multiple grouping levels with random slopes
   if (has_slopes && n_groups > 1) {
     stop("Models with random slopes across multiple grouping levels are not currently supported for ICC calculation.")
   }


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
  if (!has_slopes) {
    varcorr_df$value <- varcorr_df$vcov
  } else {

    # extract variance components
    vc <- as.data.frame(lme4::VarCorr(model))

    # keep only this grouping factor (assumes one grouping level)
    vc <- vc[vc$grp != "Residual", ]

    sigma_u0 <- vc$vcov[vc$var1 == "(Intercept)" & is.na(vc$var2)]
    sigma_u1 <- vc$vcov[vc$var1 != "(Intercept)" & is.na(vc$var2)]
    sigma_u01 <- vc$vcov[!is.na(vc$var2)]

    # compute level-2 variance at x
    var_u <- sigma_u0 + 2 * x * sigma_u01 + (x^2) * sigma_u1

    # residual
    sigma_e <- attr(lme4::VarCorr(model), "sc")^2

    # replace value column with correct total variance pieces

    grp_name <- unique(vc$grp[vc$grp != "Residual"])[1]  # get grouping factor

    varcorr_df <- data.frame(
      name = c(paste0(grp_name, " (Intercept)"), "Residual"),
      value = c(var_u, sigma_e)
    )

  }

  # prepare for loop ----
  grps <- varcorr_df[,"name"]
  j <- length(grps)
  grp <- vector()
  icc=NULL

  #calculate ICC
   for (i in 1:j) {

     if(lme4::getME(model, "devcomp")$dims[["GLMM"]] == 1 & stats::family(model)$link == "logit") {
       grp[i] <- varcorr_df[i,"value"] / (sum(varcorr_df[,"value"]) + ((pi^2)/3))
       icc <- rbind(icc, grp[i])
     }

     else{
    grp[i] <- varcorr_df[i,"value"] / (sum(varcorr_df[,"value"]))
    icc <- rbind(icc, grp[i])
  }}

  col_name <- if(has_slopes) paste0("icc (x=",x,")") else "icc"
  iccs <- data.frame(grps, icc=round(icc,3))
  names(iccs)[2] <- col_name

  # for models with random slopes
  if(has_slopes)
  {message(paste("Warning: Random slopes detected!",paste0("ICC computed at x = ",x,"."),"Interpret as conditional ICC\n
           See ?mlmhelpr::icc() for more information."))}

  # if glmer, check link function
  if(lme4::getME(model, "devcomp")$dims[["GLMM"]] == 1 & stats::family(model)$link != "logit")
   {message("Warning: Only glmer models with logit link functions supported")}
  # print
  return(iccs)
}

