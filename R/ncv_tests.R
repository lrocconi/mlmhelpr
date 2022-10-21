#' Non-constant Variance Tests at Level-1 (experimental)
#'
#' @param model a mixed model produced using the `lme4` package and the `lmer()` function. This is an object of class `merMod` and subclass `lmerMod`. Currently, only supports 2-level models.
#'
#' @param formula level-1 formula to compute H test. Formula should be of the form \eqn{y \sim x_1 + ... + x_n \; | \; g} where \eqn{y} is the response, \eqn{x_1 + ... + x_n} are the covariates, and \eqn{g} is the grouping factor, see `lme4::lmList` for details.
#'
#' @param verbose return additional statistics including d-values and outliers from H test; adjusted R-squared, ANOVA results, and mean residual by cluster for Levene test; and likelihood ratio test for B-P test.
#'
#' @description Computes three different Non-constant variance tests: the H test as discussed in Raudenbush and Bryk (2002, pp. 263-265) and Snijders and Bosker (2012, p. 159-160), an approximate Levene's test discussed by Hox et al. (2018, p. 238), and a variation of the Breusch-Pagan test.
#'
#' For the H test, the user must specify the level-1 formula. This test computes a standardized measure of dispersion for each level-2 group and detects heteroscedasticity in the form of between-group differences in the level-one residuals variances. The standardized measure of dispersion is based on estimated ordinary least squares residuals in each group.
#'
#' The Levene's test computes a oneway analysis of variance of the level-2 grouping variable on the squared residuals of the model. This test examines whether the variance of the residuals is the same in all groups.
#'
#' The Breusch-Pagan test regresses the squared residuals of the model on the fitted model. A likelihood ratio test is used to compare this model with a with a null model that regresses the squared residuals on an empty model with the same random effects.
#'
#' @return A list containing results from the three non-constant variance tests.
#'
#' @references{
#'   \insertRef{hox2018}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{raudenbush2002}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{singer2003}{mlmhelpr}
#' }
#'
#' @importFrom lme4 lmList
#'
#' @export
#'
#' @examples
#'
#' fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id), data=hsb, REML=FALSE)
#'
#' ncv_tests(fit)
#'
#' ncv_tests(fit, formula = mathach ~ 1 + ses | id, verbose = TRUE)
#'
#'
ncv_tests <- function(model, formula = NULL, verbose = FALSE ){

if(class(model)[1] != "lmerMod" & class(model)[1] != "lmerModLmerTest"){
    stop("Only models fitted using the `lmer`function are supported.")}

if(getME(model, "n_rfacs") > 1) {
    stop("Only 2-level models currently supported.")}


# Raudenbush & Bryk's H test

if(!is.null(formula)) {

  model_lm <- lme4::lmList(formula, data = model@frame)

  n <- length(model_lm)

  fj <- sapply(model_lm, FUN = "[[", 'df.residual' )
  sj <- sapply(model_lm, FUN = stats::sigma)
  sj2 <- sj^2
  #fj_plot <- plot(fj, sj2)

  d = (log(sj2) - (sum(fj*log(sj2))/sum(fj)))/(sqrt(2/fj))

  use <- fj >=10
  H <- sum((d^2)[use])

  p_H <- 1-stats::pchisq(H, sum(use)-1)

  # Find the outliers
  outliers <- which(abs(d) > 3)
  d_out <- d[outliers]

  #boxplot(d)
  #stem(d)

  H_test_verbose <- list(H = H, df = sum(use)-1, p = p_H, d = d, outliers = d_out)
  H_test <- list(H = H, df = sum(use)-1, p = p_H)

} else {
  H_test_verbose <- list(H = NA, df = NA, p = NA, d = NA, outliers = NA)
  H_test <- list(H = NA, df = NA, p = NA)
  }


# extract level-1 residuals and square them
resid2 <- stats::residuals(model)^2


# Approximate Levene's Test from Hox et al. (2018, p. 238)

  # extract name of cluster variable
  grp <- as.data.frame(lme4::VarCorr(model))[1,1]

  # Regress squared residuals on cluster
  # Significance level of ANOVA F indicates heteroscedasticity in residuals across cluster
  levene <- stats::lm(resid2 ~ get(grp[1]), data = model@frame)
  levene_anova <- stats::anova(levene)
  adj_R2 <- summary(levene)$adj.r.squared


  agg_means <- stats::aggregate(resid2~get(grp[1]), data=model@frame, FUN=mean)
  agg_sd <- stats::aggregate(resid2~get(grp[1]), data=model@frame, FUN=stats::sd)

  levene_test_verbose <- list(F = summary(levene)$fstatistic, p = levene_anova$`Pr(>F)`[1], adj_R2 = adj_R2, anova = levene_anova, mean_resids = agg_means)
  levene_test <- list(F = summary(levene)$fstatistic, p = levene_anova$`Pr(>F)`[1])

# Approximate B-P Test

  # regress squared residuals on fitted model
  newdata <- cbind(stats::model.frame(model), resid2)
  ncvt <- stats::update(model, resid2 ~ ., data = newdata)

  # functions to extract only random effects
  # from https://stackoverflow.com/questions/19815372/remove-all-fixed-effects-from-mixed-model
  parens <- function(x) paste0("(",x,")")
  onlyBars <- function(form) stats::reformulate(sapply(lme4::findbars(form),
                                              function(x) parens(deparse(x))),
                                              response='resid2')

  # regress square residuals on null model
  ncvt_null <- stats::update(model, onlyBars(formula(model)), data = newdata)

  # LRT comparing fit of auxiliary model and null model
  bp_LRT <- suppressMessages(stats::anova(ncvt, ncvt_null))

  bp_test_verbose <- list(Chisq = bp_LRT$Chisq[2], df = bp_LRT$Df[2], p = bp_LRT$`Pr(>Chisq)`[2], anova = bp_LRT)
  bp_test <- list(Chisq = bp_LRT$Chisq[2], df = bp_LRT$Df[2], p = bp_LRT$`Pr(>Chisq)`[2])

# output

  # if(verbose == TRUE & H == TRUE) {
  # output <- list(H_test = H_test_verbose, levene_test = levene_test_verbose, bp_test = bp_test_verbose)
  # }
  #
  # if(verbose == TRUE & H == FALSE) {
  # output <- list(levene_test = levene_test_verbose, bp_test = bp_test_verbose)
  # }
  #
  # if(verbose == FALSE & H == TRUE) {
  #   output <- list(H_test = H_test, levene_test = levene_test, bp_test = bp_test)
  # }
  #
  # if(verbose == FALSE & H == FALSE) {
  #   output <- list(levene_test = levene_test, bp_test = bp_test)
  # }

  if(verbose == TRUE) {
    output <- list(H_test = H_test_verbose, levene_test = levene_test_verbose, bp_test = bp_test_verbose)
  } else {
    output <- list(H_test = H_test, levene_test = levene_test, bp_test = bp_test)
  }


  return(output)

}





