#' Proportion of variance explained
#'
#' @param model1 Previous model, produced using the `lme4::lmer()` function. Usually, this is the null or unconditional model.
#'
#' @param model2 Current model, produced using the `lme4::lmer()` function.
#'
#' @description `pve` calculates the proportional reduction in variance explained (PVE) by adding variables to a prior, nested model. The PVE is considered a local effect size estimate (Peugh, 2010; Raudenbush & Bryk, 2002).
#'
#' @return Data frame
#'
#' @references{
#'   \insertRef{peugh2010}{mlmhelpr}
#' }
#'
#' @references{
#'   \insertRef{raudenbush2002}{mlmhelpr}
#' }
#'
#' @importFrom lme4 VarCorr
#'
#' @export
#'
#' @examples
#'fit1 <- lme4::lmer(mathach ~ 1 + (1|id), data=hsb, REML=FALSE)
#'fit2 <- lme4::lmer(mathach ~ 1 + ses + (1|id), data=hsb, REML=FALSE)
#'
#'pve(fit1, fit2)


pve <- function(model1, model2 = NULL) {

   # nesting test
  if(stats::nobs(model1) != stats::nobs(model2)){
    stop("Models were not all fitted to the same size of dataset. Models must be nested.")}

  # Do the models have the same number of random effects?
  if(lme4::getME(model1, "k") != lme4::getME(model2, "k")){
    stop("Models do not have the same number of random effects.")}

  # Are the random effects terms the same?
  if(identical(lme4::ngrps(model1), lme4::ngrps(model2)) == FALSE){
    stop("Models do not have the same random effects terms.")}

  # Currently, only supports `lmer` function
  if(lme4::getME(model1, "devcomp")$dims[["GLMM"]] == 1 | lme4::getME(model1, "devcomp")$dims[["NLMM"]] == 1)
  {message("Warning: Currently, only `lmer` models are fully supported. Interpret results with caution.")}

  # Is the estimation method (ML/REML) the same?
  if((lme4::getME(model1, "devcomp")$dims[["REML"]] == 0 & lme4::getME(model2, "devcomp")$dims[["REML"]] != 0)|
     (lme4::getME(model1, "devcomp")$dims[["REML"]] != 0 & lme4::getME(model2, "devcomp")$dims[["REML"]] == 0))
  {message("Warning: Some models fit with REML, some not. Interpret results with caution.")}


  #Model 1 Variance-Covariance Matrix
  varcorr_df1 <- as.data.frame(lme4::VarCorr(model1))

  # this creates full grp names ----
  varcorr_df1$name <- ifelse(is.na(varcorr_df1$var1),
                             varcorr_df1$grp,
                             paste0(varcorr_df1$grp, " ",
                                    varcorr_df1$var1))

  varcorr_df1$name <- ifelse(is.na(varcorr_df1$var2),
                             varcorr_df1$name,
                             paste0(varcorr_df1$grp, " ",
                                    varcorr_df1$var1, " ",
                                    varcorr_df1$var2))

  # this corrects for inclusion of random slopes ----
  varcorr_df1$value <- ifelse(is.na(varcorr_df1$var2),
                              varcorr_df1$vcov,
                              varcorr_df1$vcov*2)


  #Model 2 Variance-Covariance Matrix
  varcorr_df2 <- as.data.frame(lme4::VarCorr(model2))

  # this creates full grp names ----
  varcorr_df2$name <- ifelse(is.na(varcorr_df2$var1),
                             varcorr_df2$grp,
                             paste0(varcorr_df2$grp, " ",
                                    varcorr_df2$var1))

  varcorr_df2$name <- ifelse(is.na(varcorr_df2$var2),
                             varcorr_df2$name,
                             paste0(varcorr_df2$grp, " ",
                                    varcorr_df2$var1, " ",
                                    varcorr_df2$var2))

  # this corrects for inclusion of random slopes ----
  varcorr_df2$value <- ifelse(is.na(varcorr_df2$var2),
                              varcorr_df2$vcov,
                              varcorr_df2$vcov*2)

  # merge dataframes together
  varcorr_df <- merge(varcorr_df1, varcorr_df2, by = c("name"), all=TRUE, sort=FALSE)

  # No Random Coefficients (easy)
  if(sum(is.na(varcorr_df$var2.x)) == nrow(varcorr_df) &
     sum(is.na(varcorr_df$var2.y)) == nrow(varcorr_df)){

    #PVE formula
    varcorr_df$pve <- (varcorr_df$value.x - varcorr_df$value.y)/varcorr_df$value.x

    #clean up to display
    final <- varcorr_df[,c("name", "pve")]
    names(final)[names(final)=="name"] <- "level"
    names(final)[names(final)=="pve"] <- "variance_explained"
    #final[,"variance_explained"] <- round(final[,"variance_explained"], round)
    return(final)

  }

  # With Random Coefficients
  if(sum(is.na(varcorr_df$var2.x)) != nrow(varcorr_df) |
     sum(is.na(varcorr_df$var2.y)) != nrow(varcorr_df)){

    # compute totals for each unique level
    totalx <-  transform(varcorr_df, totalx= stats::ave(varcorr_df$value.x, varcorr_df$grp.x, FUN=sum))
    totaly <-  transform(varcorr_df, totaly= stats::ave(varcorr_df$value.y, varcorr_df$grp.y, FUN=sum))

    # merge in total files
    varcorr_df <- merge(varcorr_df, totalx[,c("name", "totalx")], by=c("name"), sort=FALSE)
    varcorr_df <- merge(varcorr_df, totaly[,c("name", "totaly")], by=c("name"), sort=FALSE)

    # compute pve for total levels
    varcorr_df$pve <- (varcorr_df$totalx - varcorr_df$totaly)/varcorr_df$totalx

    # compute pve for random slope
    varcorr_df[which(varcorr_df$var1.x != "(Intercept)" & is.na(varcorr_df$var2.x)), ]$pve <-
      (varcorr_df[which(varcorr_df$var1.x != "(Intercept)" & is.na(varcorr_df$var2.x)), ]$value.x -
         varcorr_df[which(varcorr_df$var1.y != "(Intercept)" & is.na(varcorr_df$var2.y)), ]$value.y) /
      varcorr_df[which(varcorr_df$var1.x != "(Intercept)" & is.na(varcorr_df$var2.x)), ]$value.x

    #clean up to display
    varcorr_df <- varcorr_df[is.na(varcorr_df$var2.x) | is.na(varcorr_df$var2.y), ]
    final <- varcorr_df[,c("name", "pve")]
    names(final)[names(final)=="name"] <- "level"
    names(final)[names(final)=="pve"] <- "variance_explained"
    final <- final[stats::complete.cases(final),]
    #final[,"variance_explained"] <- round(final[,"variance_explained"], round)

    return(final)
  }

 }

