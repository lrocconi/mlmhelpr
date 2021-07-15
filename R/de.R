#' Design Effect
#'
#' @param x model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param median Boolean argument (TRUE/FALSE) to use the median cluster size to compute the design effect. By default, the average is used.
#'
#'
#' @description The design effect estimates the difference between the variance of an observed sample and a similar simple random sample. In the multilevel modeling context, this can be used to determine whether clustering introduces negative bias and whether the assumption of independence is held. Thus, it can help determine whether multilevel modeling is appropriate for a given data set. The calculations are based on @hox2018 and uses the `mlmhelpr:icc` function. A rule of thumb is that design effects smaller than 2 indicates multilevel modelling is not necessary; however, this is dependent on cluster size and other factors [@lai2015].
#'
#' NOTE ABOUT RANDOM SLOPES
#'
#'
#' @return a data frame containing the cluster variable, number of clusters, average (or median) cluster size, intraclass correlation, and the design effect
#'
#'
#' @references{
#'   \insertRef{hox2018}{mlmhemlpr}
#'   \insertRef{lai2015}{mlmhemlpr}
#' }
#'
#' @importFrom lme4 ngrps getME
#'
#' @examples
#' fit <- lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=T)
#'
#' de(fit)


de <- function(x, median = FALSE) {

  # DE = 1 + (nc-1)*ICC, where nc is the number of individuals in each cluster.
  # If all cluster sizes are equal, this is easy. If cluster sizes are not equal, the usual option is to
  # compute the average cluster size (i.e., N/k, where N is level-1 sample size and k is cluster size).
  # Another option is to use the median cluster size

  # Compute counts at each level as well as average and median cluster size
  N <- nrow(x@frame)
  nclust <- length(lme4::ngrps(x))
  k <- lme4::ngrps(x)
  nc <- vector()
  md <- vector()

  for (i in 1:nclust) {
    nc[i] = N/k[i]
    md[i] = median(table(lme4::getME(x, "flist")[[i]]))
  }

  k <- as.data.frame(k)
  k$grp <- rownames(k)
  k$nc <- nc
  k$md <- md


  # Compute ICC
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

  #calculate ICC (same as mlmhelpr::icc)
  for (i in 1:j) {
    grp[i] <- varcorr_df[i,"value"] / (sum(varcorr_df[,"value"]))
    icc <- rbind(icc, grp[i])
  }

  df <- merge(varcorr_df, k, by="grp", all.x=TRUE)
  df$icc <- icc

  if (median == TRUE) {
    df$de <- 1 + (md - 1) * df$icc #using median
  } else {
    df$de <- 1 + (df$nc - 1) * df$icc
  }

  #for median = TRUE
  if (median == TRUE) {
    df2 <- data.frame(cluster_name=df$name,
                      n_of_clusters=df$k,
                      median_cluster_size=df$md,
                      icc=df$icc,
                      design_effect=df$de)
  #default
  } else {
    df2 <- data.frame(cluster_name=df$name,
                      n_of_clusters=df$k,
                      avg_cluster_size=df$nc,
                      icc=df$icc,
                      design_effect=df$de)
  }

  if(lme4::getME(x, "m") > 1)
  {message("Warning: Random slopes detected! Interpret with caution.\n
            See ?mlmhelpr::de() for more information.")}

  #return(head(df2, nrow(df2)-1))
  return(df2[df2$cluster_name != "Residual", ])
}

de(model0_ml, median=T)

de(model0_reml)

de(model7_ml)
