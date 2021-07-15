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

  #calculate ICC
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

  if (median == TRUE) {
    df2 <- data.frame(name=df$name, k=df$k, nc=df$md, icc=df$icc, de=df$de) #for median
  } else {
    df2 <- data.frame(name=df$name, k=df$k, nc=df$nc, icc=df$icc, de=df$de)
  }

  if(lme4::getME(x, "m") > 1)
  {message("Warning: Random slopes detected! Interpret with caution.\n
            See ?mlmhelpr::de() for more information.")}

  #return(head(df2, nrow(df2)-1))
  return(df2[df2$name != "Residual", ])
}

de(model0_ml, median=T)

de(model0_reml)

de(model7_ml)
