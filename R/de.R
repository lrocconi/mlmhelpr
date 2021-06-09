de <- function(x) {

  # DE = 1 + (nc-1)*ICC, where nc is the number of individuals in each cluster.
  # If all cluster sizes are equal, this is easy. If cluster sizes are not equal, the usual option is to
  # compute the average cluster size (i.e., N/k, where N is level-1 sample size and k is cluster size).
  # Another option is to use the median cluster size

  # ADD warning, if model includes a random slope or predictor variables

  # To ease things, maybe add an argument for the cluster variable you want the design effect for???

  N = nrow(x@frame)
  k = x@Gp[[2]] # This doesn't work for more than 2 levels, only grabs level-2 N. Need to loop maybe??
  nc = N/k
  md = median(table(getME(x, "flist"))) #This doesn't work for more than 2 levels

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

    iccs <- (data.frame(grps, icc=icc))
    return(iccs)
  }

icc_de <- icc(x)

   icc_de$de <- 1 + (nc - 1) * icc_de$icc

  return(head(icc_de, nrow(icc_de)-1))


  # if(lme4::getME(x, "m") > 1)
  # {message("Warning: Random slopes detected! Interpret with caution.\n
  #          See ?mlmhelpr::icc() for more information.")}
  #

}

de(model0_ml)

de(model0_reml)

de(model7_ml)
