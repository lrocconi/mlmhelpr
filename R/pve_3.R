#' Proportion of variance explained
#'
#' @param model1 Previous model, produced using the `lme4::lmer()` function. Usually, this is the null or unconditional model.
#'
#' @param model2 Current model, produced using the `lme4::lmer()` function.
#'
#' @description `pve` calculates the proportional reduction in variance explained (PVE) by adding variables to a prior, nested model. The PVE is considered a local effect size estimate [@peugh2020, @raudenbush2002].
#'
#' @return Message (default) or data frame (with `verbose=F`)
#'
#' @references{
#'   \insertRef{peugh2010}{mlmhemlpr}
#'   \insertRef{raudenbush2002}{mlmhemlpr}
#' }
#'
#' @importFrom lme4 varCorr
#'
#' @export
#'
#' @examples
#'


pve <- function(model1, model2, verbose=TRUE) {

 # Yes, we need to think about how to do this. We could use the following to check the number of random effect terms
  # lme4::getME(model, "k")
  # For 3-level models k will equal 2.
  # Another hiccup is how to deal with random coefficients. Maybe throw a warring or error?
  # We could also count the number of rows in as.data.frame(lme4::VarCorr(model))


    # nesting test 1 - not sure if this is correct
    if(nobs(model1) != nobs(model2)){
      stop("Models were not all fitted to the same size of dataset. Models must be nested.")}

  # get number of groups of model2
  ngrps_1 <- summary(model1)[["ngrps"]]
  ngrps_2 <- summary(model2)[["ngrps"]]

  # get level-1 variance components
  vc1_lvl1 <- (summary(model1)$sigma)^2
  vc2_lvl1 <- (summary(model2)$sigma)^2

  # get level-2 variance components
  vc1_lvl2 <- as.data.frame(lme4::VarCorr(model1))$vcov[1]
  vc2_lvl2 <- as.data.frame(lme4::VarCorr(model2))$vcov[1]

  # get level-3 variance components, model_1
  if(nrow(as.data.frame(ngrps_1)) > 1){
    ngrps_1_df <- cbind(grp=rownames(as.data.frame(ngrps_1)),
                        data.frame(n=ngrps_1, row.names=NULL))

    m1_level_3_group <- ngrps_1_df[order(ngrps_1_df$n),]$grp[1]

    vc1_lvl3 <- subset(as.data.frame(lme4::VarCorr(model1)),
                       grp == m1_level_3_group)$vcov
  }

  # get level-3 variance components, model_2
  if(nrow(as.data.frame(ngrps_2)) > 1){
    ngrps_2_df <- cbind(grp=rownames(as.data.frame(ngrps_2)),
                        data.frame(n=ngrps_2, row.names=NULL))

    m2_level_3_group <- ngrps_2_df[order(ngrps_2_df$n),]$grp[1]

    vc2_lvl3 <- subset(as.data.frame(lme4::VarCorr(model2)),
                       grp == m2_level_3_group &
                         var1 == "(Intercept)" &
                         is.na(var2))$vcov
  }

  # pve for 2-levels ----

  if(nrow(as.data.frame(ngrps_1)) ==1 &
     nrow(as.data.frame(ngrps_2)) ==1){

    # calculation for level-1 pve
    pve_1 <- (vc1_lvl1 - vc2_lvl1) / vc1_lvl1
    # calculation for level-2 pve
    pve_2 <- (vc1_lvl2 - vc2_lvl2) / vc1_lvl2

    # return message by default
    if(verbose == TRUE){
      cat("Proportion of variance explained at level-1 = ",
          round(pve_1,3), "\n")
      cat("Proportion of variance explained at level-2 = ",
          round(pve_2, 3))}
    # if verbose = F, just return dataframe of values
    if(verbose == FALSE){
      print(data.frame(level=c(1, 2),
                 variance_explained = c(pve_1, pve_2)))
    }
  }

    # pve for 2-level AND 3-level ----

    if(nrow(as.data.frame(ngrps_1)) == 1 &
       nrow(as.data.frame(ngrps_2)) > 1){

      # calculation for level-1 pve
      pve_1 <- (vc1_lvl1 - vc2_lvl1) / vc1_lvl1
      # calculation for level-2 pve
      pve_2 <- (vc1_lvl2 - vc2_lvl2) / vc1_lvl2
      # calculation for level-3 pve
      pve_3 <- (vc1_lvl2 - vc2_lvl3) / vc1_lvl2

      # return message by default
      if(verbose == T){
        cat("Proportion of variance explained at level-1 = ",
            round(pve_1,3), "\n")
        cat("Proportion of variance explained at level-2 = ",
            round(pve_2, 3), "\n")
        cat("Proportion of variance explained at level-3 = ",
            round(pve_3, 3))}
      # if verbose = F, just return dataframe of values
      if(verbose == F){
        print(data.frame(level=c(1, 2, 3),
                         variance_explained = c(round(pve_1,3),
                                                round(pve_2,3),
                                                round(pve_3,3))))
      }
    }

    # pve for 2-level AND 3-level ----

    if(nrow(as.data.frame(ngrps_1)) > 1 &
       nrow(as.data.frame(ngrps_2)) > 1){

      # calculation for level-1 pve
      pve_1 <- (vc1_lvl1 - vc2_lvl1) / vc1_lvl1
      # calculation for level-2 pve
      pve_2 <- (vc1_lvl2 - vc2_lvl2) / vc1_lvl2
      # calculation for level-3 pve
      pve_3 <- (vc1_lvl3 - vc2_lvl3) / vc1_lvl3

      # return message by default
      if(verbose == T){
        cat("Proportion of variance explained at level-1 = ",
            round(pve_1,3), "\n")
        cat("Proportion of variance explained at level-2 = ",
            round(pve_2, 3), "\n")
        cat("Proportion of variance explained at level-3 = ",
            round(pve_3, 3))}
      # if verbose = F, just return dataframe of values
      if(verbose == F){
        print(data.frame(level=c(1, 2, 3),
                         variance_explained = c(round(pve_1,3),
                                                round(pve_2,3),
                                                round(pve_3,3))))
      }
    }
  }

  load("misc/models.Rdata")

  pve(model0_ml, model1_ml, FALSE) # ok
    # 2-level + 2-level

  pve(model0_ml, model1_ml, T) # ok

  # 2-level + 3-level model
  # set 2-level model model
  model7_lvl2 <- lme4::lmer(stress ~ 1 + (1|ward),
                            nurses, REML=T)

  #model7_ml is a 3-level model, see "model examples.R"
  pve(model7_lvl2, model7_ml, T) # FIXME something is not right here - negative proportion explained

  # 3-level + 3-level
  pve(model7_ml, model8_ml, T) # ok

  # nesting check
  pve(model0_ml, model18_ml, F) # not nested, returns error as expected
