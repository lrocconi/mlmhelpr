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
#' # glmer model
#' hsb$binary_math <- ifelse(hsb$mathach <= 13, 0, 1)
#' fit2 <- lme4::glmer(binary_math ~ 1 + ses + catholic + (1+ses|id),
#'                      data=hsb, family = binomial(link="logit"))
#' reliability(fit2)

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


# add residual variance (pi^2)/3 for logistic models to varcorr_df
if(lme4::getME(model, "devcomp")$dims[["GLMM"]] == 1 & stats::family(model)$link == "logit") {
  varcorr_df[nrow(varcorr_df) + 1, ] <- c("Residual",NA,NA,NA,NA,"Residual", as.numeric((pi^2)/3))
  varcorr_df$value <- as.numeric(varcorr_df$value)
  }


N <- nrow(model@frame) #number of level-1 units
k <- lme4::ngrps(model) #number of groups

# extract size of each group and save as a dataframe
nclust <- length(lme4::ngrps(model))

# number of units within each cluster
nj <- as.data.frame(lme4::getME(model, "flist"))

# save number of units in each cluster as a dataframe
df <- lapply(nj, FUN = function(x){
  as.data.frame(table(x))}
    )

# extact names of random effects and residual
grps <- varcorr_df[,"name"]
j <- length(grps) # number of random effects including residual

# the last row is the Residual variance
resid_place <- nrow(varcorr_df)


# loop to compute reliabilities. Formula from Raudenbush & Bryk (2002, p. 46)

compute_reliabilities <- function(x) {

  for (i in 1:j){
    rxx <- rep(NA, nrow(x))
    x[, ncol(x) + 1] <- rxx
    colnames(x)[ncol(x)] <- paste0("rxx", i)

    for (r in 1:nrow(x)) {
      x[r,paste0("rxx", i)] <- (varcorr_df[i,"value"]) / ((varcorr_df[i,"value"]) + ((varcorr_df[resid_place, "value"]))/x[r,2])
    }
  }

  return(x)

}

# apply reliability function to each element of the df list
df <- lapply(df, FUN = function(x) {compute_reliabilities(x)})



# compute column means, removing groups and their size
colnums <- lapply(df, FUN = length)

num <- c()

for (i in 1:length(colnums)) {

  num[i] <- colnums[[i]]
}


for (i in 1:length(df)) {

if(num[i] > 3) {

means <- lapply(df, FUN = function(x) {

  colMeans(x[,-c(1,2)])

  })
} else {

means <- lapply(df, FUN = function(x){

  mean(x[,-c(1,2)])
})
}
}

# use random effect names
means <- lapply(means, stats::setNames, nm = grps)


# remove residual
means <- lapply(means, function(x) {x[-resid_place]})


cat("Reliability Estimates:", "\n\n")
return(means)

}


