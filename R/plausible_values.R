#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' #hierarchical linear models p. 71
#'
plausible_values <- function(x){

#get Y00
intercept <- as.data.frame(x@beta)[1,]
#get CI
sd <- 1.96 #95%

#get T00
var_df <- as.data.frame(lme4::VarCorr(x))
variance <- subset(var_df, var1 == "(Intercept)")$sdcor

upper <- intercept + (1.96*variance)
lower <- intercept - (1.96*variance)



message("Plausible values range/predicitve interval:")
message(paste0("95% of values for the intercepts fall between ",
               round(lower,2), " and ", round(upper,2), "."))
data.frame("lower"=lower, "upper" = upper)
}

load("misc/models.Rdata")
plausible_values(model1_ml)
