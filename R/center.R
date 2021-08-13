#' Automatically grand-mean or group-mean center a fitted object
#'
#' @param x A model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param type The type of centering needed. One of "grand" for grand-mean centering and "group" for within-group centering.
#'
#' @param group If "group" is selected for type, this is the variable denoting group membership for the variable(s) to be centered
#'
#' @param variable a single varibale to center or a vector of variables to center
#'
#' @param value center at a specific value rather than the mean (default)
#'
#' @description TBD
#'
#' @return a newly fitted model
#'
#' @importFrom lme4
#'
#' @examples


center <- function(x, type, group=NULL, variables, value=NULL){
  if(type == "grand"){
    #get data used
    df2 <- x@frame

    #grand-mean centering ----
    #grand-mean - working only for one variable right now
    #I didn't feel like writing a for-loop yet
    df2[[paste0(variables, "_grand")]] <-
      scale(df[[paste0(variables)]], scale=F)

    #update formula ----
    #get formula
    formula <- x@call[["formula"]]
    #convert to character vecotr
    formula_as_chr <- Reduce(paste, deparse(formula))
    #replace terms
    new_formula <- sub(variables, paste0(variables, "_grand"),
                       formula_as_chr)

    #refit model ----
    # these are important, but have not used them yet
    modeltype <- model0_ml@call[[1]]
    use_REML <- model0_ml@call[["REML"]]

    return(lme4::lmer(new_formula, df2, REML=F))


  } else {
    #for group-mean centering
    message("Not ready yet!")
  }

}


load("misc/models.Rdata")
center(model0_ml, type="grand", variables="mathach")
