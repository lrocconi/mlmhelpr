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

  #grand-mean centering ----
  if(type == "grand"){
    #get data used
    df2 <- x@frame

    # center variables
    for(i in colnames(df2)){
      if(i %in% variables){

        df2[[paste0(i, "_grand")]] <-
          scale(df2[[paste0(i)]], scale=F)
      } else {NULL}
    }

    #update formula ----
    #get formula
    formula <- x@call[["formula"]]
    #convert to character vecotr
    formula_as_chr <- Reduce(paste, deparse(formula))

    #replace terms
    variables_df <- as.data.frame(variables)

    for(i in 1:nrow(variables_df)){
      var_name <- variables_df[i,1]
      formula_as_chr <- sub(var_name, paste0(var_name, "_grand"),
                            formula_as_chr)
    }

    #refit model ----
    # these are important, but have not used them yet
    modeltype <- model0_ml@call[[1]]
    use_REML <- model0_ml@call[["REML"]]

    return(lme4::lmer(formula_as_chr, df2, REML=F))


  } else {
    #for group-mean centering
    message("Not ready yet!")
  }

}

# test
load("misc/models.Rdata")
center(model0_ml, type="grand", variables="mathach")
center(model1_ml, type="grand", variables=c("mathach", "ses"))
