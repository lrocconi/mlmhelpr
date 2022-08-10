#' Automatically grand-mean or group-mean center a fitted object
#'
#' @param x A model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param grand_variables one or more variables to center at the grand-mean or a user-specified value
#'
#' @param group Grouping variable. If a grouping variable is specified, group-mean centering (also known as centering within cluster, CWC) based on that variable will be performed. Otherwise, grand-mean centering will be performed.
#'
#' @param group_variables Variables to be group-mean centered.
#'
#' @param value Center at a specific value rather than the grand mean (default)
#'
#' @param value_variables Variables to be centered at user-specified value rather than the grand mean (default)
#'
#' @description This function refits a model using grand-mean centering (default) or group-mean centering (if a grouping variable is specified)
#'
#' @return a newly fitted model
#'
#' @importFrom lme4
#'
#' @examples
#'
#' fit <- lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=T)
#'
#' # Centering a single variable around the grand mean
#' fit_gmc <- center(fit, grand_variables="ses")
#'
#' # Centering multiple variables around the grand mean
#' fit_gmc <- center(fit, grand_variables=c("ses", "catholic"))
#'
#' # Centering variables around the group means
#' fit_cwg <- center(fit, group="id", group_variables="ses")
#'
#' # Centering variables using different strategies
#' fit_mixed <- center(fit, group = "id", group_variables = "ses", grand_variables = "catholic")

center <- function(x, grand_variables = NULL, group=NULL, group_variables = NULL, value=NULL, value_variables = NULL){

  # Check whether model is of class lmerMod
  if(class(x)[1] != "lmerMod" & class(x)[1] != "lmerModLmerTest"){
    stop("Only models fitted using the `lmer`function are supported.")}

  #get data
  df2 <- x@frame

  #get formula
  formula <- x@call[["formula"]]
  #convert to character vector
  formula_as_chr <- Reduce(paste, deparse(formula))

  #grand-mean centering ----
  if(!is.null(grand_variables)){

    # center variables
    for(i in colnames(df2)){
      if(i %in% grand_variables){

        df2[[paste0(i, "_grand")]] <-
          scale(as.numeric(df2[[paste0(i)]]), scale=F)
      } else {NULL}
    }

    #update formula ----
    # #get formula
    # formula <- x@call[["formula"]]
    # #convert to character vector
    # formula_as_chr <- Reduce(paste, deparse(formula))

    #replace terms
    variables_df <- as.data.frame(grand_variables)

    for(i in 1:nrow(variables_df)){
      var_name <- variables_df[i,1]
      formula_as_chr <- sub(var_name, paste0(var_name, "_grand"),
                            formula_as_chr)
    }

    #refit model ----
    # these are important, but have not used them yet
    # modeltype <- x@call[[1]]
    # use_REML <- isREML(x)
    # cat("Model with grand-mean centering: \n\n")
    # if(use_REML == "F" | use_REML == "FALSE") {
    #   return(lme4::lmer(formula_as_chr, df2, REML=F))
    # } else {
    #   return(lme4::lmer(formula_as_chr, df2, REML=T))
    # }


  }

  #center at user specified value ----
  if(is.numeric(value)) {

      # center variables
      for(i in colnames(df2)){
        if(i %in% value_variables){

          df2[[paste0(i, "_value")]] <-
            as.numeric(df2[[paste0(i)]]) - value
        } else {NULL}
      }

      #update formula ----
      # #get formula
      # formula <- x@call[["formula"]]
      # #convert to character vector
      # formula_as_chr <- Reduce(paste, deparse(formula))

      #replace terms
      variables_df <- as.data.frame(value_variables)

      for(i in 1:nrow(variables_df)){
        var_name <- variables_df[i,1]
        formula_as_chr <- sub(var_name, paste0(var_name, "_value"),
                              formula_as_chr)
      }

      #refit model ----
      # these are important, but have not used them yet
      # modeltype <- x@call[[1]]
      # use_REML <- isREML(x)
      # cat("Model with centering on user specified value: \n\n")
      # if(use_REML == "F" | use_REML == "FALSE") {
      #   return(lme4::lmer(formula_as_chr, df2, REML=F))
      # } else {
      #   return(lme4::lmer(formula_as_chr, df2, REML=T))
      # }
      #
  }

  #for group-mean centering ----
    if(!is.null(group)) {

    # center variables
    for(i in colnames(df2)){
      if(i %in% group_variables){

        #make dataframe of means by group
        means <- aggregate(x=as.numeric(df2[[i]]), by=list(df2[[group]]), FUN=mean)

        #rename columns to aid in merging
        names(means)[1] <- group
        names(means)[2] <- paste0(i, "_group_mean")

        #merge
        df2 <- merge(df2, means)

        #created centered column
        df2[[paste0(i,"_group_centered")]] <- df2[[i]] - df2[[paste0(i, "_group_mean")]]

        #drop helper column (group)
        df2 <- df2[,!(names(df2) %in% paste0(i, "_group_mean"))]
      } else {NULL}
    }

    #update formula ----
    # #get formula
    # formula <- x@call[["formula"]]
    # #convert to character vecotr
    # formula_as_chr <- Reduce(paste, deparse(formula))

    #replace terms
    variables_df <- as.data.frame(group_variables)

    for(i in 1:nrow(variables_df)){
      var_name <- variables_df[i,1]
      formula_as_chr <- sub(var_name, paste0(var_name, "_group_centered"),
                            formula_as_chr)
    }

    # #refit model ----
    # # these are important, but have not used them yet
    # modeltype <- x@call[[1]]
    # use_REML <- isREML(x)
    # cat("Model with group-mean centering: \n",
    #     "group = ", group, "\n\n")
    # if(use_REML == "F" | use_REML == "FALSE") {
    #   return(lme4::lmer(formula_as_chr, df2, REML=F))
    # } else {
    #   return(lme4::lmer(formula_as_chr, df2, REML=T))
    # }

    }
  # refit model ----
    use_REML <- isREML(x)
    cat("Model re-fit with centered variables: \n\n")
    if(use_REML == "F" | use_REML == "FALSE") {
      return(lme4::lmer(formula_as_chr, df2, REML=F))
    } else {
      return(lme4::lmer(formula_as_chr, df2, REML=T))
   }

}


# test
# load("misc/models.Rdata")
# center(model0_ml, variables="mathach")
#
# center(model0_ml, group="id", variables="mathach")
# center(model1_ml, variables=c("mathach", "ses"))
# tmp <- center(model1_ml, group="id", variables=c("mathach", "ses"))
#
# names(tmp@frame)
