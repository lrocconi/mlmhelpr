#' Automatically grand-mean or group-mean center a fitted object
#'
#' @param x A model produced using the `lme4::lmer()` function. This is an object of class `merMod` and subclass `lmerMod`.
#'
#' @param grand_variables one or more variables to center at the grand-mean
#'
#' @param group Grouping variable. If a grouping variable is specified, group-mean centering (also known as centering within cluster) based on that variable will be performed.
#'
#' @param group_variables Variables to be group-mean centered.
#'
#' @param value Center at a specific value rather than the grand mean
#'
#' @param value_variables Variables to be centered at user-specified value rather than the grand mean
#'
#' @description This function refits a model using grand-mean centering, group-mean centering (if a grouping variable is specified), or centering at a user-specified value. The function only works for two level models.
#'
#' @return A newly fitted model with centered variables
#'
#' @importFrom lme4 lmer
#'
#' @export
#'
#' @examples
#'
#' fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
#' data=hsb, REML=TRUE)
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
  if(!inherits(x, "lmerMod")){
    stop("Only models fitted using the `lmer` function are currently supported.")
  }

  # Check is group variable is also in group_variables
  if(!is.null(group) && !is.null(group_variables)) {
    if(group %in% group_variables) {
      stop(paste0("The grouping variable '", group, "' cannot be included in 'group_variables'. ",
                  "You cannot group-center the variable that defines the groups."))
    }
  }

  # Check if the model has more than 2 levels
  if(lme4::getME(model, "n_rfacs") > 1) {
    stop("Only 2-level models currently supported.")
  }

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

        df2[[paste0(i, "_grand")]] <- as.numeric(scale(as.numeric(df2[[i]]), scale=FALSE))

      } else {NULL}
    }

    #replace terms
    variables_df <- as.data.frame(grand_variables)

    for(i in 1:nrow(variables_df)){
      var_name <- variables_df[i,1]

      formula_as_chr <- gsub(
        paste0("\\b", var_name, "\\b"),
        paste0(var_name, "_grand"),
        formula_as_chr
      )

    }

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

      #replace terms
      variables_df <- as.data.frame(value_variables)

      for(i in 1:nrow(variables_df)){
        var_name <- variables_df[i,1]

        formula_as_chr <- gsub(
          paste0("\\b", var_name, "\\b"),
          paste0(var_name, "_value"),
          formula_as_chr
        )

      }

  }

  #for group-mean centering ----
    if(!is.null(group)) {

    # center variables
    for(i in colnames(df2)){
      if(i %in% group_variables){

        group_means <- stats::ave(as.numeric(df2[[i]]), df2[[group]], FUN = mean)
        df2[[paste0(i,"_group_c")]] <- as.numeric(df2[[i]]) - group_means

        # #make dataframe of means by group
        # means <- stats::aggregate(x=as.numeric(df2[[i]]), by=list(df2[[group]]), FUN=mean)
        #
        # #rename columns to aid in merging
        # names(means)[1] <- group
        # names(means)[2] <- paste0(i, "_group_mean")
        #
        # #merge
        # df2 <- merge(df2, means, by = group, all.x = TRUE, sort = FALSE)
        #
        # #created centered column
        # df2[[paste0(i,"_group_c")]] <- df2[[i]] - df2[[paste0(i, "_group_mean")]]
        #
        # #drop helper column (group)
        # df2 <- df2[,!(names(df2) %in% paste0(i, "_group_mean"))]
      }
    }

    #replace terms
    variables_df <- as.data.frame(group_variables)

    for(i in 1:nrow(variables_df)){
      var_name <- variables_df[i,1]


      formula_as_chr <- gsub(
        paste0("\\b", var_name, "\\b"),
        paste0(var_name, "_group_c"),
        formula_as_chr
      )

    }

    }
  # refit model
    use_REML <- lme4::isREML(x)
    message("Model re-fit with centered variables.")
    formula_obj <- stats::as.formula(formula_as_chr)
    if(!use_REML) {
      return(lme4::lmer(formula_obj, df2, REML=FALSE))
    } else {
      return(lme4::lmer(formula_obj, df2, REML=TRUE))
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
