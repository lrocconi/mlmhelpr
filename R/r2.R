# pseudo R-squared: squared correlation between predicted and observed values

r2 <- function(x) {
  
  r2 <- (cor(predict(x), lme4::getME(x, "y")))^2
  
  return(cat(c("The squared correlation between predicted and observed values is", round(r2,3)),sep="\n"))
  
}


# appears to be working correctly