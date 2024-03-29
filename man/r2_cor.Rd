% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/r2_cor.R
\name{r2_cor}
\alias{r2_cor}
\title{Pseudo R-squared: Squared correlation between predicted and observed values}
\usage{
r2_cor(x, verbose = FALSE)
}
\arguments{
\item{x}{A model produced using the \code{lme4::lmer()} function. This is an object of class \code{merMod} and subclass \code{lmerMod}.}

\item{verbose}{If true, prints an explanatory message, "The squared correlation between predicted and observed values is...". If false (default), returns a value.}
}
\value{
If \code{verbose == TRUE}, a console message. If \code{verbose == FALSE} (default), a numeric value.
}
\description{
The \code{r2_cor} function estimates a pseudo R-squared by correlating predicted \eqn{\hat{Y}} values and observed \eqn{Y} values. This pseudo R-squared is similar to the \eqn{R^2} used in OLS regression. It indicates amount of variation in the outcome that is explained by the model (Peugh, 2010; Singer & Willett, 2003, p. 36).
}
\examples{

fit <- lme4::lmer(mathach ~ 1 + ses + catholic + (1|id),
data=hsb, REML=TRUE)

# returns a numeric value
r2_cor(fit)

# returns a console message with the r2 value
r2_cor(fit, verbose = TRUE)
}
\references{
{
\insertRef{peugh2010}{mlmhelpr}
}

{
\insertRef{singer2003}{mlmhelpr}
}
}
