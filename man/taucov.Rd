% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/taucov.R
\name{taucov}
\alias{taucov}
\title{Tau Covariance}
\usage{
taucov(model)
}
\arguments{
\item{model}{A model fit using the \code{lme4::lmer} function}
}
\value{
A data frame with the intercept, randomly-varying variables, covariance, and correlation.
}
\description{
Quickly get the covariance and correlation between intercepts and slopes. By default, \code{lme4} only displays the correlation.
}
\examples{

fit <- lme4::lmer(mathach ~ 1 + ses + (1 + ses|id), data=hsb, REML=TRUE)

taucov(fit)

}
