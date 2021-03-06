% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils_01-A.R
\name{rename_inla2brms}
\alias{rename_inla2brms}
\title{Rename strings/variable from \code{inla} to \code{brms} equivalent}
\usage{
rename_inla2brms(x)
}
\arguments{
\item{x}{Character with \code{inla} strings.}
}
\value{
Character with inla strings renamed.
}
\description{
Rename strings/variable from \code{inla} to \code{brms} equivalent.
}
\details{
Rename variables and names in \code{inla} to be easier to use in plots
and summary by using the \code{brms} as closely as possible. For example
\emph{(Intercept)} becomes \emph{Intercept}, \emph{Beta for A} becomes
\emph{b_A} and \emph{"Precision for the Gaussian observations"} becomes
\emph{Precision}.
}
\examples{
df <- data.frame(
 x = c(
  NA_character_,
  "Precision for the Gaussian observations",
  "SD for the Gaussian observations",
  "sd for the Gaussian observations",
  "unknown", "(Intercept)",
  "Beta for A", "Beta for B", "Beta for B"),
 y = c(NA_character_,
  "Precision", "sigma", "sigma",
  "unknown", "b_Intercept",
  "b_A","b_B", "b_B"))
z <- rename_inla2brms(df$x)
stopifnot(identical(z, df$y))
}
