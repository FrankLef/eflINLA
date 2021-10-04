#' Sample the posterior using \code{inla.posterior.sample}
#'
#' Sample the posterior using \code{inla.posterior.sample}.
#'
#' For each sample given by \code{inla.posterior.sample} we extract the value
#' of the intercept, parameters, precision and hyperparameters. When the
#' parameters is found in the hyperparameters instead of the \emph{latent} section
#' the hyperparameters is chosen.
#'
#' @param .result \code{inla} object.
#' @param is.df TRUE: return a data.frame, FALSE: return a list.
#' @param var Name of extra column with name of the variable in the data.frame.
#'
#' @return List / data.frame of samples
#' @export
posterior_samples <- function(.result, is.df = TRUE, var = "var") {
  checkmate::assert_class(.result, classes = "inla")
  checkmate::assert_logical(is.df)
  checkmate::assert_string(var, min.chars = 1)

  samples <- list()

  samples
}




#' Find which parameter is in the hyperpar section of the sample
#'
#' Find which parameter is in the hyperpar section of the sample.
#'
#' The parameters could be in the \emph{hyperpar} section of the sample
#' as well as in the \emph{latent} section.  When this si the case, we take the
#' value in \emph{hyperpar}.
#'
#' @param .result \code{inla} object.
#'
#' @return Vector of logical values indicating if the tag is in hyper
#' @export
posterior_samples_tags <- function(.result) {
  checkmate::assert_class(.result, classes = "inla")

  samples <- INLA::inla.posterior.sample(n = 1, result = .result,
                                         selection = list("Predictor" = 1))
  assertthat::not_empty(samples)
  hypers <- names(samples[[1]]$hyperpar)

  # see which parameter is in the hyperparameters
  # This happens when then the parameter is in the f() section of
  # the inla formula
  tags <- .result$misc$configs$contents$tag

  out <- sapply(X = tags, FUN = function(p) {
    y <- grepl(pattern = p, x = hypers)
    y <- any(y)
  })
  names(out) <- tags
  out
}
