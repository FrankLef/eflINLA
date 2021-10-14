#' Get fixed and hyper marginals from \code{inla}
#'
#' Get fixed and hyper marginals from \code{inla}.
#'
#' Extract the hyper and fixed marginals from an \code{inla} object and
#' bind them in a \code{draws_rvars} object. The fixed parameter marginals
#' are obtained from \code{marginals.fixed}. The hyperparameters are obtained
#' by \code{marginals.hyperpar}.
#'
#' @param .result \code{inla} object
#' @param n Sample size.
#' @param ren Rename the variable with \code{rename_brms}
#' @param repl TRUE: Sampling with replacement; FALSE: no replacement.
#'
#' @return \code{draws_rvars} object.
#' @export
tidy_marg_draws_inla <- function(.result, n = nrow(.result$marginals.fixed[[1]]),
                          ren = TRUE, repl = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_flag(ren)
  checkmate::assert_flag(repl)

  margs <- extract_marginals(.result, ren = ren)

  out <- lapply(X = margs, FUN = function(x) {
    sample(x = x[, "x"], size = n, replace = repl, prob = x[, "y"])
  })

  out <- posterior::as_draws_rvars(out)
  out
}


#' Get all marginals from \code{inla}
#'
#' Get all marginals from \code{inla}.
#'
#' Extract the hyper and fixed marginals from an \code{inla} object and
#' put them in a data.frame when \code{is.df = TRUE} (default), otherwise
#' in a list when \code{is.df = FALSE}.
#'
#' @inheritParams tidy_marg_draws_inla
#'
#' @return List of marginals
#' @export
extract_marginals <- function(.result, ren = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_flag(ren)

  margs <- list()

  # get the transformed marginals for hyperparameters
  margs$hyper <- transform_marginal_hyper(.result)
  assertthat::not_empty(margs$hyper)

  # the list of all marginals (fixed and hyper)
  margs$all <- c(.result$marginals.fixed, margs$hyper)
  assertthat::assert_that(length(margs$all) > length(margs$hyper))

  # rename variables using brms naming convention
  if(ren) names(margs$all) <- rename_inla2brms(names(margs$all))

  margs$all
}


#' Transform the hyperparameters' marginal
#'
#' Transform the hyperparameters' marginal.
#'
#' Convert the precision used in the hyperparameers' marginals to standard
#' deviations.
#'
#' @param .result inla object.
#'
#' @return List of hyperparameter marginals.
#' @export
transform_marginal_hyper <- function(.result) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_names(names(.result),
                          must.include = c("marginals.hyperpar",
                                           "internal.marginals.hyperpar"))

  # don't modify this without a good reason and test it
  # the regex pattern used to find the "precision"
  prec_rgx = "^precision"

  margs <- mapply(
    FUN = function(marg, nm, imarg) {
      if(any(grepl(pattern = prec_rgx, x = nm, ignore.case = TRUE))) {
        # use inverse transformed internal.marginals.hyperpar for precision
        m <- prec2sd_marg(marg, is_log = TRUE)
      } else {
        # use marginals.hyperpar for other hyperparameters
        m <- marg
      }
      return(m)
    },
    .result$marginals.hyperpar,
    names(.result$marginals.hyperpar),
    .result$internal.marginals.hyperpar,
    SIMPLIFY = FALSE)

  # edit the names to replace "precision" by "SD"
  names(margs) <- rename_inla(names(margs), choice = "Precision")
  margs
}
