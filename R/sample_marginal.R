#' Get fixed and hyper marginals from \code{inla}
#'
#' Get fixed and hyper marginals from \code{inla}.
#'
#' Extract the hyper and fixed marginals from an \code{inla} object and
#' bind them in a \code{draws_rvars} object. The fixed parameter marginals
#' are obtained from \code{marginals.fixed}. The hyperparameters are obtained
#' by \code{marginals.hyperpar}.
#'
#' @param .result \code{inla} object.
#' @param n Sample size.
#' @param ren Rename the variable with \code{rename_brms}.
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
