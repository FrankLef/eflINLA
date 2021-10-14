#' Draw linear predictors from \code{marginals.linear.predictor}
#'
#' Draw linear predictors from \code{marginals.linear.predictor}.
#'
#' Draw linear predictors using marginals.
#'
#' @param .result \code{inla} object.
#' @param new_pos Integer of positions of \code{newdata}.
#' @param n Sample size (per predictor).
#' @param ren TRUE: rename using \code{brms} naming convention.
#' @param repl TRUE: Sampling with replacement.
#'
#' @return \code{posterior::draws_rvars} object where every variable is a
#' linear predictor.
#' @export
linpred_marg_draws_inla <- function(.result, new_pos = integer(),
                                    n = nrow(.result$marginals.fixed[[1]]),
                                    ren = TRUE, repl = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_integer(new_pos)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_flag(ren)
  checkmate::assert_flag(repl)

  margs <- .result$marginals.linear.predictor
  if (length(new_pos)) margs <- margs[new_pos]
  # cat("\n", "inside linpred", "\n")
  # str(margs)
  # cat("\n")

  out <- lapply(X = margs, FUN = function(x) {
    sample(x = x[, "x"], size = n, replace = repl, prob = x[, "y"])
  })

  posterior::as_draws_rvars(out)
}

#' Draw expected predictors from \code{marginals.fitted.values}
#'
#' Draw expected predictors from \code{marginals.fitted.values}.
#'
#' Draw expected predictors using marginals.
#'
#' @param .result \code{inla} object.
#' @param new_pos Integer of positions of \code{newdata}.
#' @param n Sample size (per predictor).
#' @param ren TRUE: rename using \code{brms} naming convention.
#' @param repl TRUE: Sampling with replacement.
#'
#' @return \code{posterior::draws_rvars} object where every variable is a
#' fitted predictor.
#' @export
epred_marg_draws_inla <- function(.result, new_pos = integer(),
                                    n = nrow(.result$marginals.fixed[[1]]),
                                    ren = TRUE, repl = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_integer(new_pos)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_flag(ren)
  checkmate::assert_flag(repl)

  margs <- .result$marginals.fitted.values
  if (length(new_pos)) margs <- margs[new_pos]
  # cat("\n", "inside epred", "\n")
  # str(margs)
  # cat("\n")

  out <- lapply(X = margs, FUN = function(x) {
    sample(x = x[, "x"], size = n, replace = repl, prob = x[, "y"])
  })

  posterior::as_draws_rvars(out)
}
