#' Get all marginals from \code{inla} and simulate a sample
#'
#' Get all marginals from \code{inla} and simulate a sample.
#'
#' Extract the hyper and fixed marginals from an \code{inla} object and
#' put simulate a sample of size n.
#'
#' @param .result \code{inla} object
#' @param n Sample size.
#' @param ren Rename the variable with \code{rename_brms}
#' @param repl TRUE: Sampling with replacement; FALSE: no replacement.
#'
#' @return List / data.frame of marginals
#' @export
draw_marginal <- function(.result, n = nrow(.result$marginals.fixed[[1]]),
                          ren = TRUE, repl = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_flag(ren)
  checkmate::assert_flag(repl)

  margs <- extract_marginal(.result, ren = ren)

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
#' @inheritParams draw_marginal
#'
#' @return List / data.frame of marginals
#' @export
extract_marginal <- function(.result, ren = TRUE) {
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
#' @return List of hyperparameter marginals transformed to user scale
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

  # edit the names to replace "precision" by sd_name
  # names(margs) <- sub(pattern = prec_rgx, replacement = sd_name,
  #                     x = names(margs), ignore.case = TRUE)
  # cat("\n", "inside transform_marginal_hyper", "\n")
  # str(margs)
  # cat("\n")
  # cat("\n", "inside transform_marginal_hyper - rename", "\n")
  # print(rename_inla(names(margs), choice = "Precision"))
  # cat("\n")
  names(margs) <- rename_inla(names(margs), choice = "Precision")
  margs
}
