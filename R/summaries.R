#' Write a summary of posterior distribution using marginals
#'
#' Write a summary of posterior distribution using marginals.
#'
#' Write a summary of posterior distributions with a format similar to
#' \code{brms::posterior_summary}.
#'
#' @param .result \code{inla} object.
#' @param probs Percentiles returned by the summary.
#' @param ren Rename the variable with \code{rename_brms}.
#'
#' @return Dataframe with summaries.
#' @export
posterior_marg_summmary_inla <- function(.result, probs = c(0.025, 0.975),
                                               ren = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, unique = TRUE)
  checkmate::assert_flag(ren)

  # get all marginals
  margs <- extract_marginals(.result, ren = ren)

  # create the summary line for each marginal
  summs <- lapply(X = margs, FUN = function(marg) {
    posterior_marg_summary_inla_one(marg, probs = probs)
  })

  out <- as.data.frame(do.call(rbind, summs))
  out <- tibble::rownames_to_column(out, var = "var")
  # names(out) <- names(margs)
  out
}


#' Create one line summary for a given marginal
#'
#' Create one line summary for a given marginal.
#'
#' Create one line summary using the marginal funcitons from \code{INLA} such
#' as \code{inla.emarginal, inla.qmarginal, inla.mmarginal}.
#'
#' @param marg Marginal from an \code{inla} object.
#' @param probs Percentiles returned by the summary.
#'
#' @return vector of summary data.
#' @export
posterior_marg_summary_inla_one <- function(marg, probs = c(0.025, 0.975)) {
  mom <- calc_moments_inla(marg)
  nms <- paste0("Q", probs)
  quant = as.vector(INLA::inla.qmarginal(marginal = marg, p = probs))
  names(quant) <- nms
  md = as.vector(INLA::inla.mmarginal(marg))
  c(mom, quant, "mode" = md)
}


#' Compute the moments of a marginal
#'
#' Compute the moments of a marginal.
#'
#' Compute the moments of a marginal from \code{inla} using its densities .
#'
#' @param marg Marginal from an \code{inla} object.
#'
#' @return Vector with mean and standard deviation of the marginal.
#' @export
calc_moments_inla <- function(marg) {
  vals <- INLA::inla.emarginal(marginal = marg, fun = function(val) c(val, val^2))
  c("mean" = vals[1], "sd" = sqrt(vals[2] - vals[1]^2))
}
