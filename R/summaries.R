#' Write a summary of posterior hyperparameters
#'
#' Write a summary of posterior hyperparameters.
#'
#' Write a summary of posterior hyperparameters with a format similar to
#' \code{brms::posterior_summary}.
#'
#' @param .result \code{inla} object.
#' @param probs Percentiles returned by the summary.
#' @param ren Rename the variable with \code{rename_brms}.
#'
#' @return Dataframe with summaries.
#' @export
#'
#' @examples
posterior_marg_hyper_summmary_inla <- function(.result, probs = c(0.025, 0.975),
                                               ren = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, unique = TRUE)
  checkmate::assert_flag(ren)

  margs <- extract_marginals_hyper(.result)

  summ <- calc_summary(margs, probs = probs)
}


get_summary <- function(marg, probs = c(0.025, 0.975)) {

}

calc_summary <- function(marg, dist = c("normal", "lognormal",
                                       "exponential", "gamma",
                                       "poisson")) {

  dist <- match.arg(dist)

  # print(marg)
  val <- inla.emarginal(function(val) c(val, val^2), marg)

  out <- switch(dist,
                 "normal" = {
                   m <- val[1]
                   s <- sqrt(val[2] - val[1]^2)
                   c("mean" = m, "sd" = s)},
                stopifnot("STOP")
                # val
                 )
  out
}
