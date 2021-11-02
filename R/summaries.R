#' Write a summary of posterior distribution using marginals
#'
#' Write a summary of posterior distribution using marginals.
#'
#' Write a summary of posterior distributions with a format similar to
#' \code{brms::posterior_summary}.
#'
#' @param .result \code{inla} object.
#' @param ren Rename the variable with \code{rename_brms}.
#'
#' @return Dataframe with summaries.
#' @export
#'
#' @examples
#' df <- data.frame(
#'  x1 = rnorm(50, mean = 1, sd = 0.5),
#'  x2 = rnorm(50, mean = 2, sd = 1),
#'  sigma = rexp(50, rate = 1))
#' df$mu <- df$x1 + df$x2
#' df$y <- rnorm(nrow(df), mean = df$mu, sd = df$sigma)
#' the_fit <- INLA::inla(formula = y ~ x1 + x2, data = df)
#' posterior_summary_inla(the_fit)
posterior_summary_inla <- function(.result, ren = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_flag(ren)

  # get all marginals
  margs <- extract_marginals(.result, ren = ren)

  # create the summary line for each marginal
  summs <- lapply(X = margs, FUN = function(marg) {
    posterior_summary_inla_one(marg, probs = .result$.args$quantiles)
  })

  out <- as.data.frame(do.call(rbind, summs))
  out <- tibble::rownames_to_column(out, var = "var")
  out
}


#' Create one line summary for a given marginal
#'
#' Create one line summary for a given marginal.
#'
#' Create one line summary using the marginal functions from \code{INLA} such
#' as \code{inla.emarginal, inla.qmarginal, inla.mmarginal}.
#'
#' @param marg Marginal from an \code{inla} object.
#' @param probs Percentiles returned by the summary. Default is
#' \code{c(0.025, 0.5, 0.975)} which is the \code{inla} default.
#'
#' @return vector of summary data.
#' @export
#'
#' @examples
#' df <- data.frame(
#'  x1 = rnorm(50, mean = 1, sd = 0.5),
#'  x2 = rnorm(50, mean = 2, sd = 1),
#'  sigma = rexp(50, rate = 1))
#' df$mu <- df$x1 + df$x2
#' df$y <- rnorm(nrow(df), mean = df$mu, sd = df$sigma)
#' the_fit <- INLA::inla(formula = y ~ x1 + x2, data = df)
#' posterior_summary_inla_one(the_fit$marginals.fixed[[1]])
posterior_summary_inla_one <- function(marg, probs = c(0.025, 0.5, 0.975)) {
  checkmate::assert_matrix(marg, ncols = 2)
  checkmate::assert_numeric(probs, lower = 0, upper = 1, finite = TRUE,
                            unique = TRUE)
  mom <- calc_moments_inla(marg)
  quant = as.vector(INLA::inla.qmarginal(marginal = marg, p = probs))
  names(quant) <- paste0("Q", probs)
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
#'
#' @examples
#' df <- data.frame(
#'  x1 = rnorm(50, mean = 1, sd = 0.5),
#'  x2 = rnorm(50, mean = 2, sd = 1),
#'  sigma = rexp(50, rate = 1))
#' df$mu <- df$x1 + df$x2
#' df$y <- rnorm(nrow(df), mean = df$mu, sd = df$sigma)
#' the_fit <- INLA::inla(formula = y ~ x1 + x2, data = df)
#' calc_moments_inla(the_fit$marginals.fixed[[1]])
calc_moments_inla <- function(marg) {
  checkmate::assert_matrix(marg, ncols = 2)
  vals <- INLA::inla.emarginal(marginal = marg, fun = function(val) c(val, val^2))
  c("mean" = vals[1], "sd" = sqrt(vals[2] - vals[1]^2))
}
