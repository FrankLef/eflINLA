#' Convert precision to standard deviation
#'
#' Convert precision to standard deviation.
#'
#' Convert precision to standard deviation with data from the marginal of
#' INLA.
#'
#' @param marg marginal from \code{inla}
#' @param is_log FALSE (default): convert using natural scale; TRUE: convert using
#' log scale.
#'
#' @source section 2.6, p. 31, Bayesian Inference with INLA, Gomez-Rubio
#'
#' @return precision converted to standard deviation using marginal
#' @export
prec2sd <- function(marg, is_log=FALSE) {
  if (is_log) {
    INLA::inla.tmarginal(fun = function(x) 1 / sqrt(exp(x)),
                                marginal = marg, n = nrow(marg))
  } else {
    INLA::inla.tmarginal(fun = function(x) 1 / sqrt(x),
                                marginal = marg, n = nrow(marg))
  }
}


#' Rename strings (row names) from \code{inla} to \code{brms} equivalent
#'
#' Rename strings (row names) from \code{inla} to \code{brms} equivalent.
#'
#' @param x Character with \code{inla} strings
#'
#' @return Character with inla strings renamed
#' @export
rename_inla2brms <- function(x) {
  checkmate::assert_character(x, min.chars = 1)

  # the regex patterns
  p <- c("Precision for the Gaussian.+", "Beta for ")
  # the replacements
  r <- c("Sigma", "b_")
  assertthat::assert_that(length(p) == length(r))

  out <- x  # we use x recursively
  for (i in seq_along(p)) {
    # sub recursively
    out <- sub(pattern = p[i], replacement = r[i], x = out)
  }
  out
}
