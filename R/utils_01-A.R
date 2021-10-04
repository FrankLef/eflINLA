#' Convert precision to standard deviation using marginals
#'
#' Convert precision to standard deviation using marginals.
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
prec2sd_marg <- function(marg, is_log = FALSE) {
  INLA::inla.tmarginal(fun = function(x) prec2sd(x, is_log = is_log),
                       marginal = marg, n = nrow(marg))
}


#' Convert precision to standard deviation
#'
#' Convert precision to standard deviation.
#'
#' Convert precision to standard deviation and inverse trasnform the log when
#' required.
#'
#' @param x Numeric
#' @param is_log FALSE (default): convert using natural scale; TRUE: convert using
#' log scale.
#'
#' @return precision converted to standard deviation
#' @export
prec2sd <- function(x, is_log=FALSE) {
  checkmate::assert_numeric(x, finite = TRUE)

  # must be positive when is_log = FALSE
  if (!is_log & any(x <= 0)) {
    msg_head <- cli::col_yellow("Value must be positive.")
    msg_body <- c("x" = sprintf("%d non-positive values", sum(x <= 0)))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "prec2sd_error")
  }

  # transform
  if (is_log) {
    1 / sqrt(exp(x))
  } else {
    1 / sqrt(x)
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
