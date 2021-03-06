#' Get predictors from samples obtained with \code{INLA::inla.posterior.sample}
#'
#' Get predictors from samples obtained with \code{INLA::inla.posterior.sample}.
#'
#' Extract posterior predictors (as opposed to the ones from marginals) from
#' an inla object, add variability to them using the standard deviation shown
#' in each sample \code{INLA::inla.posterior.sample} of size \code{n} and
#' output these randomized predictors as \code{posterior::draws_vars} objects.
#'
#' When creating the input inla object, use \code{eflINLA::augment_inla} to
#' ensure the data is formatted the right way and inla arguments are properly
#' set.
#'
#' @param .result \code{inla} object.
#' @param new_pos Integer of positions of \code{newdata}.
#' @param n Sample size.
#' @param sel List used for selection by
#' \code{INLA::inla.posterior.sample(selection = sel)}.
#'
#' @seealso INLA::inla.posterior.sample eflINLA::augment_inla
#'
#' @return \code{draws_rvars} object.
#' @export
predicted_draws_inla <- function(.result, new_pos = integer(), n = 1L, sel = NULL) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_integer(new_pos)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_list(sel, null.ok = TRUE)

  # get the default selection
  # the selection choices are list("APredictor" = 0L, "Predictor" = 0L)
  if (is.null(sel)) {
    if (length(new_pos)) {
      sel <- list("Predictor" = new_pos)
      # alternative choice
      # sel <- list("APredictor" = new_pos, "Predictor" = new_pos)
    } else {
      sel <- list("Predictor" = 0L)
      # alternative choice
      # sel <- list("APredictor" = 0L, "Predictor" = 0L)
    }
  }

  samples <- INLA::inla.posterior.sample(n = n,  result = .result,
                                         selection = sel)
  assertthat::not_empty(samples)

  out <- lapply(X = samples, FUN = function(x) {
    latent <- as.vector(x$latent)

    # get precision and convert it to sd
    hyper <- x$hyperpar[1]
    hyper <- prec2sd(hyper)
    names(hyper) <- rename_inla(names(hyper), choice = "Precision")

    # add variability to predictors
    latent <- stats::rnorm(n = length(latent), mean = latent, sd = hyper[[1]])
    names(latent) <- rownames(x$latent)

    # c(latent, hyper)  # do not keep hyper, used for debugging
    latent
  })
  assertthat::not_empty(out)
  out <- do.call(rbind, out)

  posterior::as_draws_rvars(out)
}
