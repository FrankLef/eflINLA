#' Get predictors from samples obtained with \code{INLA::inla.posterior.sample}
#'
#' Get predictors from samples obtained with \code{INLA::inla.posterior.sample}.
#'
#' Create list where the predictors are joined together
#' in one vector per sample.  Then each vector from the \code{n} samples are
#' put together in a \code{draws_rvars} object.
#'
#' When \code{pred = FALSE} this is the equivalent of \code{linpred_draws}.
#' When \code{pred = TRUE} this is the equivalent of \code{predicted_draws}.
#'
#' @param .result \code{inla} object.
#' @param n Sample size.
#' @param ren TRUE:rename variables to \code{brms} naming convention;
#' FALSE: Keep \code{INLA} naming convention.
#' @param sel List used for selection by
#' \code{INLA::inla.posterior.sample(selection = sel)}.
#' @param pred TRUE: Add variability to the predictors to make them predictions;
#' FALSE: Keep predictors as is.
#'
#' @seealso INLA::inla.posterior.sample
#'
#' @return \code{draws_rvars} object.
#' @export
linpred_draws_inla <- function(.result, n = 1L, ren = TRUE,
                               sel = list("APredictor" = 0L, "Predictor" = 0L),
                               pred = FALSE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_flag(ren)
  checkmate::assert_list(sel, min.len = 1)
  checkmate::assert_flag(pred)

  # get the posterior samples
  samples <- INLA::inla.posterior.sample(n = n,  result = .result,
                                         selection = sel)
  assertthat::not_empty(samples)

  out <- lapply(X = samples, FUN = function(x) {
    latent <- as.vector(x$latent)

    # get precision and convert it to sd
    hyper <- x$hyperpar[1]
    hyper <- prec2sd(hyper)
    names(hyper) <- rename_inla(names(hyper), choice = "Precision")

    # add variability to predictors when required
    if(pred) latent <- stats::rnorm(n = length(latent), mean = latent,
                                    sd = hyper)
    names(latent) <- rownames(x$latent)

    c(latent, hyper)
  })
  assertthat::not_empty(out)
  out <- do.call(rbind, out)

  # rename variables using brms naming convention
  if(ren) colnames(out) <- rename_inla2brms(colnames(out))

  posterior::as_draws_rvars(out)
}
