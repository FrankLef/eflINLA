
#' Extract all marginals from \code{inla}
#'
#' Extract all marginals from \code{inla}.
#'
#' Extract the hyper and fixed marginals from an \code{inla} object and
#' put them in a list.
#'
#' @param .result \code{inla} object.
#' @param ren Rename the variable with \code{rename_brms}.
#'
#' @return List of marginals
#' @export
extract_marginals <- function(.result, ren = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_flag(ren)

  margs <- list()

  # get the transformed marginals for hyperparameters
  margs$hyper <- extract_marginals_hyper(.result)
  assertthat::not_empty(margs$hyper)

  # the list of all marginals (fixed and hyper)
  margs$all <- c(.result$marginals.fixed, margs$hyper)
  assertthat::assert_that(length(margs$all) > length(margs$hyper))

  # rename variables using brms naming convention
  if(ren) names(margs$all) <- rename_inla2brms(names(margs$all))

  margs$all
}


#' Extract the hyperparameters' marginals
#'
#' Extract the hyperparameters' marginals.
#'
#' Convert the precision used in the hyperparameers' marginals to standard
#' deviations and extract other hyperparametrs' marginals as is.
#'
#' @param .result \code{inla} object.
#'
#' @return List of hyperparameter marginals.
#' @export
extract_marginals_hyper <- function(.result) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)

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

  # edit the names to replace "precision" by "SD"
  names(margs) <- rename_inla(names(margs), choice = "Precision")

  # put precision/SD at the end to enable comparisons with other packages
  if(length(margs) > 1) margs <- append(margs[2:length(margs)], margs[1])

  margs
}
