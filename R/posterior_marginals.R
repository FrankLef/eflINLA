#' Get all marginals from \code{inla}
#'
#' Get all marginals from \code{inla}.
#'
#' Extract the hyper and fixed marginals from an \code{inla} object and
#' put them in a data.frame when \code{is.df = TRUE} (default), otherwise
#' in a list when \code{is.df = FALSE}.
#'
#' @param .result \code{inla} object.
#' @param is.df TRUE: return a data.frame, FALSE: return a list.
#' @param var Name of extra column with name of the variable in the data.frame.
#'
#' @return List / data.frame of marginals
#' @export
posterior_marginals <- function(.result, is.df = TRUE, var = "var") {
  checkmate::assert_class(.result, classes = "inla")
  checkmate::assert_logical(is.df)
  checkmate::assert_string(var, min.chars = 1)

  margs <- list()

  # get the transformed marginals for hyperparameters
  margs$hyper <- transform_marginal_hyper(.result)
  assertthat::not_empty(margs$hyper)

  # the list of all marginals (fixed and hyper)
  margs$all <- c(.result$marginals.fixed, margs$hyper)
  assertthat::assert_that(length(margs$all) > length(margs$hyper))

  # output in dataframe format if required
  if(is.df) {
    out <- purrr::map_df(margs$all, ~as.data.frame(.x), .id=var)
  } else {
    out <- margs$all
  }

  out
}


#' Transform the hyperparameters' marginal
#'
#' Transform the hyperparameters' marginal.
#'
#' Convert the precision used in the hyperparameers' marginals to standard
#' deviations.
#'
#' @param .result inla object.
#' @param sd_name Name of standard deviation. Don't change it unless
#' you have a good reason.
#'
#' @export
#'
#' @return List of hyperparameter marginals transformed to user scale
transform_marginal_hyper <- function(.result, sd_name = "sd") {
  checkmate::assert_class(.result, classes = "inla")
  checkmate::assert_names(names(.result),
                          must.include = c("marginals.hyperpar",
                                           "internal.marginals.hyperpar"))
  checkmate::assert_string(sd_name, min.chars = 1)

  # don't modify this without a good reason and test it
  # the regex pattern used to find the "precision"
  prec_rgx = "^precision"

  margs <- mapply(
    FUN = function(marg, nm, imarg) {
      if(grepl(pattern = prec_rgx, x = nm, ignore.case = TRUE)) {
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
  names(margs) <- sub(pattern = prec_rgx, replacement = sd_name,
                      x = names(margs), ignore.case = TRUE)

  # the sd row must be the last one (useful for summaries)
  rgx <- paste0("^", sd_name, ".+")
  pos <- grepl(pattern=rgx, x=names(margs))
  append(margs[!pos], margs[pos])
}
