#' Get the posterior summary from inla
#'
#' Get the posterior summary from inla.
#'
#' Get the posterior summary from inla with precision converted to sd and
#' quantiles requested by the user.
#'
#' @param r inla object
#'
#' @export
#'
#' @return dataframe with inla posterior summary
posterior_summary <- function(r) {
  checkmate::assert_class(r, classes = "inla")

  # must use the quantiles of the given inla object
  qtl <- r$.args$quantiles

  summ_fixed <- r$summary.fixed[, !(names(r$summary.fixed) %in% c("kld"))]
  assertthat::not_empty(summ_fixed)

  summ_hyper <- transform_hyper_marginal(r)
  summ_hyper <- write_hyper_summary(summ_hyper, qtl = qtl)
  assertthat::not_empty(summ_hyper)
1
  # cat("\n")
  # print(summ_fixed)
  # cat("\n")
  # print(summ_hyper)
  # cat("\n")

  # the headings must be the same
  assertthat::assert_that(identical(names(summ_hyper), names(summ_fixed)))
  summ <- rbind(summ_fixed, summ_hyper)
  summ
}


#' Transform the hyperparameters' marginal
#'
#' Transform the hyperparameters' marginal.
#'
#' Convert the precision used in the hyperparameers' marginals to standard
#' deviations.
#'
#' @param r inla object.
#' @param sd_name String. Name of standard deviation. Don't change it unless
#' you have a good reason.
#'
#' @export
#'
#' @return List of marginals
transform_hyper_marginal <- function(r, sd_name = "sd") {
  checkmate::assert_class(r, classes = "inla")
  checkmate::assert_names(names(r),
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
        m <- INLA::inla.tmarginal(fun = function(x) 1 / sqrt(exp(x)),
                                  marginal = imarg)
      } else {
        # use marginals.hyperpar for other hyperparameters
        m <- marg
      }
      return(m)
    },
    r$marginals.hyperpar,
    names(r$marginals.hyperpar),
    r$internal.marginals.hyperpar,
    SIMPLIFY = FALSE)

  # edit the names to replace "precision" by sd_name
  names(margs) <- sub(pattern = prec_rgx, replacement = sd_name,
                         x = names(margs), ignore.case = TRUE)

  # the sd row must be the last one (useful for summaries)
  rgx <- paste0("^", sd_name, ".+")
  pos <- grepl(pattern=rgx, x=names(margs))
  out <- append(margs[!pos], margs[pos])

  out
}


#' Write summary for inla hyperparameters
#'
#' Write summary for inla hyperparameters.
#'
#' Write a summary with the mean, sd and quantiles of the hyperparameters
#' from inla. It must use the transformed marginals from `transform_hyper_marginal`
#' because it requires the standard deviation rather than the precision.  The
#' format copycat the format using by the inla summaries.
#'
#' @param margs Hyperparameters' marginals created with `transform_hyper_marginal`
#' @param qtl Numeric(). The quantiles appearing in the summary.
#'
#' @return Dtaframe with hyperparameters' posterior summary
#'
#' @export
#'
#' @seealso transform_hyper_marginal
write_hyper_summary <- function(margs, qtl = c(0.025, 0.5, 0.975)) {
  checkmate::assert_list(margs, min.len = 1)
  checkmate::assert_numeric(qtl, min.len = 1, unique = TRUE, finite = TRUE,
                            lower = 0, upper = 1)

  # suffix used by INLA, usually not recommended to start the
  # name of a variable with a number as INLA does.
  # Done here just to be consistent with INLA.
  qtl_suffix <- "quant"

  marg_lst <- mapply(
    FUN = function(mrg, nm) {
      the_mean <- INLA::inla.emarginal(fun = function(x) x, marginal = mrg)
      the_sd <- sqrt(max(0,
                         INLA::inla.emarginal(fun = function(x) x^2 - the_mean^2,
                                              marginal = mrg)
      )
      )
      the_qtl <- t(INLA::inla.qmarginal(qtl, marginal = mrg))
      colnames(the_qtl) <- paste0(qtl, qtl_suffix)
      the_mode <- INLA::inla.mmarginal(mrg)
      data.frame(mean = the_mean,
                 sd = the_sd,
                 the_qtl,
                 mode = the_mode,
                 row.names = nm,
                 check.names = FALSE)
    },
    margs,
    names(margs),
    SIMPLIFY = FALSE
  )
  do.call(rbind, marg_lst)
}
