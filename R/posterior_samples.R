#' Sample the posterior using \code{inla.posterior.sample}
#'
#' Sample the posterior using \code{inla.posterior.sample}.
#'
#' For each sample given by \code{inla.posterior.sample} we extract the value
#' of the intercept, parameters, precision and hyperparameters. When the
#' parameters is found in the hyperparameters instead of the \emph{latent} section
#' the hyperparameters is chosen.
#'
#' @param .result \code{inla} object.
#' @param n Sample size.
#' @param type Samples type. One of c("post", "fit", "pred") with the following
#' meaning:
#' \describe{
#'  \item{post}{Get posterior samples, i.e. for variables.}
#'  \item{fit}{Get posterior samples with predisctors.}
#'  \item{pred}{Get posterior samples with predisctors and variability.}
#' }
#' @param out_df TRUE: return a data.frame, FALSE: return a list.
#' @param var Name of extra column with name of the variable in the data.frame.
#' @param ren Rename the variable with \code{rename_brms}
#'
#' @return List / data.frame of samples
#' @export
#'
#' @seealso INLA::inla.posterior.sample
posterior_samples <- function(.result, n = 1L, type = c("post", "fit", "pred"),
                              out_df = TRUE, var = "var", ren = TRUE) {
  checkmate::assert_class(.result, classes = "inla")
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_logical(out_df)
  checkmate::assert_string(var, min.chars = 1)
  checkmate::assert_logical(ren)

  type <- match.arg(arg = type)

  # default selection, when type is fit or pred
  sel <- list("Predictor" = 0L)
  if(type == "post") sel <- posterior_samples_sel(.result)


  # the tags used in the samples are the ones not in the hyperparameters
  # we create the selection list as a result
  # sel <- posterior_samples_sel(.result)

  samples <- INLA::inla.posterior.sample(n = n,
                                         result = .result,
                                         selection = sel)
  assertthat::not_empty(samples)

  out <- posterior_samples_list(samples, type = type, sel)

  if(out_df) out <-  as.data.frame(do.call(rbind, out), optional = FALSE)

  out
}


#' Create list to use with \code{INLA::inla.posterior.sample(selection = sel)}
#'
#' Create list to use with \code{INLA::inla.posterior.sample(selection = sel)}.
#'
#' The parameters could be in the \emph{hyperpar} section of the sample
#' as well as in the \emph{latent} section.  This happens when then the
#' parameter is in the f() section of the inla formula. When this is the case,
#' this function remove form the selection the variables in \emph{hyperpar}.
#'
#' See the documentation \code{INLA::inla.posterior.sample} for more details
#' on the parameters \code{selection}.
#'
#' @inheritParams posterior_samples
#' @param excl vector of tags to exclude from the selection list. Represents
#' the predictors. Default is \code{c("APredictor", "Predictor")}.
#'
#' @return List of selected tags and indices
#' @export
#'
#' @seealso INLA::inla.posterior.sample
posterior_samples_sel <- function(.result, excl = c("APredictor", "Predictor")) {
  checkmate::assert_class(.result, classes = "inla")
  checkmate::assert_character(excl, any.missing = FALSE, min.len = 2)

  # the internal tags associated with the hyperparameters
  hypers <- .result$misc$theta.tags

  # identify tags associated with the selection of the posterior samples
  latents <- .result$misc$configs$contents$tag

  # flag the tag which are also in the hyperparameters
  tags <- sapply(X = latents, FUN = function(x) {
    y <- grepl(pattern = x, x = hypers)
    y <- any(y)
  })
  assertthat::assert_that(length(tags) == length(latents))
  names(tags) <- latents

  # identify tags related to exclusions (predictors)
  tags[names(tags) %in% excl] <- NA

  # At least one tags must not be an hyperparameters (e.g. (Intercept))
  if(all(tags, na.rm = TRUE)) {
    msg <- "At least one tags must not be an hyperparameters, e.g. (Intercept)."
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = sprintf("Latent tags: %s", tags[!is.na(tags)]))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "posterior_samples_sel_error1")
    }

  # all latent tags not in hyperparameters must have a count of one
  cnt <- .result$misc$configs$contents$length
  cnt <- cnt[!is.na(tags) & !tags]
  if(any(cnt != 1L)) {
    msg <- "All latent tags not in hyperparameters must have a count of one"
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = sprintf("Latent tags count: %s", cnt))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "posterior_samples_sel_error2")
  }

  # remove excl (predictors) and tags in hyperparameters
  out <- tags[!is.na(tags) & !tags]
  assertthat::not_empty(out)
  out[] <- 0L  # 0 means all items for every tag is taken

  as.list(out)
}

#' Create list from samples obtained with \code{INLA::inla.posterior.sample}
#'
#' Create list from samples obtained with \code{INLA::inla.posterior.sample}.
#'
#' Create list where the latent and hyper parameters are joined together
#' in one vector per sample.  Then the n vectors from the n samples are
#' put together in a list with n items.
#'
#' @inheritParams posterior_samples
#' @param samples Samples from \code{INLA::inla.posterior.sample}.
#' @param sel List of selections from \code{posterior_samples_sel}
#'
#' @return List of vector. Length of the list is the number of samples.
#' @export
#'
#' @seealso INLA::inla.posterior.sample eflINLA::posterior_samples_sel
posterior_samples_list <- function(samples, type = c("post", "fit", "pred"),
                                   sel) {
  checkmate::assert_list(samples, min.len = 1)
  checkmate::assert_list(sel, min.len = 1, names = "named")

  type <- match.arg(arg = type)

  out <- list()
  if(type == "post") {

    # tags of variables to extract
    tags <- names(sel)

    out <- lapply(X = samples, FUN = function(x) {
      latent <- as.vector(x$latent)
      msg <- "Nb of rows in latent matrix is invalid.  Verify if duplicate tags."
      assertthat::assert_that(length(latent) == length(tags), msg = msg)
      names(latent) <- tags

      # get all hyperparameters and convert precision to sd
      hyper <- x$hyperpar
      hyper[1] <- prec2sd(hyper[1])
      names(hyper)[1] <- rename_inla(names(hyper)[1], choice = "Precision")

      c(latent, hyper)
    })

  } else if(type == "fit") {

    out <- lapply(X = samples, FUN = function(x) {
      latent <- as.vector(x$latent)
      names(latent) <- rownames(x$latent)
      latent
    })

  } else if(type == "pred") {

    out <- lapply(X = samples, FUN = function(x) {
      latent <- as.vector(x$latent)
      names(latent) <- rownames(x$latent)

      # get precision and convert it to sd
      hyper <- x$hyperpar[1]
      hyper <- prec2sd(hyper)
      names(hyper) <- rename_inla(names(hyper), choice = "Precision")

      # add variability to predictors
      latent <- rnorm(n = length(latent), mean = latent, sd = hyper)

      c(latent, hyper)
    })

  } else {
    msg_head <- cli::col_yellow("Type must be in one of the choices.")
    msg_body <- c("i" = sprintf("Invalid type: %s", type))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "posterior_samples_list_error")
  }

  assertthat::not_empty(out)

  out
}
