#' Get a sample of posterior draws from a, \code{inla} fit as a tibble
#'
#' Get a sample of posterior draws from a, \code{inla} fit as a tibble. Using
#' the data from \code{INLA::inla.posterior.sample}.
#'
#' Create list where the latent and hyper parameters from
#' \code{INLA::inla.posterior.sample} are joined together in one vector per
#' sample.  Then each vector from the \code{n} samples are
#' put together in a \code{draws_rvars} object.
#'
#' @param .result \code{inla} object.
#' @param n Sample size.
#' @param ren TRUE:rename variables to \code{brms} naming convention;
#' FALSE: Keep \code{INLA} naming convention.
#'
#' @seealso INLA::inla.posterior.sample eflINLA::posterior_samples_sel
#'
#' @return \code{draws_rvars} object.
#' @export
tidy_draws_inla <- function(.result, n = 1L, ren = TRUE) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_count(n, positive = TRUE)
  checkmate::assert_flag(ren)

  # create the selection of variables
  sel <- posterior_samples_sel(.result)

  # get the posterior samples
  samples <- INLA::inla.posterior.sample(n = n,  result = .result,
                                         selection = sel)
  assertthat::not_empty(samples)

  # tags of variables to extract
  tags <- names(sel)

  out <- lapply(X = samples, FUN = function(x) {
    latent <- as.vector(x$latent)
    msg <- "Nb of rows in latent matrix is invalid.  Verify if duplicate tags."
    assertthat::assert_that(length(latent) == length(tags), msg = msg)
    names(latent) <- tags

    # get all hyperparameters and convert precision to sd
    hyper <- x$hyperpar
    # transform precision to standard deviation
    hyper[1] <- prec2sd(hyper[1])
    names(hyper)[1] <- rename_inla(names(hyper)[1], choice = "Precision")

    # put sigma which is always the first element to last element
    # to match with brms standards
    if (length(hyper) > 1) {
      nm <- names(hyper)
      pos <- 2:length(hyper)
      hyper <- c(hyper[pos], hyper[1])
      names(hyper) <- c(names(hyper)[pos], names(hyper)[1])
    }

    c(latent, hyper)
  })
  assertthat::not_empty(out)
  out <- do.call(rbind, out)

  # rename variables using brms naming convention
  if(ren) colnames(out) <- rename_inla2brms(colnames(out))

  posterior::as_draws_rvars(out)
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
#' @param .result \code{inla} object.
#' @param excl vector of tags to exclude from the selection list. Represents
#' the predictors. Default is \code{c("APredictor", "Predictor")}.
#'
#' @seealso INLA::inla.posterior.sample
#'
#' @return List of selected tags and indices
#' @export
posterior_samples_sel <- function(.result, excl = c("APredictor", "Predictor")) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
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
