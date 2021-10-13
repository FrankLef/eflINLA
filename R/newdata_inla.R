#' Run \code{inla} with new data to obtain new predictors
#'
#' Run \code{inla} with new data to obtain new predictors.
#'
#' Run \code{inla} with new data to allow the use of \code{summary.linear.predictor},
#' \code{summary.fitted.values}, \code{marginlas.linear.predictor}, etc.
#'
#' @param .result \code{inla} object.
#' @param newdata Dataframe of new data.  The column with the response
#' (outcome, dependent variable) must be filled with \code{NA}s.
#'
#' @return List with the following items
#' \describe{
#'  {inla}{updated \code{inla} object.}
#'  {newdata_pos}{positions of the newdata.}
#' }
#' @export
augment_inla <- function(.result, newdata) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_names(names(.result$.args), must.include = "data")
  checkmate::assert_data_frame(newdata)
  checkmate::assert_names(names(newdata), subset.of = names(.result$.args$data))

  # the row position of the new data
  the_inla <- .result
  new_pos <- integer()

  # get the new data and vector of positions
  lst <- create_newdata_inla(.result, newdata)
  data <- lst$data
  new_pos <- lst$new_pos

  if(length(new_pos)) {
    # update the list of parameters to be able to use with inla
    the_args <- .result$.args
    the_args$control.predictor = list(compute = TRUE)
    the_args$offset <- NULL  # set to NULL, otherwise error
    the_args$data <- data

    the_inla <- do.call(INLA::inla, the_args)
  }


  list("inla" = the_inla, "new_pos" = new_pos )
}

#' Create a new data set using \code{inla$.args$data} and \code{newdata}
#'
#' Create a new data set using \code{inla$.args$data} and \code{newdata}.
#'
#' Run \code{inla} with new data to allow the use of \code{summary.linear.predictor},
#' \code{summary.fitted.values}, \code{marginlas.linear.predictor}, etc.
#'
#' @param .result \code{inla} object.
#' @param newdata Dataframe of new data.  The column with the response
#' (outcome, dependent variable) must be filled with \code{NA}s.
#'
#' @return List with the following items
#' \describe{
#'  {data}{augmented \code{inla$.args$data} with \code{newdata}.}
#'  {newdata_pos}{positions of the newdata.}
#' }
#' @export
create_newdata_inla <- function(.result, newdata) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_names(names(.result$.args), must.include = "data")
  checkmate::assert_data_frame(newdata)
  checkmate::assert_names(names(newdata), subset.of = names(.result$.args$data))


  data <- .result$.args$data
  new_pos <- integer()

  if(nrow(newdata)) {

    # validate the data type
    data_types <- sapply(X = data, FUN = typeof)
    newdata_types <- sapply(X = newdata, FUN = typeof)
    if (all(newdata_types %in% data_types)) {

      # get the positions of the new data in the output
      new_pos <- (nrow(data) + 1L):(nrow(data) + nrow(newdata))
      assertthat::not_empty(new_pos)
      assertthat::assert_that(all(new_pos >= 2L))

      # create the newdata which
      #  - must have the same columns and types as the original data
      #  - any column not in newdata is set to NA
      new_cols <- names(data)[!(names(data) %in% names(newdata))]
      extra_newdata <- data[FALSE, new_cols]
      extra_newdata <- as.data.frame(
        lapply(X = extra_newdata, function(x) {
          type <- typeof(x)
          na_val <- switch(type,
                           "double" = NA_real_,
                           "integer" = NA_integer_,
                           "character" = NA_character_,
                           "complex" = NA_complex_,
                           NA)
          rep(na_val, length.out = nrow(newdata))
        }
        ))

      assertthat::assert_that(nrow(extra_newdata) == nrow(newdata))
      extra_newdata <- cbind(extra_newdata, newdata)

      assertthat::assert_that(all(names(extra_newdata) %in% names(data)))
      extra_newdata <- extra_newdata[names(data)]  # reorder to original data

      # bind original and newdata together to feed inla
      data <- as.data.frame(rbind(data, extra_newdata))
      # cat("\n")
      # str(data)
      # cat("\n")

      } else {
        msg <- "`newdata` must have the same types as the original data."
        msg_head <- cli::col_yellow(msg)
        msg_body <- c("x" = sprintf("newdata: %s", toString(newdata_types)))
        msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
        rlang::abort(
          message = msg,
          class = "get_newdata_inla_error1")
      }
    }

  list("data" = data, "new_pos" = new_pos )
}
