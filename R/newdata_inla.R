#' Run \code{inla} with new data to obtain new pedictors
#'
#' Run \code{inla} with new data to obtain new pedictors.
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
get_newdata_inla <- function(.result, newdata) {
  checkmate::assert_class(.result, classes = "inla", ordered = TRUE)
  checkmate::assert_names(names(.result$.args), must.include = "data")
  checkmate::assert_data_frame(newdata)
  checkmate::assert_names(names(newdata), subset.of = names(.result$.args$data))

  # the original data
  data <- .result$.args$data

  # the row position of the new data
  new_pos <- integer()
  if(nrow(newdata)) {

    # validate the data type
    data_cols <- sapply(X = data, FUN = typeof)
    newdata_cols <- sapply(X = newdata, FUN = typeof)
    if (all(newdata_cols %in% data_cols)) {

      # get the position of the new data in the output
      new_pos <- (nrow(data) + 1L):(nrow(data) + nrow(newdata))
      assertthat::not_empty(new_pos)

      # create the newdata which
      #  - must have the same columns and types as the original data
      #  - any column not in newdata is set to NA
      new_cols <- names(data)[!(names(data) %in% names(newdata))]
      the_newdata <- data[FALSE, new_cols]
      the_newdata <- as.data.frame(
        lapply(X = the_newdata, function(x) rep(NA, length.out = nrow(newdata)))
      )
      assertthat::assert_that(nrow(the_newdata) == nrow(newdata))
      the_newdata <- cbind(the_newdata, newdata)
      assertthat::assert_that(all(names(the_newdata) %in% names(data)))
      the_newdata <- the_newdata[names(data)]

      # bind original and newdata together to feed inla
      the_inla_data <- as.data.frame(rbind(data, the_newdata))
      # cat("\n")
      # str(the_inla_data)
      # cat("\n")

      # update the list of parameters to be able to use with inla
      the_args <- .result$.args
      the_args$control.predictor = list(compute = TRUE)
      the_args$offset <- NULL  # set to NULL, otherwise error
      the_args$data <- the_inla_data

      out <- do.call(INLA::inla, the_args)

    } else {
      msg <- "`newdata` must have the same types as the original data."
      msg_head <- cli::col_yellow(msg)
      msg_body <- c("x" = sprintf("newdata: %s", toString(newdata_cols)))
      msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
      rlang::abort(
        message = msg,
        class = "get_newdata_inla_error1")
    }

  } else {
    msg <- "`newdata` was empty.  The original inla object is returned."
    msg_head <- cli::col_yellow(msg)
    msg_body <- c("i" = "Why is newdata empty?")
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::inform(
      message = msg,
      class = "get_newdata_inla_message1")

    # if no newdata provided, just return the original data
    out <- .result
  }

  list("inla" = out, "newdata_pos" = new_pos )
}
