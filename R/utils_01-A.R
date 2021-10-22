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
  checkmate::assert_double(marg)
  checkmate::assert_flag(is_log)

  # WARNING: Don't use custom function inside INLA::inla.tmarginal.
  # Otherwise annoying warnings are issued and are very hard to track
  # to their source.

  if (is_log) {
    INLA::inla.tmarginal(fun = function(x) 1 / sqrt(exp(x)),
                         marginal = marg, n = nrow(marg))
  } else {
    INLA::inla.tmarginal(fun = function(x) 1 / sqrt(x),
                         marginal = marg, n = nrow(marg))
  }
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
prec2sd <- function(x, is_log = FALSE) {
  checkmate::assert_numeric(x, finite = TRUE)
  checkmate::assert_flag(is_log)

  # must be positive when is_log = FALSE
  # is_nonpos <- sapply(X = x, FUN = function(v) any(v <= 0))
  # is_nonpos <- any(is_nonpos)
  # message(is_nonpos)
  # if (!is_log & any(is_nonpos)) {
  #   msg_head <- cli::col_yellow("Value must be positive.")
  #   msg_body <- c("x" = sprintf("%d non-positive values.", sum(x <= 0)))
  #   msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
  #   rlang::abort(
  #     message = msg,
  #     class = "prec2sd_error")
  # }

  if (is_log) {
    1 / sqrt(exp(x))
  } else {
    1 / sqrt(x)
  }
}

#' Rename strings/variable from \code{inla} to \code{brms} equivalent
#'
#' Rename strings/variable from \code{inla} to \code{brms} equivalent.
#'
#' Rename variables and names in \code{inla} to be easier to use in plots
#' and summary by using the \code{brms} as closely as possible. For example
#' \emph{(Intercept)} becomes \emph{Intercept}, \emph{Beta for A} becomes
#' \emph{b_A} and \emph{"Precision for the Gaussian observations"} becomes
#' \emph{Precision}.
#'
#' @param x Character with \code{inla} strings.
#'
#' @return Character with inla strings renamed.
#' @export
#'
#' @examples
#' df <- data.frame(
#'  x = c(
#'   NA_character_,
#'   "Precision for the Gaussian observations",
#'   "SD for the Gaussian observations",
#'   "sd for the Gaussian observations",
#'   "unknown", "(Intercept)",
#'   "Beta for A", "Beta for B", "Beta for B"),
#'  y = c(NA_character_,
#'   "Precision", "sigma", "sigma",
#'   "unknown", "b_Intercept",
#'   "b_A","b_B", "b_B"))
#' z <- rename_inla2brms(df$x)
#' stopifnot(identical(z, df$y))
rename_inla2brms <- function(x) {
  checkmate::assert_character(x, min.chars = 1)

  df <- data.frame(
    inla = c("^Precision for the Gaussian.+", "^SD for the Gaussian.+",
             "[(]Intercept[)]","^Beta for "),
    brms = c("Precision", "sigma", "b_Intercept", "b_")
  )

  out <- x  # we use x recursively
  for (i in seq_len(nrow(df))) {
    # sub recursively
    out <- sub(pattern = df$inla[i], replacement = df$brms[i],
               x = out, ignore.case = TRUE)
  }
  out
}

#' Rename inla variables
#'
#' Rename inla variables.
#'
#' Rename inla variables such as \emph{Precision }to \emph{SD}.
#'
#' @param x Character
#' @param choice Integer, choice of renaming to perform.
#'
#' @return Modified character
#' @export
#'
#' @examples
#' x <- "Precision for the Gaussian observations"
#' y <- rename_inla(x, choice = "Precision")
#' stopifnot(identical(y, "SD for the Gaussian observations"))
rename_inla <- function(x, choice = c(NA_character_, "Precision")) {

  choice <- match.arg(choice)

  out <- x
  if(isTRUE(is.na(choice))) {
    # do nothing when choice == 0
  } else if(choice == "Precision") {
    out <- sub(pattern = "Precision", replacement = "SD", x = out,
               ignore.case = TRUE)
  } else {
    msg_head <- cli::col_yellow("choice must be in one of the choices.")
    msg_body <- c("i" = sprintf("Invalid choice: %s", choice))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "rename_inla_error")
  }

  out
}
