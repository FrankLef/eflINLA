# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


#' Example function that shows message that begins with "Hello" to console.
#'
#' @param msg String with a message that begins with "Hello". Default message
#'  is "Hello, world!".
#'
#' @return A message to console that starts with "Hello"
hello <- function(msg = "Hello, world!") {
  if (grepl(pattern = "^hello.*", x = msg, ignore.case = TRUE)) {
    message(msg)
  } else {
    msg_head <- cli::col_yellow("Message must start with \"hello\"")
    msg_body <- c("i" = sprintf("Message: %s", msg))
    msg <- paste(msg_head, rlang::format_error_bullets(msg_body), sep = "\n")
    rlang::abort(
      message = msg,
      class = "hello_error")
  }
  invisible(msg)
}
