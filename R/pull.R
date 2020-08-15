#' Pull out a single variable
#'
#' This is a direct replacement for `[[.data.frame`.
#'
#' @param .data A `data.frame`.
#' @param var A variable specified as:
#' * a literal variable name
#' * a positive integer, giving the position counting from the left
#' * a negative integer, giving the position counting from the right
#'
#' The default returns the last column (on the assumption that's the column you've created most recently).
#'
#' @examples
#' mtcars %>% pull(-1)
#' mtcars %>% pull(1)
#' mtcars %>% pull(cyl)
#' mtcars %>% pull("cyl")
#'
#' @export
pull <- function(.data, var = -1) {
  var_list <- as.list(seq_along(.data))
  names(var_list) <- names(.data)
  .var <- eval(substitute(var), var_list)
  if (.var < 0L) .var <- length(var_list) + .var + 1L
  .data[[.var]]
}
