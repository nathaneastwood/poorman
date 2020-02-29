#' Arrange
#'
#' @param .data A `data.frame`.
#' @param ... A `vector()` of name(s) to order the data by.
#'
#' @examples
#' arrange(mtcars, mpg)
#' mtcars %>% arrange(mpg)
#' mtcars %>% arrange(cyl, mpg)
#'
#' @export
arrange <- function(.data, ...) {
  UseMethod("arrange")
}

#' @export
arrange.default <- function(.data, ...) {
  rows <- eval.parent(substitute(with(.data, order(...))))
  extract(.data, rows, , drop = FALSE)
}

#' @export
arrange.grouped_data <- function(.data, ...) {
  apply_grouped_function(.data, "arrange", ...)
}
