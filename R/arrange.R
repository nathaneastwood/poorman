#' Arrange rows by variables
#'
#' Order rows of a `data.frame` by an expression involving its variables.
#'
#' @param .data A `data.frame`.
#' @param ... A comma separated vector of unquoted name(s) to order the data by.
#'
#' @examples
#' arrange(mtcars, mpg)
#' mtcars %>% arrange(mpg)
#' mtcars %>% arrange(cyl, mpg)
#'
#' @return
#' A `data.frame`.
#'
#' @export
arrange <- function(.data, ...) {
  check_is_dataframe(.data)
  UseMethod("arrange")
}

#' @export
arrange.default <- function(.data, ...) {
  rows <- eval.parent(substitute(with(.data, order(...))))
  .data[rows, , drop = FALSE]
}

#' @export
arrange.grouped_data <- function(.data, ...) {
  apply_grouped_function("arrange", .data, ...)
}
