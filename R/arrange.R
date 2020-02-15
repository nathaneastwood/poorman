#' Arrange
#'
#' @param .data A `data.frame`.
#' @param ... A `vector()` of name(s) to order the data by.
#'
#' @examples
#' mtcars %>% arrange(mpg)
#' mtcars %>% arrange(cyl, mpg)
arrange <- function(.data, ...) {
  rows <- eval.parent(substitute(with(.data, order(...))))
  extract(.data, rows, , drop = FALSE)
}
